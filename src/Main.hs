-- Wosa (WordSet Assembler).
-- Copyright (C) 2015 Moritz Schulte <mtesseract@silverratio.net>

module Main (main) where

--import Debug.Trace
import Control.Monad -- when
import qualified Data.Map.Lazy as MapL
import System.Environment           -- Commandline Arguments.
import Data.IORef                   -- We manage state with IORefs.
import Data.Maybe
import Control.Exception

import Wosa
-- This loads the 'backend implementation' named 'Nebelfiller'.
import Nebelfiller.Datatypes
import Nebelfiller
import Utilities

-- Constants:
--programName :: String
--programName = "Wosa"
--programVersion :: String
--programVersion = "0.1"

-- Data type utility functions.

------------------------------------------------------------

-- This is the datatype used for specifying the rules by which the
-- stateMachine call action handlers:
type StateMachineDescription = ( [State]         -- When in one of these states,
                               , WosaAction      -- allowe one of these actions to occur,
                               , ActionHandler   -- in which case this handler is called.
                               )

-- This is the type of an action handler. Action handlers receive a
-- Ctx context (wrapped in an IORef) as input and produce an
-- ActionResult...
type ActionHandler = IORef Ctx -> IO ActionResult

-- ... an ActionResult can be either Nothing (in case the action
-- handler triggered an error) or a pair consisting of a new Ctx and
-- maybe a WosaAction. If the latter is Just, then it specifies
-- another action to call in turn (Nothing means, no action should be
-- triggered automatically).
type ActionResult = Maybe (Ctx, Maybe WosaAction)

---------------------
-- Action Handlers --
---------------------

-- Implementation of the action ActionForm.
actionSuggestWordset :: IORef Ctx -> IO ActionResult
actionSuggestWordset ctx' = do
   -- It is guaranteed that we are in state StateManually.
   ctx <- readIORef ctx'
   let wsNo' = smallestEmptyWordset (ctxWordsets ctx)
   case wsNo' of
     Just wsNo -> do case callBackendSuggestWordset ctx of
                       Just ws -> do putStrLn $ "suggested wordset = " ++ (show ws)
                                     let newCtx = ctx { ctxState = StateAsk }
                                     writeIORef ctx' newCtx
                                     callBackendPhaseQuery newCtx
                                     callBackendPresentWordset ctx' ws wsNo CardA
                                     return $ Just (newCtx, Nothing)
                       Nothing -> do callBackendInfo ctx "Could not find another wordset"
                                     callBackendPhaseManually ctx
                                     return $ Just (ctx, Nothing)
     Nothing  -> error "no space left for wordsets"

   where -- Return the index of the smallest empty Wordset.
        smallestEmptyWordset :: WordsetMap -> Maybe Integer
        smallestEmptyWordset wsMap =
          let emptyCardNos = ((map fst) . MapL.toList) $ MapL.filter (\ q -> (q == emptyWordset)) wsMap
          in listToMaybe emptyCardNos

-- Implementation of the action ActionAccept. Expects that we are in
-- StateAsk.
actionAcceptWordset :: IORef Ctx -> IO ActionResult
actionAcceptWordset ctx' = do
  ctx <- readIORef ctx'
  let wsMap = ctxWordsets ctx
  ws' <- callBackendRetrieveWordset ctx CardA
  wsNo' <- callBackendRetrieveWordsetNo ctx CardA
  case (ws', wsNo') of
    (Just ws, Just wsNo) -> do let wsMap' = MapL.insert wsNo ws wsMap
                               newBctx <- callBackendReplaceWordset ctx wsNo Nothing ws
                               let newCtx = ctx { ctxBackend = newBctx,
                                                  ctxState = StateManually,
                                                  ctxWordsets = wsMap' }
                               writeIORef ctx' newCtx
                               dumpWordsets newCtx
                               callBackendDisplayWordsets ctx'
                               callBackendUpdateStats newCtx
                               return $ Just (newCtx, Just ActionSuggestWordset)
    _ -> error "cannot accept!" -- FIXME

-- Guaranteed to be in StateQuery.
actionRejectWordset :: IORef Ctx -> IO ActionResult
actionRejectWordset ctx' = do
  ctx <- readIORef ctx'
  callBackendPhaseManually ctx
  let newCtx = ctx { ctxState = StateManually }
  return $ Just (newCtx, Nothing)

actionSaveWordset :: Integer -> Card -> IORef Ctx -> IO ActionResult
actionSaveWordset wordsetNo card ctx' = do
  ctx <- readIORef ctx'
  let wsMap = ctxWordsets ctx
      oldWS = MapL.lookup wordsetNo wsMap
  newWS' <- callBackendRetrieveWordset ctx card
  putStrLn $ "oldWS = " ++ (show oldWS)
  let newWS = maybe (error "[actionSaveWordset] callBackendRetrieveWordset returned Nothing")
                    id
                    newWS'
  newBctx <- callBackendReplaceWordset ctx wordsetNo oldWS newWS
  let wsMap' = MapL.insert wordsetNo newWS wsMap
      newCtx = ctx { ctxBackend = newBctx,
                     ctxWordsets = wsMap' }
  writeIORef ctx' newCtx
  callBackendUpdateStats newCtx
  dumpWordsets newCtx
  callBackendDisplayWordsets ctx'
  return $ Just (newCtx, Nothing)

actionInit :: IORef Ctx -> IO ActionResult
actionInit ctx' = do
  ctx <- readIORef ctx'
  callBackendPresentWordset ctx' emptyWordset 1 CardA
  callBackendPhaseManually ctx
  callBackendUpdateStats ctx
  newCtx <- readIORef ctx'
  let newCtx' = newCtx { ctxState = StateManually }
  return $ Just (newCtx', Nothing)

-- Handler for the Quit action. Basically only informs the backend about it.
actionQuit :: IORef Ctx -> IO ActionResult
actionQuit ctx' = do
  ctx <- readIORef ctx'
  putStrLn "[actionQuit]"
  callBackendQuit ctx
  return $ Just (ctx, Nothing)
  -- throw $ ExceptionNone

-- We are in StateManually or in StateAsk.
actionSuggestOrAccept :: IORef Ctx -> IO ActionResult
actionSuggestOrAccept ctx' = do
  ctx <- readIORef ctx'
  let state = ctxState ctx
  case state of
    StateAsk      -> return $ Just (ctx, Just ActionAcceptWordset)
    StateManually -> return $ Just (ctx, Just ActionSuggestWordset)
    _             -> error $ "[actionSuggestOrAccept] while state = " ++ (show state)

-- We are in StateNothing (during initialization time), StateManually
-- or in StateAsk. But the first card (CardA) is only allowed to be
-- loaded with a wordset if we are in StaeManually.
actionLoadWordset :: Int -> Card -> IORef Ctx -> IO ActionResult
actionLoadWordset wordsetNo card ctx' = do
  ctx <- readIORef ctx'
  let state = ctxState ctx
      wordsetMap = ctxWordsets ctx
      wordset = maybe (error ("[actionLoadWordset] wordset with key "
                              ++ show wordsetNo ++ " not found in map."))
                      id
                      (MapL.lookup (toInteger wordsetNo) wordsetMap)
  when (state /= StateNothing) $
    callBackendPresentWordset ctx' wordset (toInteger wordsetNo) card
  return $ Just (ctx, Nothing)
--  when (state == StateAsk && card == CardA) $
--    error "Not allowed to load wordset in first card slot during query mode."

-- The state machine description. It defines which actions are allowed
-- during which states and which handlers to call when actions are
-- triggered.
stateMachineDescription :: [StateMachineDescription]
stateMachineDescription =
  [([StateNothing,
     StateManually,
     StateAsk],      ActionQuit,                   actionQuit),
--   ([StateNothing, StateManually, StateAsk], ActionNop,  actionNop),
   ([StateNothing],
    ActionInit,                                    actionInit),
   ([StateManually,
     StateAsk],      ActionSuggestOrAcceptWordset, actionSuggestOrAccept),
   ([StateManually], ActionSuggestWordset,         actionSuggestWordset),
   ([StateAsk],      ActionAcceptWordset,          actionAcceptWordset),
   ([StateAsk],      ActionRejectWordset,          actionRejectWordset)]
    ++ (map (\ (wordsetNo, card) -> ([StateManually],
                                     ActionSaveWordset wordsetNo card,
                                     actionSaveWordset (toInteger wordsetNo) card))
            (cartProd [1..callBackendWordsetsN] [minBound..maxBound]))
    ++ (map (\(wordsetNo, card) -> ([StateNothing, StateAsk, StateManually],
                                    ActionLoadWordset wordsetNo card,
                                    actionLoadWordset wordsetNo card))
          (cartProd [1..callBackendWordsetsN] [minBound..maxBound]))

-- Try to lookup up the StateMachineDescription, given a State and a WosaAction
stateLookup :: State -> WosaAction -> Maybe StateMachineDescription
stateLookup state wosaAction =
  let smd = filter (\ (states, action, _) -> state `elem` states && wosaAction == action)
                   stateMachineDescription
  in listToMaybe smd

-- This is the core of the program: the State Machine.
stateMachine :: IORef Ctx -> WosaAction -> IO ()
stateMachine ctx' action = do
  ctx <- readIORef ctx'
  putStrLn $ "[STM] state = " ++ show (ctxState ctx) ++ "; action = " ++ show action
  -- debugCtx ctx
  let state = ctxState ctx
      maybeSmd = stateLookup (ctxState ctx) action
      smd = maybe (error ("[STM] unexpected action " ++ show action ++ " during state " ++ show state))
                  id maybeSmd
      (_, _, actionFunc) = smd
  maybeRes <- actionFunc ctx'
  maybe (putStrLn $ "[STM] action " ++ show action ++ " during state " ++ show state ++ " failed")
        (\(newCtx, nextAction) -> do
           writeIORef ctx' newCtx
           maybe (return ()) (stateMachine ctx') nextAction)
        maybeRes

-- -- Print quadruples contained in context to stdout.
dumpWordsets :: Ctx -> IO ()
dumpWordsets ctx = do
  let wordsets = MapL.toList (ctxWordsets ctx)
  putStrLn "BEGIN DUMP WORDSETS"
  mapM_ (\ (idx, ws) -> when (ws /= emptyWordset) $ do
                            putStr   $ (numberPrefix (toInteger callBackendWordsetsN) idx)
                            putStrLn $ callBackendPrintWordset ws)
        wordsets
  putStrLn "END DUMP WORDSETS"

initialize :: [String] -> IO (Either String (IORef Ctx))
initialize args = do
  let ctx = Ctx { ctxState = StateNothing, ctxBackend = undefined, ctxDebug = True, ctxWordsets = undefined }
  ctx' <- newIORef ctx
  eitherResult <- callBackendInitialize args (stateMachine ctx')
  either (\errmsg -> return $ Left errmsg) -- less verbose
         (\(wsMap, bctx) -> do writeIORef ctx' $ ctx { ctxBackend = bctx, ctxWordsets = wsMap }
                               callBackendSetup ctx' (stateMachine ctx')
                               return $ Right ctx')
         eitherResult


wosaMain :: IORef Ctx -> IO ()
wosaMain ctx' = do
  ctx <- readIORef ctx'
  stateMachine ctx' ActionInit
  callBackendLoop ctx

-- Main entry point of the program. We use it primarily as a mechanism
-- for wrapping main' so that we can catch exceptions of main'.
main :: IO ()
main = do
  catch main'
        (\ e -> case (e :: WosaException) of
                  ExceptionString s -> putStrLn $ "Error: " ++ s
                  ExceptionNone     -> putStrLn $ "Exiting...")

  where main' :: IO ()
        main' = do
          args <- getArgs
          eitherCtx <- initialize args
          either putStrLn wosaMain eitherCtx

-- debugCtx :: Ctx -> IO ()
-- debugCtx ctx = do
--   callBackendDebugCtx (ctxBackend ctx)
--   debugMsg $ debugList "Wordsets" (MapL.toList (ctxWordsets ctx))
--   where debugMsg :: String -> IO ()
--         debugMsg s = do
--           let sLines = lines s
--           mapM_ (\ line -> putStrLn $ "  [DEBUG] " ++ line) sLines
--         debugList :: (Show a) => String -> [a] -> String
--         debugList name list =
--           name ++ " = [\n" ++ unlines (map (\ x -> "  " ++ show x) list) ++ "]\n"

callBackendWordsetsN :: Int
callBackendWordsetsN = fromInteger $ backendWordsetsN backendSpec

callBackendInitialize :: [String] -> (WosaAction -> IO ()) -> IO (Either String (WordsetMap, BackendCtx))
callBackendInitialize = backendInitialize backendSpec

callBackendLoop :: Ctx -> IO ()
callBackendLoop = backendLoop backendSpec

callBackendQuit :: Ctx -> IO ()
callBackendQuit = backendQuit backendSpec

callBackendPrintWordset :: Wordset -> String
callBackendPrintWordset = backendPrintWordset backendSpec

callBackendPhaseManually :: Ctx -> IO ()
callBackendPhaseManually = backendPhaseManually backendSpec

callBackendPhaseQuery :: Ctx -> IO ()
callBackendPhaseQuery = backendPhaseQuery backendSpec

callBackendUpdateStats :: Ctx -> IO ()
callBackendUpdateStats = backendUpdateStats backendSpec

callBackendSuggestWordset :: Ctx -> Maybe Wordset
callBackendSuggestWordset = backendSuggestWordset backendSpec

callBackendPresentWordset :: IORef Ctx -> Wordset -> Integer -> Card -> IO ()
callBackendPresentWordset = backendPresentWordset backendSpec

callBackendReplaceWordset :: Ctx -> Integer -> Maybe Wordset -> Wordset -> IO BackendCtx
callBackendReplaceWordset = backendReplaceWordset backendSpec

callBackendRetrieveWordsetNo :: Ctx -> Card -> IO (Maybe Integer)
callBackendRetrieveWordsetNo = backendRetrieveWordsetNo backendSpec

callBackendRetrieveWordset :: Ctx -> Card -> IO (Maybe Wordset)
callBackendRetrieveWordset = backendRetrieveWordset backendSpec

callBackendInfo :: Ctx -> String -> IO ()
callBackendInfo = backendInfo backendSpec

callBackendSetup :: IORef Ctx -> (WosaAction -> IO ()) -> IO ()
callBackendSetup = backendSetup backendSpec

callBackendDisplayWordsets :: IORef Ctx -> IO ()
callBackendDisplayWordsets = backendDisplayWordsets backendSpec

callBackendDebugCtx :: BackendCtx -> IO ()
callBackendDebugCtx = backendDebugCtx backendSpec
