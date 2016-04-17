-- Copyright (C) 2015-2016 Moritz Schulte <mtesseract@silverratio.net>

module Wosa where

import Data.Map
import Control.Exception
import Data.Typeable
import Data.IORef

import Nebelfiller.Datatypes

-- | These are the possible "actions". These allow for transforming a
-- given state to some other state.
data WosaAction =
     ActionInit
   | ActionNop
   | ActionSuggestWordset     -- ^ Backend shall produce a new
                              -- quadruple suggestion.
    | ActionAcceptWordset   -- ^ The user accepts the current
                            -- quadruple proposal.
   | ActionSuggestOrAcceptWordset
   | ActionRejectWordset   -- ^ The user rejects the current wordset
                           -- suggestions and wants to work on the
                           -- current quadruple manually.
   | ActionLoadWordset Int Card      -- ^ Load a quadruple into a
                                     -- card.
   | ActionSaveWordset Int Card      -- ^ The user wants to save
                                     -- quadruple on the specified
                                     -- card.
   | ActionQuit              -- ^ Program shall quit.
   deriving (Eq, Show)

type WordsetMap = Map Integer Wordset

data WosaException = ExceptionString String | ExceptionNone
    deriving (Show, Typeable)

instance Exception WosaException

-- | These are the possible states types.
data State = StateNothing -- ^ Dummy state.
  | StateManually         -- ^ User is free to modify edit wordsets.
  | StateAsk              -- ^ Used is presented a new wordset
                          -- suggestion and asked if that is a good
                          -- word set.
  deriving (Eq, Show)

-- | The global state of this application is stored in the Ctx
-- datatype.
data Ctx = Ctx { ctxState    :: State
               , ctxDebug    :: Bool
               , ctxBackend  :: BackendCtx
               , ctxWordsets :: WordsetMap
               }

-- | Actions implemented by the backend.
type BackendActionInitialize        = [String] -> (WosaAction -> IO ())
                                               -> IO (Either String (WordsetMap, BackendCtx))
type BackendActionLoop              = Ctx -> IO ()
type BackendActionQuit              = Ctx -> IO ()
type BackendActionPrintWordset      = Wordset -> String
type BackendActionPhaseManually     = Ctx -> IO ()
type BackendActionPhaseQuery        = Ctx -> IO ()
type BackendActionUpdateStats       = Ctx -> IO ()
type BackendActionSuggestWordset    = Ctx -> Maybe Wordset
type BackendActionPresentWordset    = IORef Ctx -> Wordset -> Integer -> Card -> IO ()
type BackendActionReplaceWordset    = Ctx -> Integer -> Maybe Wordset -> Wordset -> IO BackendCtx
type BackendActionRetrieveWordsetNo = Ctx -> Card -> IO (Maybe Integer)
type BackendActionRetrieveWordset   = Ctx -> Card -> IO (Maybe Wordset)
type BackendActionInfo              = Ctx -> String -> IO ()
type BackendActionSetup             = IORef Ctx -> (WosaAction -> IO ()) -> IO ()
type BackendActionDisplayWordsets   = IORef Ctx -> IO ()
type BackendActionDebugCtx          = BackendCtx -> IO ()

data BackendSpec =
  BackendSpec { backendWordsetsN         :: Integer
              , backendInitialize        :: BackendActionInitialize
              , backendLoop              :: BackendActionLoop
              , backendQuit              :: BackendActionQuit
              , backendPrintWordset      :: BackendActionPrintWordset
              , backendPhaseManually     :: BackendActionPhaseManually
              , backendPhaseQuery        :: BackendActionPhaseQuery
              , backendUpdateStats       :: BackendActionUpdateStats
              , backendSuggestWordset    :: BackendActionSuggestWordset
              , backendPresentWordset    :: BackendActionPresentWordset
              , backendReplaceWordset    :: BackendActionReplaceWordset
              , backendRetrieveWordsetNo :: BackendActionRetrieveWordsetNo
              , backendRetrieveWordset   :: BackendActionRetrieveWordset
              , backendInfo              :: BackendActionInfo
              , backendSetup             :: BackendActionSetup
              , backendDisplayWordsets   :: BackendActionDisplayWordsets
              , backendDebugCtx          :: BackendActionDebugCtx
              }
