-- Nebelfiller Backend for Wosa.
-- Copyright (C) 2015 Moritz Schulte <mtesseract@silverratio.net>

{-# LANGUAGE OverloadedStrings #-}

module Nebelfiller (Wordset, Card, BackendCtx, backendSpec) where

import Data.IORef                   -- We manage state with IORefs.

import Graphics.UI.Gtk
import Data.Maybe
import Data.List
import Data.Char
import Data.String.Utils
import Data.List.Split
import Control.Monad.Trans(liftIO)  -- We need liftIO for some Gtk related code.
import qualified Data.Map.Lazy as MapL
import Control.Monad
import Nebelfiller.Datatypes
import Nebelfiller.Utilities

import Wosa
import Utilities
import Paths_Wosa

import Nebelfiller.GUIHelpers
import Nebelfiller.Parser

nebelfillerGtkBuilder :: String
nebelfillerGtkBuilder = "Nebelfiller.glade"

-- Each ItemPair resp. each Item can be used three times.
nebelfillerRepetitionsN :: Integer
nebelfillerRepetitionsN = 3

-- This is the number of wordsets this Wosa backend wants to produce.
nebelfillerWordsetsN :: Int
nebelfillerWordsetsN = 27
-- nebelfillerWordsetsN = 87

backendUsage :: String
backendUsage = "<list1> <list2> <number>"

-- Extract the Builder object from a IORef-wrapped Ctx.
getBuilder' :: IORef Ctx -> IO Builder
getBuilder' ctx' = do
  ctx <- readIORef ctx'
  return $ (ctxBuilder . ctxBackend) ctx

-- Extract the Builder object from a Ctx.
getBuilder :: Ctx -> Builder
getBuilder = ctxBuilder . ctxBackend

---------------------------------
-- Backend API Implementations --
---------------------------------

-- Enter the backend-specific main loop.
nebelfillerLoop :: Ctx -> IO ()
nebelfillerLoop _ = do
  putStrLn "[Nebelfiller] Entering backendLoop"
  mainGUI

-- Create a new BackendContext value and a new WordsetMap.
initState :: Builder -> [ItemPair] -> [Item] -> (WordsetMap, BackendCtx)
initState builder listOne listTwo =
  let bctx = BackendCtx { ctxListOne          = listOne
                        , ctxListTwo          = listTwo
                        , ctxListOneCounter   = initListCounter listOne
                        , ctxListTwoCounter   = initListCounter listTwo
                        , ctxBuilder          = builder
                        , ctxDisableCallbacks = False
                        }
  in (emptyWordsetsMap nebelfillerWordsetsN, bctx)

  where emptyWordsetsMap :: Int -> WordsetMap
        emptyWordsetsMap m =
            MapL.fromList $ map (\ n -> (toInteger n, emptyWordset)) [1..m]

        -- Initialize the listOne resp. listTwo counters in the
        -- Backend Ctx.
        initListCounter :: (Ord a) => [a] -> MapL.Map a (Int, Int)
        initListCounter list =
          let list' = zip list [1..]
          in foldl (\ m (pair, idx) ->
                     MapL.insert pair (idx, (fromInteger nebelfillerRepetitionsN)) m)
               MapL.empty list'

setupFonts :: BackendCtx -> IO ()
setupFonts bctx = do
  let builder = ctxBuilder bctx
  textView <- guiTextView builder (guiControlName WordsetView)
  fd <- fontDescriptionNew
  fontDescriptionSetFamily fd ("Monospace" :: String)
  widgetModifyFont textView (Just fd)

-- Setup the GUI: initialize the Cards (ComboBoxes, Choosers,
-- Entries), install callbacks, show main window and disable all
-- widgets (they will be enabled selectively).
guiSetup :: IORef Ctx -> (WosaAction -> IO ()) -> IO ()
guiSetup ctx' stm = do
  ctx <- readIORef ctx'
  let bctx = ctxBackend ctx
      builder = getBuilder ctx
      wsMap = ctxWordsets ctx
  -- Setup callbacks:
  guiSetupCallbacks ctx' stm
  -- Prepare Card ComboEntries:
  mapM_ (initCardEntryModel builder) cardComboEntries
  -- Initialize Card choosers:
  initCardChooser bctx wsMap
  -- Modify Fonts:
  setupFonts bctx
  -- Disable all widgets:
  mapM_ (guiSensitivityOff builder) guiWidgets

  -- Show main window.
  window <- guiWindow builder "window_main"
  widgetShowAll window

  where initCardEntryModel :: Builder -> GUIControl -> IO ()
        initCardEntryModel builder widget = do
          combo <- guiComboBoxEntry builder (guiControlName widget)
          _ <- comboBoxEntrySetModelText combo
          return ()

-- Fill the Card Choosers with data, i.e. with a list of Wordset
-- indices.
initCardChooser :: BackendCtx -> WordsetMap -> IO ()
initCardChooser bctx wordsets = mapM_ initCardChooserCard cards
  where initCardChooserCard :: Card -> IO ()
        initCardChooserCard card = do
          let list = map (show . fst) $ MapL.toList wordsets
          combo <- guiComboBox (ctxBuilder bctx) (guiControlName (ComboCardChooser card))
          store <- listStoreNew list
          customStoreSetColumn store compareColChooser id
          cellLayoutClear combo
          comboBoxSetModel combo $ Just store
          cell <- cellRendererTextNew
          set cell [cellTextBackgroundSet := False, cellTextForegroundSet := False]
          cellLayoutClear combo
          cellLayoutPackStart combo cell True
          cellLayoutSetAttributes combo cell store (\ no -> [ cellText := no ])
          return ()

-- Setup GUI callbacks.
guiSetupCallbacks :: IORef Ctx -> (WosaAction -> IO ()) -> IO ()
guiSetupCallbacks ctx' stm = do
  -- This function installs the handlers for GUI actions.
  builder <- getBuilder' ctx'
  guiInstallCB builder "window_main" castToWindow deleteEvent
    (liftIO (callbackQuit stm >> return False))
  sequence_ [setupMenuItems, setupButtons, setupCardChoosers, setupCardEntries]

  where -- Install Button callbacks.
        setupButtons :: IO ()
        setupButtons = do
          builder <- getBuilder' ctx'
          let buttonSpecs = [(ButtonSuggestAccept, callbackSuggestOrAccept stm),
                             (ButtonRejectWordset, callbackReject stm)]
                            ++ map (\c -> (ButtonCardSave c, callbackSaveButton builder c stm))
                                [minBound..maxBound]
          mapM_ (\ (widget, cb) -> guiInstallCB builder (guiControlName widget)
                                               castToButton buttonActivated cb)
            buttonSpecs

        -- Install Button callbacks.
        setupMenuItems :: IO ()
        setupMenuItems = do
          builder <- getBuilder' ctx'
          let menuSpecs = [(MenuApplicationQuit, callbackQuit stm)]
          mapM_ (\ (widget, cb) -> guiInstallCB builder
                                               (guiControlName widget)
                                               castToMenuItem
                                               menuItemActivated
                                               cb)
            menuSpecs

        -- Install callbacks for Card Choosers:
        setupCardChoosers :: IO ()
        setupCardChoosers = do
          builder <- getBuilder' ctx'
          mapM_ (\ card -> do let chooser = ComboCardChooser card
                              combo <- guiComboBox builder (guiControlName chooser)
                              on combo changed (callbackCardChooser ctx' card stm))
            [minBound..maxBound]

        -- Install callbacks for Card Entry Combo Boxes:
        setupCardEntries :: IO ()
        setupCardEntries = do
          builder <- getBuilder' ctx'
          mapM_ (\ (card, fieldNo) -> do cardEntry <- guiComboBoxEntry builder
                                                                       (guiControlName (CardComboEntry card fieldNo))
                                         on cardEntry changed (callbackCardEntry ctx' card fieldNo stm))
            (cartProd [minBound..maxBound] [0..3])

-- Return True if the specified Item is valid or not.
isItemValid :: BackendCtx -> Int -> Item -> Bool
isItemValid bctx listNo item =
  let listOne = unwrapListOne (ctxListOne bctx)
      listTwo = ctxListTwo bctx
      list = if listNo == 1 then listOne else listTwo
  in item `elem` list

cardSetEntry :: Builder -> Card -> Integer -> String -> IO ()
cardSetEntry builder card fieldNo string = do
  entry <- guiEntry builder (guiControlName (CardEntry card fieldNo))
  entrySetText entry string

cardGetItem :: Builder -> Card -> Integer -> IO (Maybe Item)
cardGetItem builder card fieldNo = do
  s <- cardGetEntry builder card fieldNo
  return $ stringToItem s

cardGetItemValidity :: BackendCtx -> Card -> Integer -> IO (Maybe (Item, Bool))
cardGetItemValidity bctx card fieldNo = do
  let builder = ctxBuilder bctx
  maybeItem <- cardGetItem builder card fieldNo
  return $ maybe Nothing
                 (\ item -> let validity = item == emptyItem || isItemValid bctx (getListNo fieldNo) item
                                in Just (item, validity))
                 maybeItem

getListNo :: Integer -> Int
getListNo n
  | n `elem` [0,1] = 1
  | n `elem` [2,3] = 2
  | otherwise      = error $ "getListNo called for n = " ++ show n

cardGetItemValidities :: BackendCtx -> Card -> IO (MapL.Map Integer (Item, Bool))
cardGetItemValidities bctx card =
  foldl (\  m' fieldNo -> do m <- m' -- extract Map from IO Monad
                             maybeItemVal <- cardGetItemValidity bctx card fieldNo
                             return $ maybe m
                                            (\ (item, validity) -> MapL.insert fieldNo (item, validity) m)
                                            maybeItemVal)
        (return MapL.empty)
        [0..3]

cardSetItem :: Builder -> Card -> Integer -> Item -> IO ()
cardSetItem builder card fieldNo item = do
  let s = itemToString item
  cardSetEntry builder card fieldNo s

dumpHelp :: IO String
dumpHelp = do
  gladeFilename <- getDataFileName nebelfillerGtkBuilder
  return $ backendUsage ++ "\nGlade file = " ++ gladeFilename ++ "\n"

-- initCardChooser bctx
nebelfillerInitialize :: [String] -> (WosaAction -> IO ()) -> IO (Either String (WordsetMap, BackendCtx))
nebelfillerInitialize args stm = do
  putStrLn $ "[Nebelfiller] Initializing backend; args = " ++ show args
  case args of
    ["--help"] -> liftM Left dumpHelp
    [a, b]     -> backendInitialize' a b stm
    _          -> liftM Left dumpHelp -- FIXME

-- initCardChooser bctx
backendInitialize' :: String -> String -> (WosaAction -> IO ()) -> IO (Either String (WordsetMap, BackendCtx))
backendInitialize' filenameOne filenameTwo _ = do
  listOne <- parseListOne filenameOne
  listTwo <- parseListTwo filenameTwo
  gladeFilename <- getDataFileName nebelfillerGtkBuilder
  _ <- initGUI
  builder <- builderNew
  builderAddFromFile builder gladeFilename
  case listOne of
    Left err -> do putStrLn err
                   error "FIXME"
    Right _ -> return ()
  case listTwo of
    Left err -> do putStrLn err
                   error "FIXME"
    Right _ -> return ()
  let listOne' = either (const []) id listOne
      listTwo' = either (const []) id listTwo
  when (null listOne' || null listTwo') $
    error "Error: at least one of the lists is empty."
  -- Initialize context.
  let (wsMap, bctx) = initState builder listOne' listTwo'
  return $ Right (wsMap, bctx)

nebelfillerSetup :: IORef Ctx -> (WosaAction -> IO ()) -> IO ()
nebelfillerSetup ctx' stm = do
  ctx <- readIORef ctx'
  let bctx = ctxBackend ctx
      builder = ctxBuilder bctx
  guiSetup ctx' stm
  updateCardEntries bctx
  guiSensitivityOn' builder $ cardChoosers ++ [ButtonCardSave CardA, ButtonSuggestAccept]

getCardChooserNo :: Builder -> Card -> IO (Maybe Integer)
getCardChooserNo builder card = do
  combo <- guiComboBox builder (guiControlName (ComboCardChooser card))
  no <- comboBoxGetActive combo
  if no == -1
    then return Nothing
    else return (Just (toInteger no + 1))
  -- FIXME, assert no >= 0. No!
  --  return (no + 1)

setCardChooserNo :: Builder -> Card -> Integer -> IO ()
setCardChooserNo builder card wsNo = do
  combo <- guiComboBox builder (guiControlName (ComboCardChooser card))
  comboBoxSetActive combo (fromInteger wsNo - 1)

-- Backend implementation: Quit. Informs the backend about an imminent
-- quitting.
nebelfillerQuit :: Ctx -> IO ()
nebelfillerQuit _ =
  return ()

-- Backend implementation: PhaseManually. Tells the backend that we
-- are in manual mode now (this is not necessarily a mode change, we
-- also might have been in manual mode before!).
nebelfillerPhaseManually :: Ctx -> IO ()
nebelfillerPhaseManually ctx = do
  let bctx = ctxBackend ctx
      builder = ctxBuilder bctx
  -- Disable reject button:
  guiSensitivityOff builder ButtonRejectWordset
  -- Enable card entries for all cards:
  mapM_ (maybeEnableCardEntries bctx) [minBound..maxBound]
  -- Enable Save Buttons if item entries contain reasonable items:
  mapM_ (maybeEnableSaveButton  bctx) [minBound..maxBound]

maybeEnableCardEntries :: BackendCtx -> Card -> IO ()
maybeEnableCardEntries bctx card = do
  let builder = ctxBuilder bctx
  maybeChooserNo <- getCardChooserNo builder card
  maybe (return ())
        (\ _ -> mapM_ (\ entryNo -> do guiSensitivityOn builder (CardComboEntry card entryNo)
                                       guiSensitivityOn builder (CardEntry card entryNo))
                  [0..3])
        maybeChooserNo

maybeEnableSaveButton' :: BackendCtx -> Card -> MapL.Map Integer (Item, Bool) -> IO ()
maybeEnableSaveButton' bctx card itemValidities = do
  let builder = ctxBuilder bctx
  maybeChooserNo <- getCardChooserNo builder card
  if isJust maybeChooserNo && MapL.null (MapL.filter (not . snd) itemValidities)
     then guiSensitivityOn  builder (ButtonCardSave card)
     else guiSensitivityOff builder (ButtonCardSave card)

maybeEnableSaveButton :: BackendCtx -> Card -> IO ()
maybeEnableSaveButton bctx card = do
  -- Compute validity of all four items:
  itemValidities <- cardGetItemValidities bctx card
  maybeEnableSaveButton' bctx card itemValidities
  -- If the card contains four valid items, enable the save button, if
  -- not, disable it.

nebelfillerPhaseQuery :: Ctx -> IO ()
nebelfillerPhaseQuery ctx = do
  putStrLn "[Nebelfiller] Entering phase Query"
  let bctx = ctxBackend ctx
      builder = ctxBuilder bctx
  -- Enable Reject Button
  guiSensitivityOn builder ButtonRejectWordset
  -- Disable Save Buttons
  mapM_ (guiSensitivityOff builder . ButtonCardSave) [minBound..maxBound]

nebelfillerUpdateStats :: Ctx -> IO ()
nebelfillerUpdateStats ctx = do
  let bctx = ctxBackend ctx
      wsMap = ctxWordsets ctx
      builder = ctxBuilder bctx
  mapM_ (\(updateFunc, label) -> let info = updateFunc wsMap
                                 in guiSetLabel builder label info)
        globalStatsDescription

-- This bundles the statistics implementations.
globalStatsDescription :: [(WordsetMap -> String, String)]
globalStatsDescription =
  [(statsTotal,                                                     "label_quadruple_info"),
   (printList . constraintZeroStats_IncompleteWordsets,             "label_incomplete_wordsets"),
   (printList . constraintOneStats_NonUniqueInitialSounds,          "label_initial_sounds"),
   (printList . constraintTwoStats_WordsetGroupsSharingItems,       "label_wordsets_sharing_item"),
   (printList . constraintThreeStats_WordsetsSharingListTwoPair,    "label_wordsets_same_listtwo_pair"),
   (printList . constraintFourStats_wordsetIntersectionsGreaterTwo, "label_wordsets_intersection_too_big")]


statsTotal :: WordsetMap -> String
statsTotal wordsets =
  let wordsets' = completeWordsets wordsets
      wordsetsN' = length wordsets'
  in show wordsetsN' ++ "/" ++ show nebelfillerWordsetsN

completeWordsets :: WordsetMap -> [Wordset]
completeWordsets wordsetMap =
  let wordsetList = map snd $ MapL.toList wordsetMap
  in filter isComplete wordsetList
  where isComplete = not . wordsetIncomplete

wordsetIncomplete :: Wordset -> Bool
wordsetIncomplete ws = emptyItem `elem` wordsetItems ws

-- Extract the items of a Wordset.
wordsetItems :: Wordset -> [Item]
wordsetItems ws = map (wordsetItem ws) wordsetItemIndices

wordsetFromItems :: [Item] -> Maybe Wordset
wordsetFromItems items =
  case items of
    [item0, item1, item2, item3] -> Just $ Wordset item0 item1 item2 item3
    _ -> Nothing

wordsetIntersection :: Wordset -> Wordset -> [Item]
wordsetIntersection ws0 ws1 =
    let ws0items = filter (/= emptyItem) $ wordsetItems ws0
        ws1items = filter (/= emptyItem) $ wordsetItems ws1
    in listIntersection ws0items ws1items

wordsetElem :: Item -> Wordset -> Bool
wordsetElem item ws = item `elem` wordsetItems ws

nebelfillerSuggestWordset :: Ctx -> Maybe Wordset
nebelfillerSuggestWordset ctx =
  --builder <- getBuilder bctx'
  -- FIXME: assert that we still have items left!
  -- Get next listOne item pair:
  let bctx = ctxBackend ctx
      wsMap = ctxWordsets ctx
  in tryPairCompletion wsMap bctx

computePairCompletions :: WordsetMap -> [Item] -> (Item, Item) -> [(Item, Item)]
computePairCompletions wsMap listTwoRemaining (item0, item1) =
  let possiblePairs = cartProd listTwoRemaining listTwoRemaining
  in filter (checkIt wsMap (item0, item1)) possiblePairs

  where checkIt :: WordsetMap -> (Item, Item) -> (Item, Item) -> Bool
        checkIt wordsets (it0, it1) (it2, it3) =
         let ws = Wordset it0 it1 it2 it3
         in constraintZeroChecker_CompleteWordsets ws
            && constraintOneChecker_InitialSoundsUnique ws
            && constraintTwoChecker_WordsetGroupsSharingItems wordsets ws
            && constraintThreeChecker_WordsetsDistinctListTwoPairs wordsets ws
            && constraintFourChecker_WordsetIntersectionAtMostTwo wordsets ws

tryPairCompletion :: WordsetMap -> BackendCtx -> Maybe Wordset
tryPairCompletion wordsetMap bctx =
  let pairsAvailable = listOnePairsAvailable bctx
      itemsAvailable = listTwoItemsAvailable bctx
  in tryPairCompletion' wordsetMap pairsAvailable itemsAvailable

tryPairCompletion' :: WordsetMap -> [ItemPair] -> [Item] -> Maybe Wordset
tryPairCompletion' _ [] _ = Nothing
tryPairCompletion' _ _ [] = Nothing
tryPairCompletion' wsMap (x:listOneRemaining) listTwoRemaining =
  let ItemPair item0 item1 = x
      compatiblePairs = computePairCompletions wsMap listTwoRemaining (item0, item1)
  in if null compatiblePairs
        then tryPairCompletion' wsMap listOneRemaining listTwoRemaining
        else let (itemA, itemB) = head compatiblePairs
             in Just $ Wordset item0 item1 itemA itemB

-- cardEntryName :: Card -> Integer -> String
-- cardEntryName card field = "card_" ++ (show (fromEnum card)) ++ "_" ++ (show field)

compareColChooser :: ColumnId String String
compareColChooser = makeColumnIdString 0

compareCol :: ColumnId ItemDesc String
compareCol = makeColumnIdString 0

listOnePairsAvailable :: BackendCtx -> [ItemPair]
listOnePairsAvailable bctx =
  (listFst
   . (sortBy cmpPairs)
   . MapL.toList
   . (MapL.filter (\ (_, counter) -> counter > 0)))
    (ctxListOneCounter bctx)
  where cmpPairs (_, (idx, _)) (_, (idx', _)) = compare idx idx'
    
listTwoItemsAvailable :: BackendCtx -> [Item]
listTwoItemsAvailable bctx =
  (listFst
   . (sortBy cmpItems)
   . MapL.toList
   . MapL.filter (\ (_, counter) -> counter > 0))
    (ctxListTwoCounter bctx)
  where cmpItems (_, (idx, _)) (_, (idx', _)) = compare idx idx'

-- FIXME, write better.
updateCardEntries' :: BackendCtx -> Card -> IO ()
updateCardEntries' bctx card = do
  let listOne' = (sort . unwrapListOne) $ ctxListOne bctx
      listOneRemaining' = unwrapListOne $ listOnePairsAvailable bctx
      listTwo = sort $ ctxListTwo bctx
      listTwoRemaining = listTwoItemsAvailable bctx
  mapM_ (updateCardEntry card listOne' listOneRemaining') [0, 1]
  mapM_ (updateCardEntry card listTwo listTwoRemaining) [2, 3]

  where updateCardEntry :: Card -> [Item] -> [Item] -> Integer -> IO ()
        updateCardEntry c entriesAll entriesRemaining entryNo = do
         combo <- guiComboBoxEntry (ctxBuilder bctx) (guiControlName (CardComboEntry c entryNo))
         let entriesAll' = map itemToString entriesAll
             entriesRemaining' = map itemToString entriesRemaining
             comboData = ItemDesc { itemAvailable = True, itemString = "" }
                            : prepareComboData entriesAll' entriesRemaining'
         updateComboBox combo comboData

        prepareComboData list listRemaining =
           map (\ item -> ItemDesc { itemAvailable = item `elem` listRemaining,
                                     itemString = item })
               list

updateCardEntries :: BackendCtx -> IO ()
updateCardEntries bctx = mapM_ (updateCardEntries' bctx) [minBound..maxBound]

updateComboBox :: ComboBoxEntry -> [ItemDesc] -> IO ()
updateComboBox combo items = do
      store <- listStoreNew items
      customStoreSetColumn store compareCol itemString
      cellLayoutClear combo
      comboBoxSetModel combo $ Just store
      cell <- cellRendererTextNew
      set cell [cellTextBackgroundSet := False, cellTextForegroundSet := True]
      cellLayoutClear combo
      cellLayoutPackStart combo cell True
      cellLayoutSetAttributes combo cell store
       $ \row -> [ cellText := itemString row, cellTextForegroundColor := itemColor row]
      entry' <- binGetChild combo
      let entry = castToEntry $ fromJust entry'
      completion <- entryCompletionNew
      entrySetCompletion entry completion
      set completion [entryCompletionModel := Just store]
      cellLayoutClear completion
      cellLayoutPackStart completion cell True
      cellLayoutSetAttributes completion cell store
        (\ item -> [cellText := itemString item,
                    cellTextForegroundColor := itemColor item])
      entryCompletionSetMatchFunc completion (matchFunc store)
      -- FIXME: Do we have to clear the list of registered callbacks
      -- or do we only have to do it once?
      _ <- on completion matchSelected $ \ model iter -> do
             color <- treeModelGetValue model iter compareCol
             entrySetText entry color
             return True
      return ()

   where itemColor itemDesc = if itemAvailable itemDesc
                                -- Print already taken items in grey.
                                then Color 0 0 0             -- Black
                                else Color 40000 40000 40000 -- Grey.

         matchFunc model str iter = do
          tp <- treeModelGetPath model iter
          case tp of
            (i:_) -> do row <- listStoreGetValue model i
                        return $ any (isPrefixOf (map toLower str))
                                     (words (map toLower (itemString row)))
            _ -> return False

data ItemDesc = ItemDesc { itemAvailable :: Bool,
                           itemString :: String } deriving Show

-- Present the given Wordset to the user in the specified card.
nebelfillerPresentWordset :: IORef Ctx -> Wordset -> Integer -> Card -> IO ()
nebelfillerPresentWordset ctx' ws wsNo card = do
  disableCallbacks ctx'
  ctx <- readIORef ctx'
  let bctx = ctxBackend ctx
      builder =  ctxBuilder bctx
  -- Present Items:
  mapM_ (\ (item, fieldNo) -> presentItem bctx item card fieldNo)
    (zip (wordsetItems ws) [0..3])
  -- Update Card Chooser:
  setCardChooserNo builder card wsNo

  mapM_ (maybeEnableCardEntries bctx) [minBound..maxBound]
  when (ctxState ctx == StateManually) $
    maybeEnableSaveButton bctx card

  when (ctxState ctx == StateAsk && card == CardA) $
    -- FIXME, not quite what is desired.
    guiSensitivityOff' builder (map (CardComboEntry card) [0..3])
  enableCallbacks ctx'

-- Given two items, try to compute the associated ItemPair as it is
-- contained in listOne. If no of the two possible item combinations
-- is found, return Nothing.
findItemPair :: BackendCtx -> Item -> Item -> Maybe ItemPair
findItemPair bctx item0 item1
  | itemPair0 `elem` listOne = Just itemPair0
  | itemPair1 `elem` listOne = Just itemPair1
  | otherwise                = Nothing
  where itemPair0 = ItemPair item0 item1
        itemPair1 = ItemPair item1 item0
        listOne = ctxListOne bctx

markWordsetAvailable :: Maybe Wordset -> BackendCtx -> BackendCtx
markWordsetAvailable Nothing bctx = bctx
markWordsetAvailable (Just (Wordset item0 item1 item2 item3)) bctx =
  let listOneCounter = ctxListOneCounter bctx
      listTwoCounter = ctxListTwoCounter bctx
      listOneCounter' = let maybePair = normalizeItemPair bctx item0 item1
                        in maybe listOneCounter (incrementCounter listOneCounter) maybePair
      listTwoCounter' = incrementCounters (/= emptyItem) listTwoCounter [item2, item3]
  in bctx { ctxListOneCounter = listOneCounter'
          , ctxListTwoCounter = listTwoCounter'
          }

normalizeItemPair :: BackendCtx -> Item -> Item -> Maybe ItemPair
normalizeItemPair bc i0 i1 =
  if (i0, i1) == (emptyItem, emptyItem)
      then Nothing
      else mplus (findItemPair bc i0 i1)
                 (error ("[normalizeItemPair] finditemPair returned Nothing\
                         \for items " ++ show (i0, i1)))

modifyCounter :: (Show a, Ord a) =>
                 (Int -> Int) -> a -> MapL.Map a (Int, Int) -> MapL.Map a (Int, Int)
modifyCounter f k m =
  let maybeVal = MapL.lookup k m
      (idx, counter) = fromMaybe (error ("[modifyCounter] no counter found \
                                         \for key = " ++ show k))
                          maybeVal
      counter' = f counter
  in MapL.insert k (idx, counter') m

incrementCounter :: (Show a, Ord a) => MapL.Map a (Int, Int) -> a -> MapL.Map a (Int, Int)
incrementCounter m k = modifyCounter (1 +) k m

incrementCounters :: (Show a, Ord a) =>
                     (a -> Bool) -> MapL.Map a (Int, Int) -> [a] -> MapL.Map a (Int, Int)
incrementCounters filterFunc m ks =
  foldl incrementCounter m $ filter filterFunc ks

decrementCounter :: (Show a, Ord a) => MapL.Map a (Int, Int) -> a -> MapL.Map a (Int, Int)
decrementCounter m k = modifyCounter (+ (-1)) k m

decrementCounters :: (Show a, Ord a) =>
                     (a -> Bool) -> MapL.Map a (Int, Int) -> [a] -> MapL.Map a (Int, Int)
decrementCounters filterFunc m ks =
  foldl decrementCounter m $ filter filterFunc ks

markWordsetUsed :: Maybe Wordset -> BackendCtx -> BackendCtx
markWordsetUsed Nothing bctx = bctx
markWordsetUsed (Just (Wordset item0 item1 item2 item3)) bctx =
  let listOneCounter = ctxListOneCounter bctx
      listTwoCounter = ctxListTwoCounter bctx
      listOneCounter' = let maybePair = normalizeItemPair bctx item0 item1
                        in maybe listOneCounter (decrementCounter listOneCounter) maybePair
      listTwoCounter' = decrementCounters (/= emptyItem) listTwoCounter [item2, item3]
  in bctx { ctxListOneCounter = listOneCounter'
          , ctxListTwoCounter = listTwoCounter'
          }

nebelfillerReplaceWordset :: Ctx -> Integer -> Maybe Wordset -> Wordset -> IO BackendCtx
nebelfillerReplaceWordset ctx _ oldWS newWS = do
  let bctx = ctxBackend ctx
      bctx' = (markWordsetUsed (Just newWS) . markWordsetAvailable oldWS) bctx
  updateCardEntries bctx'
  return bctx'

presentItem :: BackendCtx -> Item -> Card -> Integer -> IO ()
presentItem bctx item card fieldNo = do
  let builder = ctxBuilder bctx
      itemIdx
        | item == emptyItem = Just 0
        | getListNo fieldNo == 1 =
          findItemIdx ((sort . unwrapListOne) (ctxListOne bctx)) item
        | otherwise = findItemIdx (sort (ctxListTwo bctx)) item
  maybe (return ()) (updateComboEntry builder) itemIdx

  where findItemIdx :: (Eq a) => [a] -> a -> Maybe Integer
        findItemIdx list x = fmap (toInteger . (+ 1)) (elemIndex x list)

        updateComboEntry :: Builder -> Integer -> IO ()
        updateComboEntry builder idx = do
          combo <- guiComboBoxEntry builder (guiControlName (CardComboEntry card fieldNo))
          entry <- guiEntry builder (guiControlName (CardEntry card fieldNo))
          set entry [entryEditable := False]
          comboBoxSetActive combo (fromInteger idx)

cardGetEntry :: Builder -> Card -> Integer -> IO String
cardGetEntry builder card field =
  guiEntry builder (guiControlName (CardEntry card field)) >>= entryGetText

nebelfillerRetrieveWordsetNo :: Ctx -> Card -> IO (Maybe Integer)
nebelfillerRetrieveWordsetNo ctx card = do
  let bctx = ctxBackend ctx
      builder = ctxBuilder bctx
  getCardChooserNo builder card

nebelfillerRetrieveWordset :: Ctx -> Card -> IO (Maybe Wordset)
nebelfillerRetrieveWordset ctx card = do
  items <- mapM (retrieveItem ctx card) [0..3]
  if Nothing `elem` items
     then return Nothing
     else return $ wordsetFromItems (catMaybes items)

retrieveItem :: Ctx -> Card -> Integer -> IO (Maybe Item)
retrieveItem ctx card fieldNo = do
  let bctx = ctxBackend ctx
      builder = ctxBuilder bctx
  entryString <- cardGetEntry builder card fieldNo
  return $ stringToItem entryString

-- Convert an Item to a (human-readable) string representation.
itemToString :: Item -> String
itemToString (Item "" "") = ""
itemToString (Item a b) = a ++ ", " ++ b

-- Try to convert the String representation of an item to its Item
-- datatype. Fails with Nothing if the string does not consists of
-- exactly two substrings, seperated by single comma.
stringToItem :: String -> Maybe Item
stringToItem "" = Just emptyItem
stringToItem s =
  let substrings = Data.List.Split.splitOn "," s
  in case substrings of
       [item0, item1] -> Just $ Item (strip item0) (strip item1)
       _ -> Nothing

-------------------------
-- Wordset Constraints --
-------------------------

--------------------------------------------------
-- Constraint 0: All wordsets must be complete. --
--------------------------------------------------

wordsetIsComplete :: Wordset -> Bool
wordsetIsComplete wordset = emptyItem `notElem` wordsetItems wordset

wordsetIsIncomplete :: Wordset -> Bool
wordsetIsIncomplete = not . wordsetIsComplete

constraintZeroStats_IncompleteWordsets :: WordsetMap -> [Integer]
constraintZeroStats_IncompleteWordsets wordsetMap =
  (sort . listFst . MapL.toList) $ MapL.filter wordsetIsIncomplete wordsetMap

constraintZeroChecker_CompleteWordsets :: Wordset -> Bool
constraintZeroChecker_CompleteWordsets = wordsetIsComplete

-----------------------------------------------------------------------
-- Constraint 1: No wordset may contain two items beginning with the --
-- same initial sound (not quite right...).                          --
-----------------------------------------------------------------------

-- Return True if the two given items satisfy the Initial Sound
-- comndition. If one of the items is empty, this condition is
-- immediately satisfied.
initialSoundsUnique' :: Item -> Item -> Bool
initialSoundsUnique' (Item item0A item0B) (Item item1A item1B) =
    checkStrings item0A item1A
    && checkStrings item0B item1B
    && checkStrings item0A item1B
    && checkStrings item0B item1A
  where -- Return True if the Initial Sound Condition is satisfied,
        -- i.e. if the two given words do not begin with the same
        -- letter. This is automatically satisfied if one of the
        -- strings is empty.
        checkStrings :: String -> String -> Bool
        checkStrings _ "" = True
        checkStrings "" _ = True
        checkStrings (s0:_) (t0:_) =
          let (s0U, t0U) = (Data.Char.toUpper s0, Data.Char.toUpper t0)
          in s0U /= t0U

initialSoundsUnique :: Wordset -> Bool
initialSoundsUnique (Wordset item0 item1 itemA itemB) =
    initialSoundsUnique' itemA itemB
     && initialSoundsUnique' itemA item0
     && initialSoundsUnique' itemA item1
     && initialSoundsUnique' itemB item0
     && initialSoundsUnique' itemB item1

-- Constraint 1 Statistics. Compute list of indices of those Wordsets,
-- which do not satisfy the "Unique Initial Sounds" condition.
constraintOneStats_NonUniqueInitialSounds :: WordsetMap -> [Integer]
constraintOneStats_NonUniqueInitialSounds =
  listFst . MapL.toList . MapL.filter (not . initialSoundsUnique)

-- Constraint 1 Checker: Return True if the given Wordset satisfies
-- the Unique Initial Sound condition.
constraintOneChecker_InitialSoundsUnique :: Wordset -> Bool
constraintOneChecker_InitialSoundsUnique = initialSoundsUnique

--------------------------------------------------------------------------
-- Constraint 2: No single item may appear in more than three wordsets. --
--------------------------------------------------------------------------

-- Constraint 2 Statistics. Compute list of wordset groups bigger than
-- 3 where each wordset group (regarded as list of wordset indices)
-- shares an item.
constraintTwoStats_WordsetGroupsSharingItems :: WordsetMap -> [[Integer]]
constraintTwoStats_WordsetGroupsSharingItems wordsetMap =
  let wordsets = MapL.toList wordsetMap
      itemsIndexed = (filterEmptyItems . concatMap wordsetItemsIndexed) wordsets
  in (nubSort
      . extractIndices
      . extractCriticalLists
      . computeItemSharingData wordsets) itemsIndexed

  where filterEmptyItems :: [(Integer, Item)] -> [(Integer, Item)]
        filterEmptyItems = filter (\ (_, item) -> item /= emptyItem)

        wordsetItemsIndexed :: (Integer, Wordset) -> [(Integer, Item)]
        wordsetItemsIndexed (idx, ws) =
          zip (repeat idx) (wordsetItems ws)

        extractIndices :: [[(Integer, a)]] -> [[Integer]]
        extractIndices = map listFst

        -- Computes a list of those wordset groups which share items.
        computeItemSharingData :: [(Integer, Wordset)] -> [(Integer, Item)]
                                  -> [[(Integer, Wordset)]]
        computeItemSharingData wordsetsIdx =
          map (\ (_, item) ->
                filter (\(_, ws) -> item `wordsetElem` ws) wordsetsIdx)

        extractCriticalLists :: [[a]] -> [[a]]
        extractCriticalLists = filter (\ l -> length l > 3)

-- Constraint 2 Checker. Returns True if this constraint is satisfied.
constraintTwoChecker_WordsetGroupsSharingItems :: WordsetMap -> Wordset -> Bool
constraintTwoChecker_WordsetGroupsSharingItems wsMap ws =
  let wsMap' = MapL.insert (-1) ws wsMap
      sharingData = constraintTwoStats_WordsetGroupsSharingItems wsMap'
  in not $ (-1) `elemInListOfLists` sharingData

---------------------------------------------------------------------------
-- Constraint 3: No two wordsets may contain the same two listTwo items. --
---------------------------------------------------------------------------

-- Compute the intersection of the listTwo-parts of two given
-- Wordsets.
wordsetIntersectionListTwo :: Wordset -> Wordset -> [Item]
wordsetIntersectionListTwo (Wordset _ _ item0 item1) (Wordset _ _ item2 item3) =
    let ws0List = filter (/= emptyItem) [item0, item1]
        ws1List = filter (/= emptyItem) [item2, item3]
    in listIntersection ws0List ws1List

-- Compute the size of the intersection of the listTwo-parts of two
-- given Wordsets.
countListTwoIntersection :: Wordset -> Wordset -> Integer
countListTwoIntersection ws0 ws1 = toInteger $ length (wordsetIntersectionListTwo ws0 ws1)

-- Constraint 3 Stats: Return list of those wordset groups which share
-- the same two listTwo items.
constraintThreeStats_WordsetsSharingListTwoPair :: WordsetMap -> [[Integer]]
constraintThreeStats_WordsetsSharingListTwoPair wordsetMap =
  let wordsetList = MapL.toList wordsetMap
  in (nubSort
      . extractIndices
      . filterCriticalGroups
      . map (wordsetsSharingListTwoPair wordsetList)) wordsetList

  where filterCriticalGroups :: [[a]] -> [[a]]
        filterCriticalGroups = filter (lengthAtLeast 2)

        extractIndices :: [[(a, b)]] -> [[a]]
        extractIndices = map listFst

        wordsetsSharingListTwoPair :: [(Integer, Wordset)] -> (Integer, Wordset) -> [(Integer, Wordset)]
        wordsetsSharingListTwoPair wssIdx wsIdx =
          filter (listTwoPairCoincides wsIdx) wssIdx

        listTwoPairCoincides :: (Integer, Wordset) -> (Integer, Wordset) -> Bool
        listTwoPairCoincides (_, ws0) (_, ws1) =
          countListTwoIntersection ws0 ws1 == 2

-- Constraint 3 Checker. Returns True if constraint 3 is satisfied.
constraintThreeChecker_WordsetsDistinctListTwoPairs :: WordsetMap -> Wordset -> Bool
constraintThreeChecker_WordsetsDistinctListTwoPairs wsMap wordset =
  let wsMap' = MapL.insert (-1) wordset wsMap
      stats = constraintThreeStats_WordsetsSharingListTwoPair wsMap'
  in not $ (-1) `elemInListOfLists` stats

----------------------------------------------------------------
-- Constraint 4: No two wordsets may coincide in three items. --
----------------------------------------------------------------

-- Compute list of wordset groups (containing at least two wordsets)
-- whose intersection is at least three items.
constraintFourStats_wordsetIntersectionsGreaterTwo :: WordsetMap -> [[Integer]]
constraintFourStats_wordsetIntersectionsGreaterTwo wordsetMap =
  let wordsetList = MapL.toList wordsetMap
      -- Compute intersction data, then filter trivial, extract only
      -- the indices, sort the list and remove duplicates:
  in (nubSort . extractIndices . filterTrivial . computeIntersections) wordsetList

  where filterTrivial :: [[a]] -> [[a]]
        filterTrivial = filter (lengthAtLeast 2)

        extractIndices :: [[(a, b)]] -> [[a]]
        extractIndices = map listFst

        computeIntersections :: [(Integer, Wordset)] -> [[(Integer, Wordset)]]
        computeIntersections wsList =
          map (\(_, ws) -> filter (intersectionBiggerTwo ws) wsList) wsList

        intersectionBiggerTwo :: Wordset -> (Integer, Wordset) -> Bool
        intersectionBiggerTwo ws0 (_, ws1) =
          length (wordsetIntersection ws0 ws1) > 2

-- Constraint 4 Checker.

constraintFourChecker_WordsetIntersectionAtMostTwo :: WordsetMap -> Wordset -> Bool
constraintFourChecker_WordsetIntersectionAtMostTwo wordsetMap wordset =
    let wordsetMap' = MapL.insert (-1) wordset wordsetMap -- FIXME?
        intersectionData = constraintFourStats_wordsetIntersectionsGreaterTwo wordsetMap'
    in not $ (-1) `elemInListOfLists` intersectionData

nebelfillerDebugCtx :: BackendCtx -> IO ()
nebelfillerDebugCtx bctx = do
  debugMsg (debugList "listOne" (ctxListOne bctx))
  debugMsg (debugList "listTwo" (ctxListTwo bctx))
  debugMsg (debugList "listOneCounter" (MapL.toList (ctxListOneCounter bctx)))
  debugMsg (debugList "listTwoCounter" (MapL.toList (ctxListTwoCounter bctx)))

  where debugMsg :: String -> IO ()
        debugMsg s = do
          let sLines = lines s
          mapM_ (\ line -> putStrLn $ "  [DEBUG] " ++ line) sLines

        debugList :: (Show a) => String -> [a] -> String
        debugList name list =
          name ++ " = [\n" ++ unlines (map (\ x -> "  " ++ show x) list) ++ "]\n"

printItem :: Item -> String
printItem (Item "" "") = ""
printItem (Item itemA itemB) = itemA ++ ", " ++ itemB

printWordset :: Wordset -> String
printWordset (Wordset item0 item1 item2 item3) =
  printItem item0
    ++ " | "
    ++ printItem item1
    ++ " | "
    ++ printItem item2
    ++ " | "
    ++ printItem item3

printWordset' :: WordsetMap -> Wordset -> String
printWordset' wordsetMap wordset =
  let (Wordset item0 item1 item2 item3) = wordset
      [maxItem0, maxItem1, maxItem2, maxItem3] =
        map (computeWordsetMax wordsetMap . computeItemLength) [0..3]
  in addPadding (printItem item0) maxItem0
     ++ " | " ++ addPadding (printItem item1) maxItem1
     ++ " | " ++ addPadding (printItem item2) maxItem2
     ++ " | " ++ addPadding (printItem item3) maxItem3
  where computeItemLength i ws = toInteger $ length (itemToString (wordsetItem ws i))
        addPadding s i = s ++ replicate (fromInteger i - length s) ' '

computeWordsetMax :: WordsetMap -> (Wordset -> Integer) -> Integer
computeWordsetMax wordsetMap f =
  MapL.foldl (\ m ws -> let m' = f ws
                        in if m' > m then m' else m) 0 wordsetMap

nebelfillerPrintWordset :: Wordset -> String
nebelfillerPrintWordset = printWordset

nebelfillerInfo :: Ctx -> String -> IO ()
nebelfillerInfo ctx msg = do
  let bctx = ctxBackend ctx
      builder = ctxBuilder bctx
  window <- guiWindow builder "window_main"
  guiInfo window "Wosa/Nebelfiller" msg

displayWordsets :: WordsetMap -> Builder -> IO ()
displayWordsets wordsetMap builder = do
  textView <- guiTextView builder (guiControlName WordsetView)
  clearTextView textView
  let indices = sort $ MapL.keys wordsetMap
  mapM_ (\idx -> do
          let maybeWordset = MapL.lookup idx wordsetMap
              wordset = fromMaybe (error ("[displayWordset] MapL.lookup \
                                          \returned Nothing for idx = "
                                          ++ show idx))
                                  maybeWordset
          when (wordset /= emptyWordset) $ do
            let idxPrefix = numberPrefix (toInteger nebelfillerWordsetsN) idx
            addLine textView $ idxPrefix ++ printWordset' wordsetMap wordset)
        indices

  where clearTextView :: TextView -> IO ()
        clearTextView textView = do
          buffer <- textViewGetBuffer textView
          startIter <- textBufferGetStartIter buffer
          endIter <- textBufferGetEndIter buffer
          textBufferDelete buffer startIter endIter

        addLine :: TextView -> String -> IO ()
        addLine textView line = do
          buffer <- textViewGetBuffer textView
          endIter <- textBufferGetEndIter buffer
          textBufferInsert buffer endIter (line ++ "\n")

nebelfillerDisplayWordsets :: IORef Ctx -> IO ()
nebelfillerDisplayWordsets ctx' = do
  ctx <- readIORef ctx'
  displayWordsets (ctxWordsets ctx) (getBuilder ctx)

---------------
-- Callbacks --
---------------

-- This is the callback called by quit-actions in the GUI. It triggers
-- an ActionQuit action in the state machine.
callbackQuit :: (WosaAction -> IO ()) -> IO ()
callbackQuit stm = do
  mainQuit
  wrapCallback (stm ActionQuit)

-- This is the callback called when one of the save buttons is called.
callbackSaveButton :: Builder -> Card -> (WosaAction -> IO ()) -> IO ()
callbackSaveButton builder card cb = do
  maybeChooserNo <- getCardChooserNo builder card
  let chooserNo = fromMaybe (error "[callbackSaveButton] getCardChooserNo \
                                   \returned  Nothing")
                            maybeChooserNo
  cb (ActionSaveWordset (fromInteger chooserNo) card)
  -- Disable Save button to signal the succesfull saving.
  guiSensitivityOff builder (ButtonCardSave card)

areCallbacksDisabled :: IORef Ctx -> IO Bool
areCallbacksDisabled ctx' = do
  ctx <- readIORef ctx'
  let bctx = ctxBackend ctx
  return (ctxDisableCallbacks bctx)

disableCallbacks :: IORef Ctx -> IO ()
disableCallbacks = setCallbacks True

enableCallbacks :: IORef Ctx -> IO ()
enableCallbacks = setCallbacks False

setCallbacks :: Bool -> IORef Ctx -> IO ()
setCallbacks onOrOff ctx' = do
  ctx <- readIORef ctx'
  let bctx = ctxBackend ctx
      bctx' = bctx { ctxDisableCallbacks = onOrOff }
  writeIORef ctx' ctx { ctxBackend = bctx' }

-- This is the callback called when one of the card chooser
-- combo boxes is modified.
callbackCardChooser :: IORef Ctx -> Card -> (WosaAction -> IO ()) -> IO ()
callbackCardChooser ctx' card stmCb = do
  builder <- getBuilder' ctx'
  cbsDisabled <- areCallbacksDisabled ctx'
  putStrLn $ "cbsDisabled = " ++ show cbsDisabled
  unless cbsDisabled $ do
    wordsetNo <- getCardChooserNo builder card
    maybe (return ())
          (\ n -> stmCb (ActionLoadWordset (fromInteger n) card))
          wordsetNo
  -- guiSensitivityOn builder (ButtonCardSave card) -- necessary?
--  mapM_ ((guiSensitivityOn builder) . (CardEntry card)) [0..3]

-- This callback is called when one of the card entry fields is
-- changed.
callbackCardEntry :: IORef Ctx -> Card -> Integer -> (WosaAction -> IO ()) -> IO ()
callbackCardEntry ctx' card fieldNo _ = do
  ctx <- readIORef ctx'
  let bctx = ctxBackend ctx
  -- cardGetEntry builder card fieldNo
  -- entry <- guiComboBoxEntry builder (guiControlName (CardEntry card fieldNo))
  -- maybeItem <- cardGetItem builder card fieldNo

  -- Compute validity of all four items:
  itemValidities <- cardGetItemValidities bctx card

  -- In case the changed field is now a valid listOne item, change the
  -- other field to be the associated listOne item:
  when (getListNo fieldNo == 1) $ do
    let maybeItemVal = MapL.lookup fieldNo itemValidities -- FIXME?
    maybe (return ())
          (\ (item, isValid) -> when isValid (frobListOneCards item))
          maybeItemVal

  -- If the card contains four valid items, enable the save button, if
  -- not, disable it.
  when (ctxState ctx == StateManually) $
    maybeEnableSaveButton' bctx card itemValidities

  where frobListOneCards :: Item -> IO ()
        frobListOneCards item = do
          ctx <- readIORef ctx'
          let bctx = ctxBackend ctx
              builder = ctxBuilder bctx
          let otherField = 1 - fieldNo
              maybeItem = getAssociatedItem bctx item
          maybe (return ())
                (cardSetItem builder card otherField)
                maybeItem

        -- Return (Maybe) the associated ListOne Item.
        getAssociatedItem :: BackendCtx -> Item -> Maybe Item
        getAssociatedItem bctx item =
          -- Find the right listOnePair.
          if item == emptyItem
             then Just emptyItem
          else let pairs = filter (\ (ItemPair item0 item1) -> item0 == item || item1 == item)
                                  (ctxListOne bctx)
               in maybe Nothing
                        (\ (ItemPair item0 item1) -> if item == item0
                                                        then Just item1
                                                        else Just item0)
                        (listToMaybe pairs)

callbackReject :: (WosaAction -> IO ()) -> IO ()
callbackReject stm = stm ActionRejectWordset

callbackSuggestOrAccept :: (WosaAction -> IO ()) -> IO ()
callbackSuggestOrAccept stm = stm ActionSuggestOrAcceptWordset

backendSpec :: BackendSpec
backendSpec = BackendSpec
  { backendWordsetsN = toInteger nebelfillerWordsetsN
  , backendInitialize = nebelfillerInitialize
  , backendLoop = nebelfillerLoop
  , backendQuit = nebelfillerQuit
  , backendPrintWordset = nebelfillerPrintWordset
  , backendPhaseManually = nebelfillerPhaseManually
  , backendPhaseQuery = nebelfillerPhaseQuery
  , backendUpdateStats = nebelfillerUpdateStats
  , backendSuggestWordset = nebelfillerSuggestWordset
  , backendPresentWordset = nebelfillerPresentWordset
  , backendReplaceWordset = nebelfillerReplaceWordset
  , backendRetrieveWordsetNo = nebelfillerRetrieveWordsetNo
  , backendRetrieveWordset = nebelfillerRetrieveWordset
  , backendInfo = nebelfillerInfo
  , backendSetup = nebelfillerSetup
  , backendDisplayWordsets = nebelfillerDisplayWordsets
  , backendDebugCtx = nebelfillerDebugCtx
  }
