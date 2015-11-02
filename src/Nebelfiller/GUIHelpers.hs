module Nebelfiller.GUIHelpers where

import Graphics.UI.Gtk
import Control.Monad.Trans(liftIO)  -- We need liftIO for Gtk

import Nebelfiller.Datatypes
import Nebelfiller.Utilities
-- import Wosa

-- Easy retrival of widgets through Builder.

-- These are the GUI widgets.
data GUIControl = ComboCardChooser Card
                | ButtonCardSave Card
                | ButtonSuggestAccept
                | ButtonRejectWordset
                | CardComboEntry Card Integer
                | CardEntry Card Integer
                | MenuApplicationQuit
                | WordsetView
                deriving (Eq, Show, Read)

-- Convert GUIControl values into their widget names as stored in the
-- Glade file.

-- These are all cards.
cards :: [Card]
cards = [minBound..maxBound]

cardEntries :: [GUIControl]
cardEntries = map (uncurry CardEntry) (cartProd cards [0..3])

cardComboEntries :: [GUIControl]
cardComboEntries = map (uncurry CardComboEntry) (cartProd cards [0..3])

cardSaveButtons :: [GUIControl]
cardSaveButtons = map ButtonCardSave cards

cardChoosers :: [GUIControl]
cardChoosers = map ComboCardChooser cards

guiWidgets :: [GUIControl]
guiWidgets = cardChoosers
             ++ cardSaveButtons
             ++ [ButtonSuggestAccept, ButtonRejectWordset]
             ++ cardComboEntries
             ++ cardEntries

guiControlName :: GUIControl -> String
guiControlName guictrl =
  case guictrl of
    ComboCardChooser CardA    -> "card_0_chooser"
    ComboCardChooser CardB    -> "card_1_chooser"
    ComboCardChooser CardC    -> "card_2_chooser"
    ComboCardChooser CardD    -> "card_3_chooser"
    ButtonCardSave CardA      -> "button_CardA_save" -- FIXME
    ButtonCardSave CardB      -> "button_CardB_save"
    ButtonCardSave CardC      -> "button_CardC_save"
    ButtonCardSave CardD      -> "button_CardD_save"
    CardComboEntry card n     -> "card_" ++ show (fromEnum card) ++ "_" ++ show n
    CardEntry card n          -> "card_entry_" ++ show (fromEnum card) ++ "_" ++ show n
    ButtonSuggestAccept       -> "button_form_quadruple"
    ButtonRejectWordset       -> "button_reject_wordset"
    MenuApplicationQuit       -> "menu_main_quit"
    WordsetView               -> "textview_wordsets"

-- Manage Sensitivity of GUI widgets.
guiSetSensitivity :: Bool -> Builder -> GUIControl -> IO ()
guiSetSensitivity bool builder guictrl = do
  widget <- guiWidget builder (guiControlName guictrl)
  widgetSetSensitive widget bool

guiSensitivityOn :: Builder -> GUIControl -> IO ()
guiSensitivityOn = guiSetSensitivity True

guiSensitivityOn' :: Builder -> [GUIControl] -> IO ()
guiSensitivityOn' builder = mapM_ (guiSetSensitivity True builder)

guiSensitivityOff :: Builder -> GUIControl -> IO ()
guiSensitivityOff = guiSetSensitivity False

guiSensitivityOff' :: Builder -> [GUIControl] -> IO ()
guiSensitivityOff' builder = mapM_ (guiSetSensitivity False builder)

-- Reorder arguments of builderGetObject.
builderGetObject' :: GObjectClass cls =>
                     (GObject -> cls) -> Builder -> String -> IO cls
builderGetObject' = flip builderGetObject

-- Retrieve a plain widget through its name
guiWidget :: Builder -> String -> IO Widget
guiWidget = builderGetObject' castToWidget

-- Retrieve a Window widget through its name
guiWindow :: Builder -> String -> IO Window
guiWindow = builderGetObject' castToWindow

-- Retrieve an Entry widget through its name
guiEntry :: Builder -> String -> IO Entry
guiEntry = builderGetObject' castToEntry

-- Retrieve a TextView widget through its name
guiTextView :: Builder -> String -> IO TextView
guiTextView = builderGetObject' castToTextView

-- Retrieve a Label widget through its name
guiLabel :: Builder -> String -> IO Label
guiLabel = builderGetObject' castToLabel

guiComboBox :: Builder -> String -> IO ComboBox
guiComboBox = builderGetObject' castToComboBox

guiComboBoxEntry :: Builder -> String -> IO ComboBoxEntry
guiComboBoxEntry = builderGetObject' castToComboBoxEntry

-- Retrieve a Fixed widget through its name
guiFixed :: Builder -> String -> IO Fixed
guiFixed = builderGetObject' castToFixed

-- Retrieve a Button widget through its name
guiButton :: Builder -> String -> IO Button
guiButton = builderGetObject' castToButton

-- Retrieve an Alignment widget through its name
guiAlignment :: Builder -> String -> IO Alignment
guiAlignment = builderGetObject' castToAlignment

-- Retrieve an HBox widget through its name
guiHBox :: Builder -> String -> IO HBox
guiHBox = builderGetObject' castToHBox

-- Display a popup window.
guiPopup :: Bool -> Window -> String -> String -> IO ()
guiPopup _ parent title msg = do
  dialog <- dialogNew
  windowSetTransientFor dialog parent
  _ <- on dialog deleteEvent (liftIO $ return True) -- FIXME: True or False?
  set dialog [ windowTitle := title ]
  contentArea <- dialogGetUpper dialog
  label <- labelNew $ Just msg
  boxPackStart contentArea label PackGrow 10
  widgetShowAll contentArea
  _ <- dialogAddButton dialog stockOk ResponseOk
  _ <- dialogRun dialog
  widgetDestroy dialog
  return ()

-- Display a popup window containing an error message.
guiError :: Window -> String -> String -> IO ()
guiError = guiPopup True

-- Display a popup window containing an error message.
guiInfo :: Window -> String -> String -> IO ()
guiInfo = guiPopup False

guiSetLabel :: Builder -> String -> String -> IO ()
guiSetLabel builder labelName text = do
  labelObj <- builderGetObject builder castToLabel labelName
  labelSetText labelObj text

-- Install a callback handler for events. Install CALLBACK for widget
-- NAME on SIGNAL. CAST has to be the correct Gtk casting function
-- which is to be used for builderGetObject.
guiInstallCB :: (GObjectClass cls) =>
                 Builder -> String -> (GObject -> cls) ->
                 Signal cls callback -> callback -> IO ()
guiInstallCB builder name cast signal callback = do
  obj <- builderGetObject builder cast name
  _ <- on obj signal callback
  return ()
