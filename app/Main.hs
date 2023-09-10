module Main (main) where

import Control.Exception (SomeException, catch)
import Control.Monad (void)
import DBus (Variant, toVariant)
import Data.Bits (Bits (shiftR), (.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Char (intToDigit)
import Data.Default.Class (def)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text, pack, unpack)
import Data.Word (Word32)
import Desktop.Portal (AddNotificationOptions (..), Client, GetUserInformationOptions (..), GetUserInformationResults (..), NotificationButton (..), NotificationIcon (..), NotificationPriority (..), Request, addNotificationOptions)
import Desktop.Portal qualified as Portal
import Desktop.Portal.Settings (ReadAllOptions (..), ReadAllResults)
import Desktop.Portal.Settings qualified as Settings
import Documents (documents)
import Monomer
import Monomer.Hagrid
import OpenURI (openURI)
import Paths_monomer_flatpak_example (getDataFileName)
import System.Directory (getCurrentDirectory, getHomeDirectory, listDirectory)
import System.Environment (getEnvironment)

data AppModel = AppModel
  { portalClient :: Client,
    fileSystem :: Seq EnvironmentInfo,
    environmentVariables :: Seq EnvironmentInfo,
    showOpenURI :: Bool,
    showDocuments :: Bool,
    alertContents :: AlertContents
  }
  deriving (Eq, Show)

data EnvironmentInfo = EnvironmentInfo
  { key :: Text,
    value :: Text
  }
  deriving (Eq, Show)

data AlertContents
  = AlertNotShown
  | AlertRequestingUserInformation (Request GetUserInformationResults)
  | AlertUserInformation GetUserInformationResults
  | AlertSettings ReadAllResults
  | AlertMessage {title :: Text, body :: Text}
  deriving (Eq, Show)

data AppEvent
  = AppInit
  | AppInitFinish (Seq EnvironmentInfo) (Seq EnvironmentInfo)
  | GetUserInformation
  | GetUserInformationStart (Request GetUserInformationResults)
  | GetUserInformationFinish GetUserInformationResults
  | ShowAlertMessage {title :: Text, body :: Text}
  | AddNotification
  | ReadSettings
  | ReadSettingsFinish ReadAllResults
  | RetrieveSecret
  | SetShowOpenURI Bool
  | SetShowDocuments Bool
  | RequestFailed Text
  | CancelRequest
  | CloseAlert

main :: IO ()
main = do
  regularFontPath <- pack <$> getDataFileName "/fonts/Cantarell/Cantarell-Regular.ttf"
  boldFontPath <- pack <$> getDataFileName "/fonts/Cantarell/Cantarell-Bold.ttf"
  iconPath <- pack <$> getDataFileName "/io.github.Dretch.MonomerFlatpakExample.png"
  portalClient <- Portal.connect
  void (Portal.handleNotificationActionInvoked portalClient handleNotificationAction)
  startApp (initialModel portalClient) handleEvent buildUI (config regularFontPath boldFontPath iconPath)
  where
    initialModel portalClient =
      AppModel
        { portalClient,
          fileSystem = mempty,
          environmentVariables = mempty,
          showOpenURI = False,
          showDocuments = False,
          alertContents = AlertNotShown
        }
    config regularFontPath boldFontPath iconPath =
      [ appTheme darkTheme,
        appWindowTitle "Monomer Flatpak Example",
        appWindowIcon iconPath,
        appWindowState (MainWindowNormal (1000, 800)),
        appFontDef "Regular" regularFontPath,
        appFontDef "Bold" boldFontPath,
        appDisableAutoScale True,
        appInitEvent AppInit
      ]

buildUI :: UIBuilder AppModel AppEvent
buildUI _wenv model = tree
  where
    tree =
      zstack
        [ vstack_
            [childSpacing_ 5]
            [ label "Monomer Flatpak Example"
                `styleBasic` [textSize 40, paddingL 5, paddingT 10, paddingR 10],
              label "This is a demo of the monomer framework running inside the Flatpak sandbox."
                `styleBasic` [padding 5],
              hstack_
                [childSpacing]
                [ label "Portals:",
                  button "Get User Information" GetUserInformation,
                  button "Documents" (SetShowDocuments True),
                  button "Add Notification" AddNotification,
                  button "Read Settings" ReadSettings,
                  button "Open URI" (SetShowOpenURI True),
                  button "Retrieve Secret" RetrieveSecret
                ]
                `styleBasic` [padding 5],
              hagrid
                [ (textColumn "File System Info" (.key)) {initialWidth = 300},
                  (widgetColumn "" valueColumn) {initialWidth = 700}
                ]
                model.fileSystem,
              hagrid
                [ (textColumn "Environment Variable" (.key)) {initialWidth = 300},
                  (widgetColumn "" valueColumn) {initialWidth = 700}
                ]
                model.environmentVariables
            ],
          maybeOpenURI,
          maybeDocuments,
          -- nodeKey to workaround https://github.com/fjvallarino/monomer/issues/265
          maybeAlert `nodeKey` pack (show model.alertContents)
        ]

    maybeOpenURI
      | model.showOpenURI =
          alert_
            (SetShowOpenURI False)
            [titleCaption "OpenURI Portal"]
            (openURI model.portalClient ShowAlertMessage `styleBasic` [padding 20])
      | otherwise = spacer `nodeVisible` False

    maybeDocuments
      | model.showDocuments =
          alert_
            (SetShowDocuments False)
            [titleCaption "Documents Portal"]
            (documents model.portalClient ShowAlertMessage `styleBasic` [padding 20])
      | otherwise = spacer `nodeVisible` False

    maybeAlert = case model.alertContents of
      AlertNotShown ->
        spacer `nodeVisible` False
      AlertRequestingUserInformation _ ->
        alert_
          CancelRequest
          [titleCaption "Getting User Information"]
          (label "Wait or cancel by closing this alert..." `styleBasic` [padding 20])
      AlertMessage {title, body} ->
        alert_
          CloseAlert
          [titleCaption title]
          (label_ body [multiline] `styleBasic` [padding 20])
      AlertUserInformation info ->
        alert_
          CloseAlert
          [titleCaption "Get User Information Response"]
          (userInfoAlertContents info `styleBasic` [padding 20])
      AlertSettings results ->
        alert_
          CloseAlert
          [titleCaption "Read Settings Response"]
          (settingsAlertContents results `styleBasic` [height 400, width 800, padding 10])

    userInfoAlertContents results =
      vstack_
        [childSpacing]
        [ label "Request successful.",
          label ("User Id: " <> results.id),
          label ("User Name: " <> results.name),
          label ("User Image: " <> maybe "[none]" pack results.image)
        ]

    settingsAlertContents :: ReadAllResults -> WidgetNode () AppEvent
    settingsAlertContents results =
      hagrid
        [ (textColumn "Namespace" (.namespace)) {initialWidth = 200},
          (textColumn "Key" (.key)) {initialWidth = 200},
          (textColumn "Value" (pack . show . (.value))) {initialWidth = 200},
          (textColumn "Standard Value" (pack . show . (.standardValue))) {initialWidth = 500}
        ]
        (Seq.fromList results.values)

valueColumn :: Int -> EnvironmentInfo -> WidgetNode s e
valueColumn _ix model =
  label_ model.value [multiline, ellipsis]

handleEvent :: EventHandler AppModel AppEvent sp ep
handleEvent _wenv _node model = \case
  AppInit ->
    [ Task $ do
        curDir <- pack <$> getCurrentDirectory
        homeDir <- pack <$> getHomeDirectory
        lsRoot <- pack . unlines <$> listDirectory "/"
        lsCurDir <- pack . unlines <$> listDirectory (unpack curDir)
        lsHomeDir <- pack . unlines <$> listDirectory (unpack homeDir)
        envVars <- Seq.fromList . map (\(k, v) -> EnvironmentInfo (pack k) (pack v)) <$> getEnvironment
        pure $
          AppInitFinish
            ( Seq.fromList
                [ EnvironmentInfo "Current Directory" curDir,
                  EnvironmentInfo "Home Directory" homeDir,
                  EnvironmentInfo "ls /" lsRoot,
                  EnvironmentInfo ("ls " <> curDir) lsCurDir,
                  EnvironmentInfo ("ls " <> homeDir) lsHomeDir
                ]
            )
            envVars
    ]
  AppInitFinish fileSystem environmentVariables ->
    [Model model {fileSystem, environmentVariables}]
  GetUserInformation ->
    [ Producer $ \emit -> do
        catchRequestErrors emit $ do
          req <-
            Portal.getUserInformation
              model.portalClient
              def
                { reason = Just "Allows FlatpakMonomerExample to show user information."
                }
          emit (GetUserInformationStart req)
          Portal.await req >>= \case
            Nothing -> emit CloseAlert
            Just result -> emit (GetUserInformationFinish result)
    ]
  GetUserInformationStart res ->
    [Model model {alertContents = AlertRequestingUserInformation res}]
  GetUserInformationFinish info ->
    [Model model {alertContents = AlertUserInformation info}]
  SetShowOpenURI showOpenURI ->
    [Model model {showOpenURI}]
  SetShowDocuments showDocuments ->
    [Model model {showDocuments}]
  AddNotification ->
    [ Producer $ \emit -> do
        catchRequestErrors emit $
          Portal.addNotification model.portalClient $
            (addNotificationOptions "testNotification")
              { title = Just "Test Notification",
                body = Just "Hello!",
                priority = Just NotificationPriorityLow,
                icon = Just (NotificationIconThemed ["weather-snow", "zoom-in"]),
                defaultAction = Just "testDefaultAction",
                defaultActionTarget = Just (toVariant ("wibble" :: Text, 42 :: Word32)),
                buttons = Just [NotificationButton {label_ = "Click Me", action = "testButtonAction", target = Nothing}]
              }
    ]
  ReadSettings ->
    [ Producer $ \emit ->
        catchRequestErrors emit $ do
          result <- Settings.readAll model.portalClient (ReadAllOptions [])
          emit (ReadSettingsFinish result)
    ]
  ReadSettingsFinish results ->
    [Model model {alertContents = AlertSettings results}]
  RetrieveSecret ->
    [ Producer $ \emit -> do
        catchRequestErrors emit $ do
          secret <- Portal.retrieveSecret model.portalClient
          emit (ShowAlertMessage "Retrieved Secret Successfully" (hex secret))
    ]
  ShowAlertMessage {title, body} ->
    [Model model {alertContents = AlertMessage {title, body}}]
  CancelRequest ->
    let cancel = case model.alertContents of
          AlertRequestingUserInformation res ->
            [Producer (const (Portal.cancel res))]
          _ -> []
     in cancel <> [Model model {alertContents = AlertNotShown}]
  CloseAlert ->
    [Model model {alertContents = AlertNotShown}]
  RequestFailed msg ->
    [Model model {alertContents = AlertMessage "Portal Error" msg}]

catchRequestErrors :: (AppEvent -> IO ()) -> IO () -> IO ()
catchRequestErrors emit cmd = catch cmd handler
  where
    handler (e :: SomeException) =
      emit (RequestFailed . pack . show $ e)

handleNotificationAction :: Text -> Text -> Maybe Variant -> IO ()
handleNotificationAction notificationId action actionTarget = do
  putStrLn $ "Received notification action:"
  putStrLn $ "  Notification Id: " <> show notificationId
  putStrLn $ "  Action: " <> show action
  putStrLn $ "  Action Target: " <> show actionTarget

hex :: ByteString -> Text
hex = pack . ByteString.foldr' (\w acc -> hexChar (shiftR w 4) : hexChar (w .&. 0b1111) : acc) []
  where
    hexChar = intToDigit . fromIntegral
