{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module ProtoTui.Draw where

import Brick
import Brick.Forms
import Brick.Focus
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import Brick.Widgets.List
import Brick.Widgets.Dialog
import Control.Lens
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import Data.Text (Text)
import qualified Data.Text.Lazy as LazyText
import Data.Text.Format
import Data.ProtoLens.TextFormat
import Proto.Google.Protobuf.Descriptor
import Proto.Google.Protobuf.Descriptor_Fields
import ProtoTui.AppState
import ProtoTui.Protoc
import ProtoTui.Search

draw :: AppState -> [Widget WName]
draw = \case
    Errored args err ->
      [ errorui err
      , viewErrorHelp args
      ]
    Running rs -> case rs ^. stateZoomed of
        Zoomed ->
          [ hBox
            [ vBox
              [ fdBodyViewPort $ rs
              , viewSearchForm $ rs
              ]
            ]
          ]
        UnZoomed ->
          [ hBox
            [ vBox
              [ fdBodyViewPort $ rs
              , viewSearchForm $ rs
              ]
            , vBox
              [ viewNestedMessagesList $ _stateFilteredMessagesList rs
              ]
            , vBox
              [ viewMethodsList $ _stateFilteredMethodsList rs
              , viewServicesList $ _stateFilteredServicesList rs
              , viewFilesList $ _stateFilteredFilesList rs
              ]
            ]
          ]

viewSearchForm :: RunningState -> Widget WName
viewSearchForm rs =
    txt "Search: " <+> (renderForm $ rs ^. stateCurrentSearch)

viewErrorHelp :: ProtocArguments -> Widget WName
viewErrorHelp args = viewport ErrorHelp Both
  $ txt "Could not loard proto-tui with protoc arguments:"
  <=> txt (LazyText.toStrict $ format "{}" (Only (Shown args)))
  <=> txt "If you can't find a solution."
  <=> txt "please consider asking for help at https://github.com/lucasdicioccio/proto-tui/issues ."
errorui :: AppError -> Widget WName
errorui err = renderDialog
  (dialog (Just "oh no!") Nothing 120)
  (str $ showError err)

showError :: AppError -> String
showError = \case
    ProtocIsNotFound ->
        "protoc not found on executables search PATH"
    ProtocCompileFailure n msg ->
        mconcat ["protoc compilation failed (" ++ show n  ++ ")", "\n", ByteString.unpack msg]
    ProtocResultParsingError msg ->
        mconcat ["protoc output could not be interpreted", "\n", msg]

viewWelcome :: Widget WName
viewWelcome = 
    txt "This is a preview of proto-tui"
  <=> txt "Soon we will improve navigation,"
  <=> txt "provide statistics,"
  <=> txt "also there will be a nice search syntax!"
  <=> txt "Usage:"
  <=> txt "- use TAB(Back-TAB) to cycle through panes"
  <=> txt "- use up/down to select in lists"
  <=> txt "- use hjkl to move this window"
  <=> txt "- use z to zoom/unzoom this window"
  <=> txt "- type in case-insentive search"

viewSearch :: Form Search AppEvent WName -> Widget WName
viewSearch frm = txt "Peforming a search:" <+> txt (formState frm ^. searchText)

fdBodyViewPort :: RunningState -> Widget WName
fdBodyViewPort rs =
    withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (txt viewportTitle)
  $ showCursor InitialView (Location (0, 0))
  $ viewport InitialView Both $ viewportContent
  where
    fds = _stateFileDescriptorSet rs
    (viewportTitle,viewportContent) = case focusGetCurrent (_stateFocus rs) of
        Nothing                -> ("", str "focus-on-something")
        (Just InitialView)     -> ("Welcome!", viewWelcome)
        (Just SearchField)     -> ("Search", viewSearch (rs ^. stateCurrentSearch))
        (Just FilesDigest)     -> ("Files", viewFilesViewport (rs ^. stateFilteredFilesList))
        (Just ServicesDigest)  -> ("Services", viewServicesViewport (rs ^. stateFilteredServicesList))
        (Just MessagesDigest)  -> ("Messages", viewMessagesViewport (rs ^. stateFilteredMessagesList))
        (Just MethodsDigest)   -> ("Methods", viewMethodsViewport (rs ^. stateFilteredMethodsList))

viewFieldsViewport :: List WName FieldDescriptorProto -> Widget WName
viewFieldsViewport lst = case listSelectedElement lst of
  Nothing -> str "pick an Field"
  (Just (_,fdp)) -> viewFieldDescriptorProto fdp

viewFieldDescriptorProto :: FieldDescriptorProto -> Widget WName
viewFieldDescriptorProto fdp =
    (txt $ fdp ^. name)
  <=> (str $ showMessage fdp)

viewServicesViewport :: List WName ServiceDescriptorProto -> Widget WName
viewServicesViewport lst = case listSelectedElement lst of
  Nothing -> str "pick an Service"
  (Just (_,fdp)) -> viewServiceDescriptorProto fdp

viewServiceDescriptorProto :: ServiceDescriptorProto -> Widget WName
viewServiceDescriptorProto fdp =
    (txt $ fdp ^. name)
  <=> (str $ showMessage fdp)

viewMethodsViewport :: List WName MethodDescriptorProto -> Widget WName
viewMethodsViewport lst = case listSelectedElement lst of
  Nothing -> str "pick an Method"
  (Just (_,fdp)) -> viewMethodDescriptorProto fdp

viewMethodDescriptorProto :: MethodDescriptorProto -> Widget WName
viewMethodDescriptorProto fdp =
    (txt $ fdp ^. name)
  <=> (str $ showMessage fdp)

viewMessagesViewport :: List WName DescriptorProto -> Widget WName
viewMessagesViewport lst = case listSelectedElement lst of
  Nothing -> str "pick an Message"
  (Just (_,fdp)) -> viewMessageDescriptorProto fdp

viewMessageDescriptorProto :: DescriptorProto -> Widget WName
viewMessageDescriptorProto fdp =
    (txt $ fdp ^. name)
  <=> (str $ showMessage fdp)

viewFilesViewport :: List WName FileDescriptorProto -> Widget WName
viewFilesViewport lst = case listSelectedElement lst of
  Nothing -> str "pick an File"
  (Just (_,fdp)) -> viewFileDescriptorProto fdp

viewFileDescriptorProto :: FileDescriptorProto -> Widget WName
viewFileDescriptorProto fdp =
    (txt $ fdp ^. name)
  <=> (str $ showMessage fdp)

viewFilesList :: List WName FileDescriptorProto -> Widget WName
viewFilesList lst = listdigests FilesDigest lst (\fdp -> fdp ^. name)

viewNestedMessagesList :: List WName DescriptorProto -> Widget WName
viewNestedMessagesList lst = listdigests MessagesDigest lst
    (\fdp -> LazyText.toStrict $
        format "{} ({} fields)" (
            (fdp ^. name)
          , (fdp ^. field . to length)
          )
     )

viewServicesList :: List WName ServiceDescriptorProto -> Widget WName
viewServicesList lst = listdigests ServicesDigest lst
    (\sdp -> LazyText.toStrict $
        format "{} ({} methods)" (
            (sdp ^. name)
          , (sdp ^. method . to length)
          )
     )

viewMethodsList :: List WName MethodDescriptorProto -> Widget WName
viewMethodsList lst = listdigests MethodsDigest lst (\mdp -> mdp ^. name)

listdigests
  :: WName
  -> List WName a
  -> (a -> Text)
  -> Widget WName
listdigests wname lst render =
    withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (txt $ LazyText.toStrict $ format "{} ({})" $ (Shown wname, length lst))
  $ renderList renderOne True lst
  where
    renderOne inFocus obj
      | inFocus   = txt $ render obj
      | otherwise = txt $ render obj
