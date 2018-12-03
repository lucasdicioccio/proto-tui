{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module ProtoTui where

import Brick
import Brick.Focus
import Brick.Widgets.List
import Brick.Widgets.Dialog
import Brick.Forms
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS

import Control.Monad (void)

import Control.Lens.TH (makeLenses)

import Data.String (IsString)

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as ByteString

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Vector as Vector
import Data.Text.Format

import Control.Lens
import Data.ProtoLens.TextFormat
import Proto.Google.Protobuf.Descriptor
import Proto.Google.Protobuf.Descriptor_Fields

import qualified Graphics.Vty as Vty
import qualified Graphics.Vty.Attributes as Vty
import System.Environment (getArgs)

import ProtoTui.Search
import ProtoTui.AppState
import ProtoTui.Protoc


runMain :: IO ()
runMain = do
    let app = App draw chooseCursor handleEvent startEvent makeAttrMap
    let initState = initProtoc =<< getArgs
    void $ defaultMain app =<< initState

handleEvent :: AppState -> BrickEvent WName AppEvent -> EventM WName (Next AppState)
handleEvent s0 = \case
    -- | Quit on ESCAPE.
    VtyEvent (Vty.EvKey Vty.KEsc _)
      -> halt s0
    -- | Switch focus on TAB.
    VtyEvent (Vty.EvKey (Vty.KChar '\t') _)
      -> continue $ over _Running (stateFocus %~ focusNext) s0
    VtyEvent (Vty.EvKey (Vty.KBackTab) _)
      -> continue $ over _Running (stateFocus %~ focusPrev) s0
    -- | Zoom toggle.
    VtyEvent (Vty.EvKey (Vty.KChar 'z') _)
      -> continue $ over _Running (stateZoomed %~ toggleZoomed) s0
    -- | General event
    ev@(VtyEvent vtyEv)
      -> case getFocus s0 of
             Nothing       -> halt s0
             Just ErrorHelp  -> halt s0
             Just InitialView -> handleViewPort s0 ev
             Just SearchField -> do
                 sf <- handleFormEvent ev (s0 ^?! _Running . stateCurrentSearch)
                 let s1 = updateSearch (s0 ^?! _Running) (formState sf)
                 handleViewPort s1 ev
             Just FilesDigest -> do
                 fl <- handleListEvent vtyEv (s0 ^?! _Running . stateFilteredFilesList)
                 let s1 = s0 & _Running . stateFilteredFilesList .~ fl
                 handleViewPort s1 ev
             Just MessagesDigest -> do
                 fl <- handleListEvent vtyEv (s0 ^?! _Running . stateFilteredMessagesList)
                 let s1 = s0 & _Running . stateFilteredMessagesList .~ fl
                 handleViewPort s1 ev
             Just ServicesDigest -> do
                 fl <- handleListEvent vtyEv (s0 ^?! _Running . stateFilteredServicesList)
                 let s1 = s0 & _Running . stateFilteredServicesList .~ fl
                 handleViewPort s1 ev
             Just MethodsDigest -> do
                 fl <- handleListEvent vtyEv (s0 ^?! _Running . stateFilteredMethodsList)
                 let s1 = s0 & _Running . stateFilteredMethodsList .~ fl
                 handleViewPort s1 ev

handleViewPort :: AppState -> BrickEvent WName e -> EventM WName (Next AppState)
handleViewPort s0 = \case
    VtyEvent (Vty.EvKey (Vty.KChar 'k') _)
      -> do
          let vp = viewportScroll InitialView
          vScrollBy vp (-1)
          continue s0
    VtyEvent (Vty.EvKey (Vty.KChar 'j') _)
      -> do
          let vp = viewportScroll InitialView
          vScrollBy vp 1
          continue s0
    VtyEvent (Vty.EvKey (Vty.KChar 'h') _)
      -> do
          let vp = viewportScroll InitialView
          hScrollBy vp (-1)
          continue s0
    VtyEvent (Vty.EvKey (Vty.KChar 'l') _)
      -> do
          let vp = viewportScroll InitialView
          hScrollBy vp 1
          continue s0
    VtyEvent (Vty.EvKey Vty.KPageUp _)
      -> do
          let vp = viewportScroll InitialView
          vScrollPage vp Up
          continue s0
    VtyEvent (Vty.EvKey Vty.KPageDown _)
      -> do
          let vp = viewportScroll InitialView
          vScrollPage vp Down
          continue s0
    VtyEvent (Vty.EvKey Vty.KBS _)
      -> continue $ over _Running (stateFocus %~ focusSetCurrent SearchField) s0
    ev
      -> continue s0

startEvent :: AppState -> EventM n AppState
startEvent = pure

chooseCursor :: AppState -> [CursorLocation WName] -> Maybe (CursorLocation WName)
chooseCursor s0 xs = getFocus s0 >>= flip showCursorNamed xs

makeAttrMap :: AppState -> AttrMap
makeAttrMap _ = attrMap Vty.defAttr attrs
  where
    attrs = [ (listAttr, Vty.blue `on` Vty.white)
            , (listSelectedAttr, Vty.blue `on` Vty.white)
            , (listSelectedFocusedAttr, Vty.white `on` Vty.blue)
            ]

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

--------------------

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

--------------------

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
