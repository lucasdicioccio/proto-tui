{-# LANGUAGE LambdaCase #-}

module ProtoTui.Events where

import Brick
import Brick.Focus
import Brick.Forms
import Brick.Widgets.List
import Control.Lens
import qualified Graphics.Vty as Vty
import ProtoTui.AppState

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
