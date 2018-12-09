
module ProtoTui.Style where

import Brick (AttrMap, attrMap, on)
import Brick.Widgets.List (listAttr, listSelectedAttr, listSelectedFocusedAttr)
import ProtoTui.AppState (AppState)
import qualified Graphics.Vty.Attributes as Vty

makeAttrMap :: AppState -> AttrMap
makeAttrMap _ = attrMap Vty.defAttr attrs
  where
    attrs = [ (listAttr, Vty.blue `on` Vty.white)
            , (listSelectedAttr, Vty.blue `on` Vty.white)
            , (listSelectedFocusedAttr, Vty.white `on` Vty.blue)
            ]
