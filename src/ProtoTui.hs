
module ProtoTui where

import Brick (App(..), defaultMain)
import Control.Monad (void)
import System.Environment (getArgs)

import ProtoTui.AppState (initProtoc)
import ProtoTui.Events (chooseCursor, handleEvent, startEvent)
import ProtoTui.Draw (draw)
import ProtoTui.Style (makeAttrMap)

runMain :: IO ()
runMain = do
    let app = App draw chooseCursor handleEvent startEvent makeAttrMap
    let initState = initProtoc =<< getArgs
    void $ defaultMain app =<< initState
