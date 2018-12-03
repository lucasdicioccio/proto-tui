{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module ProtoTui.AppState where

import Brick.Focus
import Brick.Forms
import Brick.Widgets.List
import Control.Lens ((^.), (^..), (^?), traversed, cosmosOf)
import Control.Lens.TH (makeLenses,makePrisms)
import Data.ByteString.Char8 (ByteString)
import Data.ProtoLens.Encoding (decodeMessage, decodeMessageOrDie)
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Vector as Vector
import System.Exit (ExitCode(..))
import System.Process.ByteString (readCreateProcessWithExitCode)
import System.Process (proc)

import Proto.Google.Protobuf.Descriptor
import Proto.Google.Protobuf.Descriptor_Fields

import ProtoTui.Search
import ProtoTui.Protoc

-- | Application widgets names.
data WName
  = InitialView
  | FilesDigest -- ^ a digest showing per-file statistics
  | MessagesDigest -- ^ a digest showing per-messages statistics
  | ServicesDigest -- ^ a digest showing per-service statistics
  | MethodsDigest -- ^ a digest showing per-method statistics
  | SearchField
  | ErrorHelp
  deriving (Show, Eq, Ord)

-- | Application errooooooooooooooooored.
data AppError =
    ProtocIsNotFound
  | ProtocCompileFailure Int ByteString
  | ProtocResultParsingError String

type AppEvent = ()

data Zoomed = UnZoomed | Zoomed

toggleZoomed :: Zoomed -> Zoomed
toggleZoomed UnZoomed = Zoomed
toggleZoomed Zoomed   = UnZoomed

data RunningState = RunningState {
    _stateProtocArgs        :: !ProtocArguments
  , _stateFileDescriptorSet :: !FileDescriptorSet
  , _stateCurrentSearch     :: Form Search AppEvent WName
  , _stateZoomed            :: !Zoomed
  -- filter results
  , _stateFilteredFilesList     :: List WName FileDescriptorProto
  , _stateFilteredServicesList  :: List WName ServiceDescriptorProto
  , _stateFilteredMethodsList   :: List WName MethodDescriptorProto
  , _stateFilteredMessagesList  :: List WName DescriptorProto
  -- proper interactions
  , _stateFocus                 :: FocusRing WName
  }
makeLenses ''RunningState

-- | The application state.
data AppState =
    Errored !ProtocArguments !AppError
  | Running !RunningState
makePrisms ''AppState

getFocus :: AppState -> Maybe WName
getFocus s0 =
    s0 ^? _Running . stateFocus >>= focusGetCurrent

-- | Loads the initial state from Protoc arguments.
initProtoc :: ProtocArguments -> IO AppState
initProtoc args = do
    mProtocPath <- findProtoc
    case mProtocPath of
      Nothing ->
          pure $ Errored args ProtocIsNotFound
      Just protocPath -> do
          let createProtocProcess = proc protocPath ("-o/dev/stdout" : args)
          (!code,!bs,!err) <- readCreateProcessWithExitCode createProtocProcess ""
          case code of
              ExitFailure n -> pure $ Errored args $ ProtocCompileFailure n err
              ExitSuccess -> case decodeMessage bs of
                  Right fds -> pure $ initialState args fds
                  Left decodeErr -> pure $ Errored args $ ProtocResultParsingError decodeErr

initialState :: ProtocArguments -> FileDescriptorSet -> AppState
initialState args fds =
    makeState args fds (Search "") defaultFocus
  where
    defaultFocus =
        focusRing [ InitialView
                  , SearchField
                  , MessagesDigest
                  , MethodsDigest
                  , ServicesDigest
                  , FilesDigest
                  ]

makeState
  :: ProtocArguments
  -> FileDescriptorSet
  -> Search
  -> FocusRing WName
  -> AppState
makeState args fds searchStr focus =
    Running $ RunningState {..}
  where
    _stateProtocArgs = args
    _stateFileDescriptorSet = fds
    _stateZoomed = UnZoomed
    _stateCurrentSearch =
        newForm [ editTextField searchText SearchField (Just 1) ] searchStr
    _stateFocus = focus
    _stateFilteredMethodsList =
        let allObjs = fds ^.. file . traversed . service . traversed . method . traversed
        in list MethodsDigest (Vector.fromList $ filter (match searchStr) allObjs) 1
    _stateFilteredServicesList =
        let allObjs = fds ^.. file . traversed . service . traversed
        in list ServicesDigest (Vector.fromList $ filter (match searchStr) allObjs) 1
    _stateFilteredFilesList =
        let allObjs = fds ^.. file . traversed
        in list FilesDigest (Vector.fromList $ filter (match searchStr) allObjs) 1
    _stateFilteredMessagesList =
        let allObjs = (fds ^.. file . traversed . messageType . traversed . cosmosOf (nestedType . traversed))
        in list MessagesDigest (Vector.fromList $ filter (match searchStr) allObjs) 1

updateSearch :: RunningState -> Search -> AppState
updateSearch s0 search = makeState
    (s0 ^. stateProtocArgs)
    (s0 ^. stateFileDescriptorSet)
    search
    (s0 ^. stateFocus)

