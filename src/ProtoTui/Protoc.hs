{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module ProtoTui.Protoc where

import Data.ProtoLens.Encoding (decodeMessage, decodeMessageOrDie)
import System.Directory (findExecutable)
import System.Process.ByteString (readCreateProcessWithExitCode)
import System.Process (proc)
import Proto.Google.Protobuf.Descriptor


type ProtocArguments = [String]

-- | Compiles a proto to a file-descriptor-set.
--
-- This function is useful when someone wants to use GHCi to explore protos
-- directly.
unsafeProtoc :: ProtocArguments -> IO FileDescriptorSet
unsafeProtoc args = do
    let createProtocProcess = proc "protoc" ("-o/dev/stdout" : args)
    (!_,!bs,!_) <- readCreateProcessWithExitCode createProtocProcess ""
    pure $ decodeMessageOrDie bs

-- | Looks for protoc on the PATH.
findProtoc :: IO (Maybe FilePath)
findProtoc = findExecutable "protoc"
