{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Library for the command line app.
module Lib where

import Control.Monad
import Data.Graph.Sparse
import Data.List qualified as L
import Data.List.Extra (nubSort, stripSuffix)
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Vector.Unboxed qualified as U
import Language.Haskell.Exts qualified as H
import Language.Haskell.TH (runIO)
import System.Directory (doesDirectoryExist, getCurrentDirectory, getDirectoryContents)
import System.Exit (exitFailure)

-- | Returns the root directory of the project.
installPath :: FilePath
installPath = $(do dir <- runIO getCurrentDirectory; [e|dir|])

-- | Converts root path (@/path/to/file@) into an absolute path.
rootPath :: FilePath -> FilePath
rootPath = (installPath ++)

-- | Converts module name to source file path.
modulePath :: String -> FilePath
modulePath name = concat [installPath, "/src/", map tr name, ".hs"]
  where
    tr '.' = '/'
    tr c = c

-- | Converts source file path to module name
moduleName :: FilePath -> Maybe String
moduleName name = do
  name1 <- L.stripPrefix (installPath ++ "/src/") name
  name2 <- stripSuffix ".hs" name1
  return $ map (\c -> if c == '/' then '.' else c) name2

