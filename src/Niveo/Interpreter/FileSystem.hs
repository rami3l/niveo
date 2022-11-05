module Niveo.Interpreter.FileSystem
  ( FileSystem (..),
    FsError (..),
    FSMap,
    readFile,
    writeFile,
    runFileSystemIO,
    runFileSystemPure,
  )
where

import Control.Exception (IOException)
import Control.Monad.Catch
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.String.Interpolate
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.State.Static.Local
import Effectful.TH
import Optics
import System.FilePath (joinPath, normalise, splitPath)
import System.IO qualified as IO
import Prelude hiding (evalState, gets, modify, readFile, writeFile)

-- See:
-- - <https://github.com/haskell-effectful/effectful/blob/0c66324baf95e5479c3b0bea4fea49f3ba41af22/effectful/examples/FileSystem.hs>.
-- - <https://hackage.haskell.org/package/effectful-th-1.0.0.0/docs/Effectful-TH.html>.

-- | An effect for reading and writing files.
type FileSystem :: (Type -> Type) -> Type -> Type
data FileSystem :: Effect where
  ReadFile :: FilePath -> FileSystem m String
  WriteFile :: FilePath -> String -> FileSystem m ()

makeEffect ''FileSystem

-- | File system error.
newtype FsError = FsError Text

-- | A `Map` from text file paths to their contents.
type FSMap = Map FilePath String

-- Handlers

runFileSystemIO ::
  [IOE, Error FsError] :>> es =>
  Eff (FileSystem : es) a ->
  Eff es a
runFileSystemIO = interpret $ const \case
  ReadFile path -> adapt $ IO.readFile path
  WriteFile path contents -> adapt $ IO.writeFile path contents
  where
    adapt m = liftIO m `catch` \(e :: IOException) -> throwError . FsError $ show e

runFileSystemPure ::
  Error FsError :> es =>
  FSMap ->
  Eff (FileSystem : es) a ->
  Eff es a
runFileSystemPure fs0 = reinterpret (evalState fs0) $ const \case
  ReadFile path ->
    let path' = joinPath . simplifyPath . splitPath . normalise $ path
     in gets (Map.!? path') >>= maybe (throwError $ FsError [i|file `#{path}` not found|]) pure
  WriteFile path contents -> modify $ Map.insert path contents
  where
    simplifyPath xs = let (lft, rgt) = xs & span isDot2 in lft <> simplifyPath' rgt
    simplifyPath' [] = []
    simplifyPath' xs@(_ : tl) =
      -- We assume that in this fake FS there are no symlinks, so the following equation holds:
      -- x/y/../z == x/z
      tl
        & List.findIndex isDot2
        -- Remove `y` at `idx` and `..` at `idx+1`.
        & maybe xs (\idx -> xs & toListOf (elements (`notElem` [idx, idx + 1])) & simplifyPath')
    isDot2 = (`elem` ["../" :: String, "..\\"])
