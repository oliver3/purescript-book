module FileOperations where

import Prelude
import Control.MonadPlus (guard)
import Data.Maybe (Maybe)

import Data.Path (Path, filename, isDirectory, ls, root)
import Data.Array (head, concatMap, (:))

allFiles :: Path -> Array Path
allFiles root = root : concatMap allFiles (ls root)

allFiles' :: Path -> Array Path
allFiles' file = file : do
  child <- ls file
  allFiles' child

onlyFiles :: Path -> Array Path
onlyFiles file = if isDirectory file
  then concatMap onlyFiles (ls file)
  else [file]

onlyDirs :: Path -> Array Path
onlyDirs file = if isDirectory file
  then file : concatMap onlyDirs (ls file)
  else []

-- > whereIs "/bin/ls"
--  Just (/bin/)

--  > whereIs "/bin/cat"
--  Nothing

whereIs :: String -> Maybe Path
whereIs name = head $ do
  dir <- onlyDirs root
  file <- ls dir
  guard (filename file == name)
  pure dir
