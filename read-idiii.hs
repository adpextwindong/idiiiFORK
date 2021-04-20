module Main where

import ID3
import qualified Data.Map as Map
import System.Environment (getArgs)

main = do
  args <- getArgs
  let f = head args
  maybeTag <- readTag f
  case maybeTag of
    Nothing -> putStrLn "File has no tag."
    Just tag -> do
      putStrLn $ "Here's the header: " ++ (show $ tagHeader tag)
      putStrLn $ "Here are the frames: "
      let lines = Map.elems $ Map.mapWithKey (\key val -> "  " ++ (show key) ++ ": " ++ (show $ frInfo_ val))
                                             (tagFrames tag)
      mapM_ putStrLn lines
