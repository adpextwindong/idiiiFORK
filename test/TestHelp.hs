module TestHelp (testFile, getValueForTextFrame, assertTagHasGivenFrames,
                 assertTagHasGivenTextValues)
  where

import Paths_idiii (getDataFileName)
import Test.HUnit

import qualified Data.Map as Map
import ID3 (ID3Tag, ID3Frame(..), readTag, tagFrames)
import ID3.Type.FrameInfo (FrameInfo(Text, COMM, WXXX))

testFile = getDataFileName

assertTagHasGivenFrames :: String -> [(String, FrameInfo)] -> Assertion
assertTagHasGivenFrames fname expectedFrames = do
  fpath <- testFile fname
  maybeTag <- readTag fpath
  case maybeTag of
    Nothing -> fail "Tag should have been read from file"
    Just tag -> mapM_
      (\(n, fi) -> assertEqual ("Should get expected frame info for '" ++ n ++ "' frame")
                     (Just fi) (getFrameInfo tag n)) expectedFrames

assertTagHasGivenTextValues :: String -> [(String, String)] -> Assertion
assertTagHasGivenTextValues fname expectedValues = do
  fpath <- testFile fname
  maybeTag <- readTag fpath
  case maybeTag of
    Nothing -> fail "Tag should have been read from file"
    Just tag -> mapM_
      (\(n, v) -> assertEqual ("Should get expected value for '" ++ n ++ "' frame")
                              (Just v) (getValueForTextFrame tag n)) expectedValues

getValueForTextFrame :: ID3Tag -> String -> Maybe String
getValueForTextFrame tag frameId = case getFrameInfo tag frameId of
  Nothing -> Nothing
  Just info -> Just $ getFrameText info
    where getFrameText (Text _ t) = t
          getFrameText (COMM _ _ _ t) = t
          getFrameText (WXXX _ _ t) = t

getFrameInfo :: ID3Tag -> String -> Maybe FrameInfo
getFrameInfo tag frameId = case Map.lookup frameId (tagFrames tag) of
  Nothing -> Nothing
  Just f -> Just $ frInfo_ f

failTest = assertFailure
