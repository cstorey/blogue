{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Development.Shake
import Development.Shake.FilePath
import Data.Foldable


-- convert a source filepath to a build filepath
-- e.g. site/css/style.css -> build/css/style.css
srcToBuild :: FilePath -> FilePath
srcToBuild path = "build" </> dropDirectory1 path

main :: IO ()
main =
  shakeArgs shakeOptions $ do
    "build" ~> return ()
