{-# LANGUAGE OverloadedStrings #-}

{- Pretty print XML file.
-}

import qualified Data.Text.Lazy     as T
import qualified Data.Text.Lazy.IO  as TIO
import           System.Environment (getArgs, getProgName)
import           System.Exit        (exitFailure)
import           System.IO          (hPutStrLn, stderr)
import           Text.XML
import qualified Text.XML           as X

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> do content <- TIO.getContents
            case parseText def content of
              Right doc -> TIO.putStrLn $ renderText def {rsPretty = True} doc
              Left e    -> hPutStrLn stderr $ "Error: " ++ show e
    1 -> do doc <- X.readFile def $ head args
            TIO.putStrLn $ renderText def {rsPretty = True} doc
    _ -> do progName <- getProgName
            hPutStrLn stderr $ "Usage: " ++ progName ++ " ?file.xml?"
            exitFailure
