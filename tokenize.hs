{-# LANGUAGE OverloadedStrings #-}

{- Split text into token for later analyze.

Punctuation file is text file. Format is:
Name Character

where Name is name of punctuation symbol and Character is single character.

-}

import           Control.Applicative          ((<$>))
import           Control.Monad                (forM_, liftM, when)
import           Data.Char                    (isAlpha, isDigit, isSpace)
import           Data.List                    (find, sortBy)
import qualified Data.Map                     as M
import           Data.Text.Lazy               (Text)
import qualified Data.Text.Lazy               as T
import qualified Data.Text.Lazy.IO            as TIO
import           System.Console.GetOpt
import           System.Console.GetOpt.Simple
import           System.Environment           (getArgs, getProgName)
import           System.Exit                  (exitFailure)
import           System.IO                    (hPutStrLn, stderr)


type Punctuation = Text
type PunctuationTable = [(Punctuation, Char)]


optDebug = "debug"
optUnknown = "unknown"
optPunctuation = "punctuation"


main :: IO ()
main = do
  let options = [ Option ['d'] [optDebug] (noArg optDebug) "Dump original text before tokens."
                , Option ['u'] [optUnknown] (noArg optUnknown) "Show only Unknown tokens."
                , Option ['p'] [optPunctuation] (arg optPunctuation) "Punctuation table file."
                ]
  (opts, args) <- getOptsArgs options [] ["file.txt"]

  let debug = M.member optDebug opts
      unknown = if M.member optUnknown opts
                then \t -> case t of
                            Unknown _ -> True
                            _         -> False
                else const True

  punctuationTable <- case M.lookup optPunctuation opts of
                       Nothing -> return []
                       Just filename ->
                         (map (\[a, b] -> (a, T.head b) ) .
                          sortBy (\[ak, av] [bk, bv] -> ak `compare` bk) .
                          filter (\ws -> length ws == 2) .
                          map T.words .
                          T.lines
                         ) <$> TIO.readFile filename

  when debug $ do
    putStrLn "Punctuation table:"
    forM_ punctuationTable $ \p -> TIO.putStrLn $ T.concat [fst p, " ", T.singleton $ snd p]
    putStrLn ""

  case length args of
    1 -> do file <- TIO.readFile $ head args
            forM_ (filter garbageLine $ T.lines file) $ \l -> do
             when debug $ TIO.putStrLn l

             TIO.putStrLn
               $ T.intercalate ", "
               $ map (T.pack . show)
               $ filter unknown
               $ tokenize punctuationTable l

             when debug $ putStrLn ""

    _ -> showUsage [] []


garbageLine :: Text -> Bool
garbageLine = T.any isAlpha


tokenize :: PunctuationTable -> Text -> [Part]
tokenize punctuationTable t =
  let (p, r) = readPart punctuationTable $ T.dropWhileEnd isSpace t
  in p : case r of
           Nothing   -> []
           Just rest -> tokenize punctuationTable rest


data Part = Word Text
          | Number Text
          | Punctuation Punctuation
          | Unknown Text


instance Show Part where
  show (Word w)        = "Word \"" ++ T.unpack w ++ "\""
  show (Number n)      = "Number \"" ++ T.unpack n ++ "\""
  show (Punctuation p) = "Punctuation " ++ T.unpack p
  show (Unknown t)     = "Unknown \"" ++ T.unpack t ++ "\""


readPart :: PunctuationTable -> Text -> (Part, Maybe Text)
readPart punctuationTable t =
  case T.head t' of
    c | isAlpha c -> (Word w, theRest rest)
      where w = T.takeWhile isAlpha t'
            rest = T.drop (T.length w) t'

    c | isDigit c -> (Number n, theRest rest)
      where n = T.takeWhile isDigit t'
            rest = T.drop (T.length n) t'

    c | isPunctuation c -> (findPunctuation c, theRest $ T.tail t')

    c -> (Unknown $ T.singleton c, theRest $ T.tail t')

  where
    isPunctuation :: Char -> Bool
    isPunctuation ch = any ((ch ==) . snd) punctuationTable

    t' = T.dropWhile isSpace t

    theRest rest = if T.null rest then Nothing else Just rest

    findPunctuation :: Char -> Part
    findPunctuation ch =
      case find (\(_, c) -> c == ch) punctuationTable of
        Just (p, _) -> Punctuation p
        Nothing     -> Unknown $ T.singleton ch
