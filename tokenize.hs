{-# LANGUAGE OverloadedStrings #-}

{- Split text into token for later analyze.
-}

import           Control.Monad                (forM_, when)
import           Data.Char                    (isAlpha, isDigit, isSpace)
import           Data.List                    (find)
import qualified Data.Map                     as M
import           Data.Text.Lazy               (Text)
import qualified Data.Text.Lazy               as T
import qualified Data.Text.Lazy.IO            as TIO
import           System.Console.GetOpt
import           System.Console.GetOpt.Simple
import           System.Environment           (getArgs, getProgName)
import           System.Exit                  (exitFailure)
import           System.IO                    (hPutStrLn, stderr)


optDebug = "debug"
optUnknown = "unknown"


main :: IO ()
main = do
  let options = [ Option ['d'] [optDebug] (noArg optDebug) "Dump original text before tokens."
                , Option ['u'] [optUnknown] (noArg optUnknown) "Show only Unknown tokens."
                ]
  (opts, args) <- getOptsArgs options [] ["file.txt"]

  let debug = M.member "debug" opts
  let unknown = if M.member "unknown" opts
                then \t -> case t of
                            Unknown _ -> True
                            _         -> False
                else const True

  case length args of
    1 -> do file <- TIO.readFile $ head args
            forM_ (filter garbageLine $ T.lines file) $ \l -> do
             when debug $ TIO.putStrLn l

             TIO.putStrLn
               $ T.intercalate ", "
               $ map (T.pack . show)
               $ filter unknown
               $ tokenize l

             when debug $ putStrLn ""


    _ -> showUsage [] []


garbageLine :: Text -> Bool
garbageLine = T.any isAlpha


tokenize :: Text -> [Part]
tokenize t =
  let (p, r) = readPart $ T.dropWhileEnd isSpace t
  in p : case r of
           Nothing   -> []
           Just rest -> tokenize rest


data Part = Word Text
          | Number Text
          | Punctuation Punctuation
          | Unknown Text


instance Show Part where
  show (Word w)        = "Word \"" ++ T.unpack w ++ "\""
  show (Number n)      = "Number \"" ++ T.unpack n ++ "\""
  show (Punctuation p) = show p
  show (Unknown t)     = "Unknown \"" ++ T.unpack t ++ "\""


data Punctuation = Comma -- запятая
                 | Point -- точка
                 | Colon -- двоеточие
                 | Dash -- тире
                 | Plus -- плюс
                 | Question -- вопросительный знак
                 | Exclamation -- восклицательный знак
                 | Quotation -- двойная кавычка
                 | QuotationOpen -- открывающая двойная кавычка
                 | QuotationClose -- закрывающая двойная кавычка
                 | Quote -- одинарная кавычка "'"
                 | Ellipsis -- многоточие (троеточие)
                 | Slash -- слэш (/)
                 | ParenOpen -- открывающая круглая скобка "("
                 | ParenClose -- закрывающая круглая скобка ")"
                 | Degree -- символ градуса "°"
                 | Percent -- символ процента "%"
                 deriving (Eq)


instance Show Punctuation where
  show p = case lookup p punctuation of
             Nothing -> "Unknown punctuation"
             Just s  -> "Punctuation \"" ++ [s] ++ "\""


punctuation :: [(Punctuation, Char)]
punctuation = [ (Comma, ',')
              , (Point, '.')
              , (Colon, ':')
              , (Dash, '-')
              , (Dash, '–')
              , (Dash, '—')
              , (Plus, '+')
              , (Question, '?')
              , (Exclamation, '!')
              , (Quotation, '"')
              , (QuotationOpen, '«')
              , (QuotationClose, '»')
              , (QuotationOpen, '„')
              , (QuotationClose, '“')
              , (Quote, '\'')
              , (Ellipsis, '…')
              , (Slash, '/')
              , (ParenOpen, '(')
              , (ParenClose, ')')
              , (Degree, '°')
              , (Percent, '%')
              ]

readPart :: Text -> (Part, Maybe Text)
readPart t =
  case T.head t' of
    c | isAlpha c -> let w = T.takeWhile isAlpha t'
                         rest = T.drop (T.length w) t'
                    in (Word w, if T.null rest then Nothing else Just rest)
    c | isDigit c -> let n = T.takeWhile isDigit t'
                         rest = T.drop (T.length n) t'
                    in (Number n, if T.null rest then Nothing else Just rest)
    c | c == '.' && T.take 3 t' == "..." -> (Punctuation Ellipsis, if T.null (T.drop 3 t') then Nothing else Just (T.drop 3 t'))

    c | isPunctuation c -> singleChar $ Punctuation $ findPunctuation c

    c -> let u = T.singleton c
             rest = T.drop (T.length u) t'
        in (Unknown u, if T.null rest then Nothing else Just rest)
  where
    isPunctuation ch =
      case find (\(p, c) -> c == ch) punctuation of
        Nothing -> False
        Just _  -> True
    t' = T.dropWhile isSpace t
    singleChar part =
      let rest = T.tail t'
      in (part, if T.null rest then Nothing else Just rest)
    findPunctuation :: Char -> Punctuation
    findPunctuation ch =
      case find (\(p, c) -> c == ch) punctuation of
        Just (p, c) -> p
        Nothing     -> error $ "Invalid punctuation \"" ++ show ch ++ "\""
