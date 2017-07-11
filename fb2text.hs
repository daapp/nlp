{-# LANGUAGE OverloadedStrings #-}

{- Extract text from FictionBook file.
-}

import           Control.Monad          (when)
import           Data.Char              (isSpace)
import           Data.List              (dropWhileEnd, intercalate)
import           System.Environment     (getArgs, getProgName)
import           System.Exit
import           System.IO              (hPutStrLn, stderr)
import           Text.HTML.TagSoup
import           Text.HTML.TagSoup.Tree


type FBGenre = String

data FBAuthor = FBAuthor
  { firstName  :: String
  , middleName :: String
  , lastName   :: String
  }

instance Show FBAuthor where
  show author = firstName author ++ " " ++ middleName author ++ " " ++ lastName author


data FBTitleInfo = FBTitleInfo
  { genres     :: [FBGenre]
  , authors    :: [FBAuthor]
  , bookTitle  :: String
  , annotation :: String
  , date       :: String
  , lang       :: String
  }

instance Show FBTitleInfo where
  show titleInfo = bookTitle titleInfo ++ "\n" ++
                   intercalate ", " (map show $ authors titleInfo)


data FBDescription = FBDescription
  { titleInfo :: FBTitleInfo
  }

instance Show FBDescription where
  show description = show (titleInfo description)


data FictionBook = FictionBook
  { description :: FBDescription
  , body        :: [String]
  }

instance Show FictionBook where
--  show book = (show $ description book) ++ "\n\n" ++ (intercalate "\n\n" $ body book)
  show book = intercalate "\n\n" $ body book


main :: IO ()
main = do
  args <- getArgs
  case length args of
    1 -> let showTitle = False
        in do fb2 <- getFb2 $ head args
              case fb2 of
                Left e      -> hPutStrLn stderr e >> exitFailure
                Right fbook -> showFictionBook fbook showTitle

    2 -> if head args == "-t"
        then do fb2 <- getFb2 $ args !! 1
                case fb2 of
                  Left e      -> hPutStrLn stderr e >> exitFailure
                  Right fbook -> showFictionBook fbook True
        else getProgName >>= usage

    _ -> getProgName >>= usage


showFictionBook :: FictionBook -> Bool -> IO ()
showFictionBook fictionBook showTitle = do
  when showTitle $
    putStrLn $ (show $ description fictionBook) ++ "\n\n"

  putStrLn $ intercalate "\n\n" $ body fictionBook


usage :: String -> IO ()
usage progName = do
  hPutStrLn stderr $ "Usage: " ++ progName ++ " ?-t? file.fb2"
  exitFailure


takeTagBranch :: String -> [TagTree String] -> Either String (TagTree String)
takeTagBranch tag tree =
  case [ x | x@(TagBranch t _ _) <- tree, t == tag] of
       (child:_) -> Right child
       _         -> Left $ "takeTagBranch: " ++ tag ++ " not found"


getFb2 :: String -> IO (Either String FictionBook)
getFb2 filename = do
  text <- readFile filename
  let tags = parseTree text
  case head tags of
    TagLeaf (TagOpen "?xml" _) ->
      case takeTagBranch "FictionBook" (tail tags) of
        Right fictionBook -> return $ parseFictionBook fictionBook
        Left e            -> return $ Left e
    _                          -> return $ Left "File is not a XML file."


parseFictionBook :: TagTree String -> Either String FictionBook
parseFictionBook branch =
  let TagBranch "FictionBook" _ children  = branch
  in case takeTagBranch "description" children of
       Right descriptionTree ->
         case parseDescription descriptionTree of
           Right description' ->
             case takeTagBranch "body" children of
               Right bodyTree -> Right FictionBook { description = description'
                                                  , body = parseBody bodyTree
                                                  }
               Left e         -> Left e
           Left e             -> Left e
       Left e            -> Left e


parseBody :: TagTree String -> [String]
parseBody branch =
  [ t | (TagText t ) <- flattenTree [branch], not $ null $ trim t ]


parseDescription :: TagTree String -> Either String FBDescription
parseDescription branch =
  let TagBranch "description" _ children = branch
  in case takeTagBranch "title-info" children of
       Right titleInfoTree ->
         let titleInfo = parseTitleInfo titleInfoTree
         in Right FBDescription { titleInfo = titleInfo }
       Left e -> Left e


parseTitleInfo :: TagTree String -> FBTitleInfo
parseTitleInfo branch =
    let TagBranch "title-info" _ children =  branch
    in foldl (\ti tag ->
                 case tag of
                   TagBranch "book-title" _ [TagLeaf (TagText t)] ->
                     ti { bookTitle = trim t }

                   TagBranch "lang" _ [TagLeaf (TagText lang)] ->
                     ti { lang = trim lang }

                   TagBranch "genre" _ [TagLeaf (TagText g)] ->
                     ti { genres = trim g : genres ti }

                   TagBranch "author" _ authorTags ->
                     ti { authors = parseAuthor authorTags : authors ti }

                   _ -> ti
             ) (FBTitleInfo
                { genres = []
                , authors = []
                , bookTitle = ""
                , annotation = ""
                , date = ""
                , lang = ""
                }) children


parseAuthor :: [TagTree String] -> FBAuthor
parseAuthor =
  foldl (\author tag ->
           case tag of
             TagBranch "first-name" _ [TagLeaf (TagText firstName)] ->
               author { firstName = trim firstName }
             TagBranch "middle-name" _ [TagLeaf (TagText middleName)] ->
               author { middleName = trim middleName }
             TagBranch "last-name" _ [TagLeaf (TagText lastName)] ->
               author { lastName = trim lastName }
             _ -> author
             ) (FBAuthor "" "" "")


trim :: String -> String
trim s = dropWhileEnd isSpace $ dropWhile isSpace s
