{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Prelude hiding (takeWhile)
import Control.Applicative
import Data.Attoparsec.Text
import Data.Char (isDigit, isLetter)
import Data.List (intercalate, find)
import Data.Maybe (catMaybes)
import qualified Data.Text as T

data RequestMember = RequestMemberAll | RequestMemberSpecific T.Text
data ImportRequest = RequestRef T.Text | RequestType T.Text [RequestMember] 
data ImportLine    = ImportRequestLine ImportRequest | ImportComment T.Text | ImportBlank
data ImportFile    = ImportFile [ImportRequest]


instance Show RequestMember where
  show RequestMemberAll = "*"
  show (RequestMemberSpecific t) = "<" ++ T.unpack t ++ ">"

instance Show ImportRequest where
  show (RequestRef t) = "reference <" ++ T.unpack t ++ ">"
  show (RequestType t xs) = "type <" ++ T.unpack t ++ "> members: [" ++ intercalate  "," (map show xs) ++ "]"

instance Show ImportLine where
  show (ImportRequestLine x) = show x
  show (ImportComment x) = "Comment --" ++ T.unpack x
  show ImportBlank = "Blank"

instance Show ImportFile where
  show (ImportFile xs) = intercalate "\n" $ map show xs

manySpace = many $ satisfy isHorizontalSpace

parseMemAll :: Parser RequestMember
parseMemAll = do
    char '*'
    return RequestMemberAll

parseMemSpecific :: Parser RequestMember
parseMemSpecific = do
    t <- takeWhile1 (\c-> isDigit c || isLetter c)
    return $ RequestMemberSpecific t

parseMember :: Parser RequestMember
parseMember = parseMemAll <|> parseMemSpecific

parseComment :: Parser ImportLine
parseComment = do
    string "#" <|> string "--"
    t <- takeWhile (\c-> not $ isEndOfLine c)
    return $ ImportComment t

parseRef :: Parser ImportRequest
parseRef = do
    string "reference"
    some space
    t <- takeTill isEndOfLine
    return $ RequestRef $ T.strip t

parseType :: Parser ImportRequest
parseType = do
    t <- takeWhile1 (\c-> isDigit c || isLetter c || c == '.')  
    manySpace
    option () $ skip $ (==) ':'
    manySpace
    mems <- sepBy parseMember (manySpace >> char ',' >> manySpace)
    if length mems > 1 then do
      case find matchMemAll mems of
        (Just _) -> fail "Members should either be a comma seperated list or a single '*' but not both"
        Nothing -> return ()
    else return ()
    manySpace
    return $ RequestType t mems
    where matchMemAll :: RequestMember -> Bool
          matchMemAll RequestMemberAll = True
          matchMemAll _ = False
        
parseImportRequest :: Parser ImportRequest
parseImportRequest = parseRef <|> parseType

parseImportRequestLine :: Parser ImportLine
parseImportRequestLine = do
    r <- parseImportRequest
    return $ ImportRequestLine r

parseBlank :: Parser ImportLine
parseBlank = do
    skipWhile (\c-> not $ isEndOfLine c)
    return ImportBlank

parseImportLine :: Parser ImportLine
parseImportLine = do
    manySpace
    r <- parseComment <|> parseImportRequestLine <|> parseBlank
    return r

importLineAsImportRequest :: ImportLine -> Maybe ImportRequest
importLineAsImportRequest l = case l of
    ImportRequestLine r -> Just r
    _ -> Nothing

importLinesToImportRequests :: [ImportLine] -> [ImportRequest]
importLinesToImportRequests x = catMaybes $ map importLineAsImportRequest x

parseImportFile :: Parser ImportFile
parseImportFile = do
    linesOfFile <- sepBy parseImportLine endOfLine
    let importRequestLines = importLinesToImportRequests linesOfFile
    return $ ImportFile importRequestLines

