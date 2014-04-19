{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Attoparsec.Combinator
import Data.Maybe
import qualified Data.Text as T

data RequestMember = RequestMemberAll | RequestMemberText T.Text
data ImportRequest = RequestRef T.Text | RequestType T.Text [RequestMember] 
data ImportLine = ImportRequestLine ImportRequest | ImportComment | ImportBlank
data ImportFile = ImportFile [ImportRequest]

parseMemAll :: Parser RequestMember
parseMemAll = do
    char '*'
    return RequestMemberAll

parseMemSpecific :: Parser RequestMember
parseMemSpecific = do
    t <- takeWhile1 (not . isHorizontalSpace)
    return $ RequestMemberText t

parseMember :: Parser RequestMember
parseMember = parseMemAll <|> parseMemSpecific

parseComment :: Parser ImportLine
parseComment = do
    string "#" <|> string "--"
    return ImportComment

parseRef :: Parser ImportRequest
parseRef = do
    string "reference"
    some space
    t <- takeText
    return $ RequestRef t

parseType :: Parser ImportRequest
parseType = do
    t <- takeTill isHorizontalSpace
    many space
    mems <- sepBy1 parseMember (many space)
    return $ RequestType t mems
        
parseImportRequest :: Parser ImportRequest
parseImportRequest = parseRef <|> parseType

parseImportRequestLine :: Parser ImportLine
parseImportRequestLine = do
    r <- parseImportRequest
    return $ ImportRequestLine r

parseBlank :: Parser ImportLine
parseBlank = do
    many space
    endOfInput
    return ImportBlank

parseImportLine :: Parser ImportLine
parseImportLine = do
    many space
    parseBlank <|> parseComment <|> parseImportRequestLine

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

