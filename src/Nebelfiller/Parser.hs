-- Copyright (C) 2015 Moritz Schulte <mtesseract@silverratio.net>

module Nebelfiller.Parser where

import Nebelfiller.Datatypes
import Data.Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

parseListOne :: String -> IO (Either String [ItemPair])
parseListOne file = do
  csvData <- BL.readFile file
  let csvParsed = decode NoHeader csvData :: Either String (V.Vector (String, String, String, String))
  case csvParsed of
    Left err -> return $ Left err
    Right v -> return $ Right $ V.toList $ V.map (\ (a, b, c, d) -> ItemPair (Item a b) (Item c d)) v

parseListTwo :: String -> IO (Either String [Item])
parseListTwo file = do
  csvData <- BL.readFile file
  let csvParsed = decode NoHeader csvData :: Either String (V.Vector (String, String))
  case csvParsed of
    Left err -> return $ Left err
    Right v -> return $ Right $ V.toList $ V.map (uncurry Item) v
