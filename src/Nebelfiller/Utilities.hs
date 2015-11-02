-- Copyright (C) 2015 Moritz Schulte <mtesseract@silverratio.net>

module Nebelfiller.Utilities (listFst,
                              listSnd,
                              nonNull,
                              elemInListOfLists,
                              lengthAtLeast,
                              cartProd,
                              wrapCallback) where

import Utilities
import Control.Exception
import Wosa

listFst :: [(a,b)] -> [a]
listFst = map fst

listSnd :: [(a,b)] -> [b]
listSnd = map snd

nonNull :: [a] -> Bool
nonNull = not . null

elemInListOfLists :: (Eq a) => a -> [[a]] -> Bool
elemInListOfLists x lol = any (\ list -> x `elem` list) lol

lengthAtLeast :: Int -> [a] -> Bool
lengthAtLeast n list = length list >= n

wrapCallback :: IO () -> IO ()
wrapCallback callback =
  catch callback
        (\ e -> case (e :: WosaException) of
                  ExceptionString s -> putStrLn $ "Wosa Error: " ++ s
                  ExceptionNone     -> return ())
