module Lib where

import Data.List

stickerWord = "indonesia" -- will be matched against "idea is one asia" -> 3 stickers
validChars = "indoesa"

stripSpaces :: String -> String
stripSpaces = concat . words

isInputValid :: String -> Bool
isInputValid input = checkValid (stripSpaces input) True where
  checkValid strippedInput lastResult
    | lastResult == False = False
    | lastResult == True = do
      case strippedInput of
        []     -> False
        [x]    -> isInfixOf [x] validChars
        (x:xs) -> checkValid xs $ isInfixOf [x] validChars

type Pair = (Char, Int)

findOccurrence :: String -> [Pair]
findOccurrence = map (\x -> (head x, length x)) . group . sort . stripSpaces

letterNeeded :: [Pair] -> [Pair] -> [Pair]
letterNeeded input predefined =
  [ (c, ceiling $ ( fromIntegral(n) / fromIntegral(pn) ))
    | (c, n)   <- input
    , (pc, pn) <- predefined
    , c == pc ]

findMaxInPair :: [Pair] -> Int
findMaxInPair = maximum . map (\x -> snd x)

countHowManyStickersNeeded :: String -> String -> Int
countHowManyStickersNeeded input pre = findMaxInPair $ letterNeeded x y where
  x = findOccurrence(input)
  y = findOccurrence(pre)
