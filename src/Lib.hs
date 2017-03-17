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

letterNeeded :: [Pair] -> [Pair] -> Int
letterNeeded input predefined =
  maximum [ ceiling $ fromIntegral(letterCount) / fromIntegral(preLetterCount)
    | (letter, letterCount)       <- input
    , (preLetter, preLetterCount) <- predefined
    , letter == preLetter ]

countHowManyStickersNeeded :: String -> String -> Int
countHowManyStickersNeeded input pre = letterNeeded x y where
  x = findOccurrence(input)
  y = findOccurrence(pre)
