module Lib where

import Data.List

stickerWord = "indonesia" -- will be matched against "idea is one asia" -> 3 stickers
validChars = "indoesa"

stripSpaces :: String -> String
stripSpaces = concat . words

-- Needs improvement to immediately break after encountering False
isInputValid :: String -> Bool
isInputValid input = case stripSpaces input of
  []     -> False
  [x]    -> isInfixOf [x] validChars
  (x:xs) -> isInfixOf [x] validChars && isInputValid xs

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
