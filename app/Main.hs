module Main where

import Data.List
import Lib

main :: IO ()
main = do
  putStr "Enter a sentence for your sticker: "
  userSentence <- getLine
  if isInputValid userSentence
    then putStrLn $ "You need " ++ show (countHowManyStickersNeeded userSentence stickerWord) ++
      " \"indonesia\" stickers to print \"" ++ userSentence ++ "\""
    else main
