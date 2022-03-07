module GameActionParser (parseGameAction) where

import Data.Char (isDigit)
import Data.Maybe (listToMaybe)
import DataTypes
import Text.ParserCombinators.ReadP
  ( ReadP,
    many1,
    readP_to_S,
    satisfy,
    string,
    choice
  )

parseGameAction :: String -> Maybe GameAction
parseGameAction = listToMaybe . map fst . readP_to_S gameAction

gameAction :: ReadP GameAction
gameAction = choice [playFromHand, activateFromField, announceDirectAttack, announceAttack, end, pass]

-- Parsers for each action

playFromHand :: ReadP GameAction
playFromHand = do
  string "p"
  PlayFromHand <$> index

activateFromField :: ReadP GameAction
activateFromField = do
  string "c"
  ActivateFromField <$> index

announceDirectAttack :: ReadP GameAction
announceDirectAttack = do
  string "d"
  AnnounceDirectAttack <$> index

announceAttack :: ReadP GameAction
announceAttack = do
  string "a "
  i <- index
  string " "
  AnnounceAttack i <$> index

pass :: ReadP GameAction
pass = do
  string "pass"
  return Pass

end :: ReadP GameAction
end = do
  string "end"
  return EndRound

-- helper methods

digit :: ReadP Char
digit = satisfy isDigit

number :: ReadP Int
number = read <$> many1 digit

index :: ReadP Int
index = minus1 <$> number

minus1 :: Int -> Int
minus1 x = x - 1