module GameActionParser (parseGameAction) where

import Control.Applicative ((<|>))
import Data.Maybe (listToMaybe)
import DataTypes
import Text.ParserCombinators.ReadP

parseGameAction :: String -> Maybe GameAction
parseGameAction = listToMaybe . map fst . readP_to_S gameAction

gameAction :: ReadP GameAction
gameAction = foldl1 (<|>) [playFromHand, activateFromField, announceDirectAttack, announceAttack, end, pass]

-- Parsers for each action

playFromHand :: ReadP GameAction
playFromHand = do
  string "p"
  i <- index
  return $ PlayFromHand i

activateFromField :: ReadP GameAction
activateFromField = do
  string "c"
  i <- index
  return $ ActivateFromField i

announceDirectAttack :: ReadP GameAction
announceDirectAttack = do
  string "d"
  i <- index
  return $ AnnounceDirectAttack i

announceAttack :: ReadP GameAction
announceAttack = do
  string "a "
  i1 <- index
  string " "
  i2 <- index
  return $ AnnounceAttack i1 i2

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
digit = satisfy (\char -> char >= '0' && char <= '9')

index :: ReadP Int
index = fmap (minus1 . read) (many1 digit)

minus1 :: Int -> Int
minus1 x = x - 1