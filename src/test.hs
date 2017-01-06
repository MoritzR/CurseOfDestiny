import Control.Monad.State.Lazy

data Card = Card String
    deriving Show
data Player = Player {
    deck :: [Card],
    hand :: [Card]
} deriving Show

data Game = Game {
    players :: [Player],
    activePlayer :: Int
} deriving Show

getPlayers :: Game -> [Player]
getPlayers g = players g

getActivePlayer :: Game -> Player
getActivePlayer g = (players g) !! (activePlayer g)

getNextPlayerIndex :: Game -> Int
getNextPlayerIndex g = (-) ((+) (-1) $ length $ players g) (activePlayer g)

endRound :: Game -> Game
endRound g = Game (players g) (getNextPlayerIndex g)

pass :: Game -> Game
pass g = g

actionStringToFunction :: String -> (Game -> Game)
actionStringToFunction "end" = endRound
actionStringToFunction "pass" = pass


player1 = Player [] [Card "test"]
player2 = Player [] [Card "test2"]
game = Game [player1,player2] 0

type Cod game = State game

main =  do
    putStr "Select action (pass/end): "
    inp <- getLine
    if inp=="exit" || inp=="q"
        then putStrLn "bye"
        else do 
            let newG = actionStringToFunction inp game
            putStrLn $ show $ newG
            main
