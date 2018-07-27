module GameIO where

class Monad m => GameIO m where
    getLine :: m String
    log :: String -> m ()
    logLn :: String -> m ()
    chooseOne :: Show a => [a] -> m (Maybe a)

instance GameIO IO where
    getLine = Prelude.getLine
    log = putStr
    logLn = putStrLn
    chooseOne = chooseOneIO

chooseOneIO :: Show a => [a] -> IO (Maybe a)
chooseOneIO l = do
    displayEnumeratedItems l
    putStr "Choose one: "
    choice <- readLn
    if choice < 1 || choice > length l
        then return Nothing
        else return $ Just (l !! (choice -1))

displayEnumeratedItems :: (GameIO m, Show a) => [a] -> m ()
displayEnumeratedItems = mapM_ displayTuple . zip [1..]
    where displayTuple (i, v) = logLn $ show i ++ ": " ++ (show v)