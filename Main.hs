import System.Random
import Data.List

data GameState = GameState { gameWord :: String }  deriving (Show)

wordList :: [String]
wordList = [ "donkey"
           , "brush"
           , "rain"
           , "fall"
           , "sadistic"
           , "reversal"
           , "bacteria"
           , "blackwater"
           , "public"
           , "canyon"
           , "ego"
           , "distancing"
           , "boutique"]

randomWord :: IO String
randomWord = do
    wordIndex <- randomRIO (0, (length wordList) - 1)
    return (wordList !! wordIndex)

initializeGame :: String -> GameState
initializeGame word = GameState { gameWord = word }

main :: IO ()
main = do
    word <- randomWord
    let game = initializeGame word
    putStrLn $ show game
