import System.Random
import Data.List
import Control.Monad.Loops
import GameState

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

gameLoop :: GameState -> IO GameState
gameLoop g = do
    putStrLn  "==========================================="
    putStrLn $ "word: " ++ playerGuess g
    putStrLn "Guess a character: "
    guess <- getLine
    return $ updateGame g (head guess)

endGameScreen :: GameState -> IO ()
endGameScreen game = do
    putStrLn "===================================="
    putStrLn $ endGameMessage game
    putStrLn ""
    putStrLn "Bye bye now."

main :: IO ()
main = do
    word <- randomWord
    let game = initializeGame word

    putStrLn $ " ** Your word has " ++ (show . length . gameWord $ game) ++ " characters **"
    endGame <- iterateUntilM gameOver gameLoop game
    endGameScreen endGame
