import System.Random
import Data.List
import Control.Monad.Loops

data GameState = GameState { gameWord :: String
                           , playerGuess :: String
                           , attempt :: Int }  deriving (Show)

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
initializeGame word = GameState { gameWord = word
                                , playerGuess = map (\_ -> '_') word
                                , attempt = 0 }

updateGame :: GameState -> Char -> GameState
updateGame game guess = game { playerGuess = newGuess
                             , attempt = (attempt game) + 1 }
    where newGuess = uncoverLetters (gameWord game) (playerGuess game) guess

gameOver :: GameState -> Bool
gameOver g = (gameWord g) == (playerGuess g)

uncoverLetters :: String -> String -> Char -> String
uncoverLetters word guess c = map f pairs
    where pairs = zip word guess
          f (wc, gc)
            | gc == '_' && wc == c = wc
            | otherwise = gc

gameLoop :: GameState -> IO GameState
gameLoop g = do
    putStrLn  "==========================================="
    putStrLn $ "word: " ++ playerGuess g
    putStrLn "Guess a character: "
    guess <- getLine
    return $ updateGame g (head guess)

endGameMessage :: GameState -> String
endGameMessage g
  | attempts < 10 = "You got " ++ word ++ " in " ++ (show attempts) ++ " attempts! You're a fucking 200iq genius!"
  | attempts >= 10 && attempts <= 20  = "GG! You managed to find the word: " ++ word ++ " in " ++ (show attempts) ++ " attempts!"
  | attempts > 20  = "lol, you found " ++ word ++ " in " ++ (show attempts) ++ " tries... pepega you are."
    where attempts = attempt g
          word = gameWord g

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
