import System.Random
import Data.List

data GameState = GameState { gameWord :: String
                           , playerGuess :: String }  deriving (Show)

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
                                , playerGuess = map (\_ -> '_') word }

-- cannot reuse this game after assigning it in ghci :/
updateGame :: GameState -> Char -> GameState
updateGame game guess = game { playerGuess = newGuess }
    where newGuess = uncoverLetter (gameWord game) (playerGuess game) guess

uncoverLetter :: String -> String -> Char -> String
uncoverLetter word guess c = map f pairs
    where pairs = zip word guess
          f (wc, gc)
            | gc == '_' && wc == c = wc
            | otherwise = '_'

main :: IO ()
main = do
    word <- randomWord
    let game = initializeGame word
    putStrLn $ show game
