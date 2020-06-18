module GameState
( GameState
, initializeGame
, updateGame
, gameOver
, gameWord
, playerGuess
, attempt
, endGameMessage
) where


data GameState = GameState { gameWord :: String
                           , playerGuess :: String
                           , attempt :: Int }  deriving (Show)

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

endGameMessage :: GameState -> String
endGameMessage g
  | attempts < 10 = "You got " ++ word ++ " in " ++ (show attempts) ++ " attempts! You're a fucking 200iq genius!"
  | attempts >= 10 && attempts <= 20  = "GG! You managed to find the word: " ++ word ++ " in " ++ (show attempts) ++ " attempts!"
  | attempts > 20  = "lol, you found " ++ word ++ " in " ++ (show attempts) ++ " tries... pepega you are."
    where attempts = attempt g
          word = gameWord g

