module Main where

main :: IO ()
main = gameLoop $ initializeGame $ head wordList


wordList :: [String]
wordList = ["apple", "banana", "cherry"] 

data Hangman = Hangman
    { wordToGuess :: String
    , guessedLetters :: [Char]
    , remainingAttempts :: Int
    } deriving (Show)

initializeGame :: String -> Hangman
initializeGame word = Hangman word [] 6

showResult :: Hangman -> String
showResult game = "Word: " ++ (displayWord game) ++ "\nAttempts left: " ++ show (remainingAttempts game)     

displayWord :: Hangman -> String
displayWord game = [if c `elem` guessedLetters game then c else '_' | c <- wordToGuess game]

gameLoop :: Hangman -> IO ()
gameLoop game 
    | displayWord game == wordToGuess game = putStrLn $ "Congratulations! You've guessed the word: " ++ wordToGuess game
    | remainingAttempts game <= 0 = putStrLn $ "Game Over! The word was: " ++ wordToGuess game
    | otherwise = do
        putStrLn $ showResult game
        putStrLn "Enter your guess: "
        guess <- getLine
        let newGame = updateGame guess game
        gameLoop newGame      

updateGame :: String -> Hangman -> Hangman
updateGame guess game
    | c `elem` guessedLetters game = game
    | c `elem` wordToGuess game = game { guessedLetters = c : guessedLetters game }
    | otherwise = game { guessedLetters = c : guessedLetters game, remainingAttempts = remainingAttempts game - 1 }
    where c = head guess