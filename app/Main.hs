module Main where
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "No URL specified"
    url:_ -> putStrLn ("Navigating to: " ++ url)
