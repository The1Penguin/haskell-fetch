import System.Process
import System.Environment
import GHC.IO.Exception

user :: IO String
user = getEnv "USER"

main :: IO ()
main = putStrLn =<< user
