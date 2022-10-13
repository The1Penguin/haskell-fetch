module Main where

import System.Environment
import Network.HostName
import Control.Monad

user :: IO String
user = getEnv "USER"

hostname :: IO String
hostname = getHostName

main :: IO ()
main = putStrLn =<< liftM2 (\x y -> x ++ "@" ++ y) user hostname
