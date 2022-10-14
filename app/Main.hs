module Main where

import System.Environment
import Network.HostName
import Control.Monad
import System.Info
import System.Process
import System.IO
import Data.List
import Data.Tuple.Select
import Text.Regex.PCRE

user :: IO String
user = getEnv "USER"

hostname :: IO String
hostname = getHostName

distro :: IO String
distro = let
    pattern = "[A-z\\d\\s\\/]+(?=\\\")" :: String
    in
      flip (=~~) pattern =<< readFile "/etc/os-release"

architechture :: IO String
architechture = return arch

kernel :: IO String
kernel = liftM (head . drop 2 . words . sel2) $ readProcessWithExitCode "uname" ["-a"] ""

display :: String -> String -> String -> String -> String -> String
display x y z v b = concat [x, "@", y
                           ,"\nDistro: ", z
                           ,"\nArchitechture: ", v
                           ,"\nKernel: ", b]

main :: IO ()
main = do
  putStrLn =<< liftM5 display user hostname distro architechture kernel
