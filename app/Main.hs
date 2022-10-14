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

cpu :: IO String
cpu = let
  pattern = "(?<=\\s)[A-z\\s\\d]+(?=\\nstepping)" :: String
  in
    flip (=~~) pattern =<< readFile "/proc/cpuinfo"

display :: String -> String -> String -> String -> String -> String -> String
display x y z v b n = concat [x, "@", y
                           ,"\nDistro: ", z
                           ,"\nArchitechture: ", v
                           ,"\nKernel: ", b
                           , "\nCPU: ", n]

main :: IO ()
main = do
  putStrLn =<< return display `ap` user `ap` hostname `ap` distro `ap` architechture `ap` kernel `ap` cpu
