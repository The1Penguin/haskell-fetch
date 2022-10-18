module Main where

import System.Environment ( getEnv )
import Network.HostName ( getHostName )
import Control.Monad ( liftM )
import System.Info ( arch )
import System.Process ( readProcessWithExitCode )
import Data.Tuple.Select ( Sel2(sel2) )
import Data.List
import Text.Regex.PCRE ( (=~~), (=~) )
import Text.Printf ( printf )

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

uptime :: IO String
uptime = let
  pattern = "(?<=up )[A-z\\s\\d,]+(?=\\n)" :: String
  in
    liftM (flip (=~) pattern . sel2) $ readProcessWithExitCode "uptime" ["-p"] ""

cpu :: IO String
cpu = let
  pattern = "(?<=(model\\sname\\t:\\s))[A-z\\s\\d-@()]+(?=\\n)" :: String
  in
    flip (=~~) pattern =<< readFile "/proc/cpuinfo"

shell :: IO String
shell = liftM (drop 1 . head . reverse . groupBy (\_ b -> b /= '/')) $ getEnv "SHELL"

display :: String -> String -> String -> String -> String -> String -> String -> String -> String
display = printf "       __\t%s@%s\n\
                 \-=(o '.\t\tDistro:\t%s\n\
                 \   '.-.\\\tArch:\t%s\n\
                 \   /|  \\\\\tKernel:\t%s\n\
                 \   '|  ||\tUptime:\t%s\n\
                 \    _\\_):,_\tShell:\t%s\n\
                 \\t\tCPU:\t%s"

main :: IO ()
main = putStrLn =<< return display <*> user <*> hostname <*> distro <*> architechture <*> kernel <*> uptime <*> shell <*> cpu
