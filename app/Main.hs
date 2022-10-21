module Main where

import System.Environment ( getEnv )
import Network.HostName ( getHostName )
import Control.Monad ( liftM )
import System.Info ( arch )
import System.Process ( readProcessWithExitCode )
import Data.Tuple.Select ( Sel2(sel2) )
import Data.List ( groupBy )
import Data.Char
-- import Text.Regex.PCRE ( (=~~), (=~) )
import Text.Printf ( printf )
import Text.ParserCombinators.ReadP

parse :: ReadP String -> String -> String
parse rules = fst . last . readP_to_S rules

user :: IO String
user = getEnv "USER"

hostname :: IO String
hostname = getHostName

distro :: IO String
distro = liftM (parse distroParse) $ readFile "/etc/os-release"

distroParse :: ReadP String
distroParse =
  manyTill (many1 (satisfy (/= '\n')) >> satisfy (== '\n')) (string "NAME=\"") >>
  many1 (satisfy (/= '\"'))

architechture :: IO String
architechture = return arch

-- To be cleaned
kernel :: IO String
kernel = liftM (parse kernelParse . sel2) $ readProcessWithExitCode "uname" ["-a"] ""
kernelParse :: ReadP String
kernelParse = many1 (satisfy (not . isSpace)) >>
  get >>
  many1 (satisfy (not . isSpace)) >>
  get >>
  many1 (satisfy (not . isSpace))

uptime :: IO String
uptime = liftM (parse uptimeParse . sel2) $ readProcessWithExitCode "uptime" ["-p"] ""

uptimeParse :: ReadP String
uptimeParse = string "up " >> many1 (satisfy (/= '\n'))

cpu :: IO String
cpu = liftM (parse cpuParse) $ readFile "/proc/cpuinfo"

cpuParse :: ReadP String
cpuParse =
  manyTill (many1 (satisfy (/= '\n')) >> satisfy (== '\n')) (string "model name\t: ") >>
  many1 (satisfy (/= '\n'))

shell :: IO String
shell = liftM (drop 1 . last . groupBy (\_ b -> b /= '/')) $ getEnv "SHELL"

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
