
import System.Exit
import System.Environment
import System.IO
import System.Console.GetOpt
import System.Directory
import System.Posix.Files (getFileStatus, isDirectory, fileExist)
import Control.Monad (when, sequence)
import qualified Control.Exception as Ex
import Data.Char (isSpace)
import Data.List (isSuffixOf)
import Text.VCard
import Text.VCard.Format.Directory
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as CB

usageHeader = "Usage: pplqq [OPTIONS] DIRECTORY PATTERN"
usageString = usageInfo usageHeader options
printUsage = putStrLn usageString >> exitFailure
printErrorMsg msg = putStrLn msg >> exitFailure

data Flag = Name

options :: [OptDescr Flag]
options =  [ Option ['n'] ["name"] (NoArg Name) "searches the name" ]

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

fixDirName dir = case reverse trimmed of
                      '/':_ -> trimmed
                      _     -> trimmed ++ "/"
                 where trimmed = trim dir

getArgsAndOpts :: [String] -> IO( String, String, [Flag] )
getArgsAndOpts argv = 
    case getOpt RequireOrder options argv of
      ( o, [d,p], [] ) -> return (d,fixDirName p,o)
      _                -> printUsage

checkDir :: String -> IO()
checkDir dir = do
    exists <- fileExist dir
    when (not exists) $ printErrorMsg (dir ++ " does not exist")
    isDir  <- fmap isDirectory $ getFileStatus dir
    when (not isDir) $ printErrorMsg ( dir ++ " is not a direcoty" ) 


toVCards_unsave :: String -> IO( [VCard] )
toVCards_unsave filename  = do
        handle <- openFile filename ReadMode 
        input  <- B.hGetContents handle
        return $! readVCards filename (insertCarriageReturns input)

isVCard :: String -> Bool
isVCard = isSuffixOf ".vcf"

toVCards :: String -> IO( [VCard] )
toVCards path = Ex.catch ( toVCards_unsave path ) handler
        where handler = (\e -> ( print ("Error in file " ++ path ++ (show e) )  >> (return [] ) ) )::( Ex.SomeException -> IO [VCard] )

allVCards :: String -> IO( [VCard] )
allVCards directory = do
       _             <- checkDir directory
       fileNames     <- getDirectoryContents directory
       let fullFilePaths = map (directory ++) fileNames
       listOfLists   <- sequence $ map toVCards ( filter isVCard fullFilePaths )
       return $ concat listOfLists

toLines = CB.lines . (CB.filter (/= '\r') )
nl = CB.pack "\r\n"

insertCarriageReturns :: B.ByteString -> B.ByteString
insertCarriageReturns = ( CB.intercalate nl ) . toLines



main :: IO ()
main = do
    ( dir, pat, opts ) <- getArgs >>= getArgsAndOpts
    cards              <- allVCards dir
    _                  <- ( putStrLn . show . length ) $ cards
    exitSuccess
    

