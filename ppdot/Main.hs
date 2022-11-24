module Main (main) where

import Control.Exception   (IOException, try)
import Control.Monad.Error (ErrorT(..), MonadError(..))
import System.Environment  (getArgs, getProgName)
import System.Exit         (exitFailure, exitSuccess)
import System.IO           (hPutStrLn, stderr)
import qualified Data.Text.IO as T

import Language.Dot (parseDot, renderDot)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

main :: IO ()
main =
    getArgs >>= run

run :: [String] -> IO ()
run args =
    case args of
      [fp] -> renderDotFile fp
      []   -> displayUsage >> exitSuccess
      _    -> displayUsage >> exitFailure

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

renderDotFile :: FilePath -> IO ()
renderDotFile fp =
    runErrorT (renderDotFileET fp) >>= either exitError putStrLn

renderDotFileET :: FilePath -> ErrorT String IO String
renderDotFileET fp = do
    contents <- T.readFile fp `liftCatch` show
    graph    <- parseDot fp contents `liftEither` show
    return $ renderDot graph

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

displayUsage :: IO ()
displayUsage = do
    programName <- getProgName
    ePutStrLns
      [ programName ++ ": Pretty-print a Graphviz DOT file."
      , unwords ["Usage:", programName, "FILE"]
      ]

exitError :: String -> IO ()
exitError e = do
    displayUsage
    ePutStrLn ""
    let el = lines e
    if length el == 1
      then ePutStrLn  ("ERROR: " ++ e)
      else ePutStrLns ("ERROR:" : indent el)
    exitFailure
  where
    indent = map ("  "++)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

liftCatch :: IO a -> (IOException -> e) -> ErrorT e IO a
liftCatch a f = ErrorT $ fmap (either (Left . f) Right) (try a)

liftEither :: (MonadError e m) => Either l r -> (l -> e) -> m r
liftEither e f = either (throwError . f) return e

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

ePutStrLn :: String -> IO ()
ePutStrLn = hPutStrLn stderr

ePutStrLns :: [String] -> IO ()
ePutStrLns = mapM_ (hPutStrLn stderr)
