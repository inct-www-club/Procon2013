module Util.Doc (addFile, addDirectory, showAll) where

import Data.Monoid
import Control.Monad.Writer
import Control.Lens
import System.FilePath
import System.FilePath.Lens
import System.Directory
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as BS

------------------------------------------------------

main = showAll $ do
    addFile "preview/preview.hs"
    addFile "encoder/encoder.hs"
    addDirectory [".cpp", ".h"] "ui/procon"

------------------------------------------------------


addFile x = tell $ Endo (x:)

addDirectory exts dir = do
    ps <- liftIO $ getDirectoryContents dir
    forM_ ps $ \path -> do
        let ext = view extension path
        when (ext `elem` exts) $ addFile $ dir </> path

languageByExtension = Map.fromList [(".hs", "haskell"), (".h", "cpp"), (".cpp", "cpp"), (".txt", "")]

showAll m = do
    paths <- liftM (flip appEndo []) $ execWriterT m

    forM_ paths $ \path -> do
        putStrLn $ "## " ++ view filename path
        putStrLn ""
        putStrLn $ "```" ++ languageByExtension Map.! view extension path
        BS.readFile path >>= BS.putStrLn
        putStrLn "```\n"