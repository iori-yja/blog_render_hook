module Main where
import Control.Monad
import Text.Pandoc
import System.Directory
import System.IO
import System.Exit
import Data.List
import Data.Functor
import System.FilePath.Posix

gettemplate :: IO(String)
gettemplate =
	getDirectoryContents "." >>=
		(filterM doesFileExist) . (filter $ isSuffixOf ".template") >>=
			mapM readFile >>= return . concat

traversedocs :: FilePath -> FilePath -> IO [(FilePath, String)]
traversedocs src dest = getDirectoryContents src >>=
	(filterM doesFileExist) . (filter $ isSuffixOf ".md") >>=
		mapM (\s -> (\c -> (newfname s, c)) <$> (readFile s))
		where newfname = (</>) dest . flip replaceExtension ".html"

docconvert :: String -> (FilePath, String) -> (FilePath, String)
docconvert t (f, s) = (f, md2html t s)

md2html:: String -> String -> String
md2html t = (writeHtmlString (writerconf t)) . readMarkdown def

writerconf a = def {writerTemplate = a, writerStandalone = True}

main::IO()
main = do
	fs <- traversedocs "." "web/html"
	t <- gettemplate
	mapM_ (\(s,t) -> writeFile s t) $ map (docconvert t) fs
--main = traversedocs >>= mapM (writehtml . md2html) >> exitWith ExitSuccess

