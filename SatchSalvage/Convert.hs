module SatchSalvage.Convert where

import Text.Pandoc.Writers.Markdown
import Text.Pandoc.Readers.HTML
import Text.Pandoc.Error (PandocError)
import Text.Pandoc.Options
import System.IO (stderr, hPrint)

htmlToMd :: String -> Either PandocError String
htmlToMd s = fmap (writeMarkdown def) (readHtml def s)

htmlToMdIO :: String -> IO String
htmlToMdIO s = handle (htmlToMd s)
  where handle (Left err) = hPrint stderr err >> return s
        handle (Right out) = return out
  
