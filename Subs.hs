{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, TemplateHaskell #-}
module Main where

import qualified Prelude as P (read, putStrLn)
import BasicPrelude hiding (try)

import Data.Attoparsec.ByteString.Char8 (isDigit, isSpace)
import Data.Attoparsec.Text.Lazy as Atto hiding (option)

import Data.Text.Lazy.Builder (Builder, toLazyText)

import qualified Data.ByteString.Lazy as BL (readFile)

import Data.Text.Lazy.Encoding (decodeLatin1)
import Data.Text.Lazy.IO as TIO (writeFile)

import Formatting hiding (string)

import Options.Applicative as Op hiding ((&))

import Control.Lens


newtype SubTitles = SubTitles {
  _entries :: [SubEntry]
} deriving Show

newtype TimeStamp = TimeStamp {
  _ts :: Integer
} deriving Show

data SubEntry = SubEntry {
  _tsbeg :: TimeStamp,
  _tsend :: TimeStamp,
  _text :: Text
} deriving Show

data Options = Options {
  infile :: String,
  outfile :: String,
  speed :: Maybe Double,
  off :: Maybe Integer
} deriving Show

makeLenses ''SubTitles
makeLenses ''SubEntry
makeLenses ''TimeStamp

offset :: Maybe Integer -> SubTitles -> SubTitles
offset Nothing subs = subs
offset (Just newoff) subs = subs & addts tsbeg & addts tsend
                              -- filter out entries with timestamps that have become negative.
                              & over entries (toListOf $ folded . filtered (\t -> t ^. tsbeg . ts > 0 && t ^. tsend . ts > 0))
  where addts which = over (entries . traverse . which) (over ts (+newoff))

speedup :: Maybe Double -> SubTitles -> SubTitles
speedup Nothing subs = subs
speedup (Just factor) subs = subs & multts tsbeg & multts tsend
  where multts which = over (entries . traverse . which) (over ts (round . (*factor) . fromIntegral))


main :: IO ()
main = do
  options <- execParser opts
  txt <- decodeLatin1 <$> BL.readFile (infile options)
  case maybeResult (parse subp txt) of

    -- Technically this should be rerendered as latin1, but unicode should work in vast majority of cases.
    Just res -> TIO.writeFile (outfile options) (toLazyText . renderSubtitles . speedup (speed options) . offset (off options) $ res)
    Nothing -> P.putStrLn $ "Unable to parse " <> (infile options) <> "file"

  where
    opts = info (helper <*> optionsParser) $
            fullDesc <> progDesc "Adjust the offset and speed that srt (subrip) subtitles are played." <> header "subspeed - adjust subtitle speed and offset"

    optionsParser = Options <$>
      option (short 'i' <> long "infile" <> metavar "FILE" <> help "Input file") <*>
      option (short 'o' <> long "outfile" <> metavar "FILE" <> help "Output file") <*>
      option (long "speed" <> metavar "FACTOR" <> help "Float indicating speed up (may be negative)" <> reader (pure . Just . P.read) <> value Nothing) <*>
      option (long "offset" <> metavar "MSEC" <> help "Number of milliseconds to offset subs (non negative)" <> reader (pure . Just . P.read) <> value Nothing)

subp :: Atto.Parser SubTitles
subp = SubTitles <$> many1 subentryp
  where
    subentryp :: Atto.Parser SubEntry
    subentryp = do
      skipWhile isDigit >> endOfLine
      ts1 <- timestampp
      void $ string "-->"
      ts2 <- timestampp
      endOfLine
      txt <- linep (takeWhile1 (not . isEndOfLine))
      return $ SubEntry ts1 ts2 (concat $ intersperse "\n" txt)
      where
        linep :: Atto.Parser a -> Atto.Parser [a]
        linep p = try (endOfLine <|> endOfInput) *> return [] <|>
                  (:) <$> (p <* endOfLine) <*> linep p

    timestampp :: Atto.Parser TimeStamp
    timestampp = do
      skipWhile isSpace
      h <- decimal <* char ':'
      m <- decimal <* char ':'
      s <- decimal <* char ','
      ms <- decimal
      skipWhile isHorizontalSpace
      return $ TimeStamp $ ms+(1000*s)+(60000*m)+(3600000*h)


renderSubtitles :: SubTitles -> Builder
renderSubtitles subs = foldl (\txt (n, e) -> txt <> renderSubEntry n e) mempty $ zip [1..] (subs ^. entries)
  where
    renderSubEntry :: Int -> SubEntry -> Builder
    renderSubEntry n (SubEntry beg end txt) = bprint (int % "\n" % builder % " --> " % builder % "\n" % stext % "\n\n") n (renderTimeStamp beg) (renderTimeStamp end) txt

    renderTimeStamp :: TimeStamp -> Builder
    renderTimeStamp (TimeStamp stamp) = bprint ((left 2 '0') % ":" % (left 2 '0') % ":" % (left 2 '0') % "," % (left 3 '0')) hours mins secs msecs
      where
        hours, mins, secs, msecs :: Integer
        hours = stamp `div` 3600000
        mins  = stamp `mod` 3600000 `div` 60000
        secs  = stamp `mod` 3600000 `mod` 60000 `div` 1000
        msecs = stamp `mod` 3600000 `mod` 60000 `mod` 1000
