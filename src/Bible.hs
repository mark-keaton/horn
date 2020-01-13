{-# LANGUAGE TemplateHaskell #-}

module Bible
  ( BibleBook(..)
  , Book(..)
  , books
  , next
  , prev
  , chapters
  , currentChapter
  , mkReadingList
  ) where

import           Control.Monad

import           Control.Lens
import qualified Data.Map      as Map

data Book
  = Genesis
  | Exodus
  | Leviticus
  | Numbers
  | Deuteronomy
  | Joshua
  | Judges
  | Ruth
  | FirstSamuel
  | SecondSamuel
  | FirstKings
  | SecondKings
  | FirstChronicles
  | SecondChronicles
  | Ezra
  | Nehemiah
  | Esther
  | Job
  | Psalms
  | Proverbs
  | Ecclesiastes
  | SongOfSolomon
  | Isaiah
  | Jeremiah
  | Lamentations
  | Ezekiel
  | Daniel
  | Hosea
  | Joel
  | Amos
  | Obadiah
  | Jonah
  | Micah
  | Nahum
  | Habakkuk
  | Zephaniah
  | Haggai
  | Zechariah
  | Malachi
  | Matthew
  | Mark
  | Luke
  | John
  | Acts
  | Romans
  | FirstCorinthians
  | SecondCorinthians
  | Galatians
  | Ephesians
  | Philippians
  | Colossians
  | FirstThessalonians
  | SecondThessalonians
  | FirstTimothy
  | SecondTimothy
  | Titus
  | Philemon
  | Hebrews
  | James
  | FirstPeter
  | SecondPeter
  | FirstJohn
  | SecondJohn
  | ThirdJohn
  | Jude
  | Revelation
  deriving (Eq, Ord)

data BibleBook =
  BibleBook
    { _name           :: Book
    , _chapters       :: Integer
    , _currentChapter :: Integer
    }

makeLenses ''BibleBook

class Looping a where
  next :: a -> a
  prev :: a -> a

instance Looping BibleBook where
  next book
    | book ^. currentChapter == book ^. chapters = book & over currentChapter (const 1)
    | otherwise = book & over currentChapter (+ 1)
  prev book
    | book ^. currentChapter == 1 = book & over currentChapter (const $ book ^. chapters)
    | otherwise = book & over currentChapter ((-) 1)

instance Show BibleBook where
  show book = show (book ^. name) ++ " " ++ show (book ^. currentChapter)

-- Show Instances
instance Show Book where
  show Genesis             = "Genesis"
  show Exodus              = "Exodus"
  show Leviticus           = "Leviticus"
  show Numbers             = "Numbers"
  show Deuteronomy         = "Deuteronomy"
  show Joshua              = "Joshua"
  show Judges              = "Judges"
  show Ruth                = "Ruth"
  show FirstSamuel         = "1 Samuel"
  show SecondSamuel        = "2 Samuel"
  show FirstKings          = "1 Kings"
  show SecondKings         = "2 Kings"
  show FirstChronicles     = "1 Chronicles"
  show SecondChronicles    = "2 Chronicles"
  show Ezra                = "Ezra"
  show Nehemiah            = "Nehemiah"
  show Esther              = "Esther"
  show Job                 = "Job"
  show Psalms              = "Psalms"
  show Proverbs            = "Proverbs"
  show Ecclesiastes        = "Ecclesiastes"
  show SongOfSolomon       = "Song of Solomon"
  show Isaiah              = "Isaiah"
  show Jeremiah            = "Jeremiah"
  show Lamentations        = "Lamentations"
  show Ezekiel             = "Ezekiel"
  show Daniel              = "Daniel"
  show Hosea               = "Hosea"
  show Joel                = "Joel"
  show Amos                = "Amos"
  show Obadiah             = "Obadiah"
  show Jonah               = "Jonah"
  show Micah               = "Micah"
  show Nahum               = "Nahum"
  show Habakkuk            = "Habakkuk"
  show Zephaniah           = "Zephaniah"
  show Haggai              = "Haggai"
  show Zechariah           = "Zechariah"
  show Malachi             = "Malachi"
  show Matthew             = "Matthew"
  show Mark                = "Mark"
  show Luke                = "Luke"
  show John                = "John"
  show Acts                = "Acts"
  show Romans              = "Romans"
  show FirstCorinthians    = "1 Corinthians"
  show SecondCorinthians   = "2 Corinthians"
  show Galatians           = "Galatians"
  show Ephesians           = "Ephesians"
  show Philippians         = "Philippians"
  show Colossians          = "Colossians"
  show FirstThessalonians  = "1 Thessalonians"
  show SecondThessalonians = "2 Thessalonians"
  show FirstTimothy        = "1 Timothy"
  show SecondTimothy       = "2 Timothy"
  show Titus               = "Titus"
  show Philemon            = "Philemon"
  show Hebrews             = "Hebrews"
  show James               = "James"
  show FirstPeter          = "1 Peter"
  show SecondPeter         = "2 Peter"
  show FirstJohn           = "1 John"
  show SecondJohn          = "2 John"
  show ThirdJohn           = "3 John"
  show Jude                = "Jude"
  show Revelation          = "Revelation"

books :: Map.Map Book BibleBook
books =
  Map.fromList
    [ (Genesis, BibleBook Genesis 50 1)
    , (Exodus, BibleBook Exodus 40 1)
    , (Leviticus, BibleBook Leviticus 27 1)
    , (Numbers, BibleBook Numbers 36 1)
    , (Deuteronomy, BibleBook Deuteronomy 34 1)
    , (Joshua, BibleBook Joshua 24 1)
    , (Judges, BibleBook Judges 21 1)
    , (Ruth, BibleBook Ruth 4 1)
    , (FirstSamuel, BibleBook FirstSamuel 31 1)
    , (SecondSamuel, BibleBook SecondSamuel 24 1)
    , (FirstKings, BibleBook FirstKings 22 1)
    , (SecondKings, BibleBook SecondKings 25 1)
    , (FirstChronicles, BibleBook FirstChronicles 29 1)
    , (SecondChronicles, BibleBook SecondChronicles 36 1)
    , (Ezra, BibleBook Ezra 10 1)
    , (Nehemiah, BibleBook Nehemiah 13 1)
    , (Esther, BibleBook Esther 10 1)
    , (Job, BibleBook Job 42 1)
    , (Psalms, BibleBook Psalms 150 1)
    , (Proverbs, BibleBook Proverbs 31 1)
    , (Ecclesiastes, BibleBook Ecclesiastes 12 1)
    , (SongOfSolomon, BibleBook SongOfSolomon 8 1)
    , (Isaiah, BibleBook Isaiah 66 1)
    , (Jeremiah, BibleBook Jeremiah 52 1)
    , (Lamentations, BibleBook Lamentations 5 1)
    , (Ezekiel, BibleBook Ezekiel 48 1)
    , (Daniel, BibleBook Daniel 12 1)
    , (Hosea, BibleBook Hosea 14 1)
    , (Joel, BibleBook Joel 3 1)
    , (Amos, BibleBook Amos 9 1)
    , (Obadiah, BibleBook Obadiah 1 1)
    , (Jonah, BibleBook Jonah 4 1)
    , (Micah, BibleBook Micah 7 1)
    , (Nahum, BibleBook Nahum 3 1)
    , (Habakkuk, BibleBook Habakkuk 3 1)
    , (Zephaniah, BibleBook Zephaniah 3 1)
    , (Haggai, BibleBook Haggai 2 1)
    , (Zechariah, BibleBook Zechariah 14 1)
    , (Malachi, BibleBook Malachi 4 1)
    , (Matthew, BibleBook Matthew 28 1)
    , (Mark, BibleBook Mark 16 1)
    , (Luke, BibleBook Luke 24 1)
    , (John, BibleBook John 21 1)
    , (Acts, BibleBook Acts 28 1)
    , (Romans, BibleBook Romans 16 1)
    , (FirstCorinthians, BibleBook FirstCorinthians 16 1)
    , (SecondCorinthians, BibleBook SecondCorinthians 13 1)
    , (Galatians, BibleBook Galatians 6 1)
    , (Ephesians, BibleBook Ephesians 6 1)
    , (Philippians, BibleBook Philippians 4 1)
    , (Colossians, BibleBook Colossians 4 1)
    , (FirstThessalonians, BibleBook FirstThessalonians 5 1)
    , (SecondThessalonians, BibleBook SecondThessalonians 3 1)
    , (FirstTimothy, BibleBook FirstTimothy 6 1)
    , (SecondTimothy, BibleBook SecondTimothy 4 1)
    , (Titus, BibleBook Titus 3 1)
    , (Philemon, BibleBook Philemon 1 1)
    , (Hebrews, BibleBook Hebrews 13 1)
    , (James, BibleBook James 5 1)
    , (FirstPeter, BibleBook FirstPeter 5 1)
    , (SecondPeter, BibleBook SecondPeter 3 1)
    , (FirstJohn, BibleBook FirstJohn 5 1)
    , (SecondJohn, BibleBook SecondJohn 1 1)
    , (ThirdJohn, BibleBook ThirdJohn 1 1)
    , (Jude, BibleBook Jude 1 1)
    , (Revelation, BibleBook Revelation 22 1)
    ]

-- Expand a starting chapter book into all chapters of that book
mkBook :: BibleBook -> [BibleBook]
mkBook book = fmap mkBook' range
  where
    range = [1 .. book ^. chapters]
    mkBook' chapter = BibleBook {_name = book ^. name, _currentChapter = chapter, _chapters = book ^. chapters}

mkReadingList :: [BibleBook] -> [BibleBook]
mkReadingList bibleBooks = mkBook =<< bibleBooks
