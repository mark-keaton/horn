module Plan
  ( list1
  , list2
  , list3
  , list4
  , list5
  , list6
  , list7
  , list8
  , list9
  , list10
  , plan
--  , getReadingForList
--  , setReadingForBook
  , getReadingForDay
  ) where

import           Control.Lens
import           Data.Map     ((!))

import           Bible

list1 = mkReadingList [books ! Matthew, books ! Mark, books ! Luke, books ! John]

list2 = mkReadingList [books ! Genesis, books ! Exodus, books ! Leviticus, books ! Numbers, books ! Deuteronomy]

list3 =
  mkReadingList
    [ books ! Romans
    , books ! FirstCorinthians
    , books ! SecondCorinthians
    , books ! Galatians
    , books ! Ephesians
    , books ! Philippians
    , books ! Colossians
    , books ! Hebrews
    ]

list4 =
  mkReadingList
    [ books ! FirstThessalonians
    , books ! SecondThessalonians
    , books ! FirstTimothy
    , books ! SecondTimothy
    , books ! Titus
    , books ! Philemon
    , books ! James
    , books ! FirstPeter
    , books ! SecondPeter
    , books ! FirstJohn
    , books ! SecondJohn
    , books ! ThirdJohn
    , books ! Jude
    , books ! Revelation
    ]

list5 = mkReadingList [books ! Job, books ! Ecclesiastes, books ! SongOfSolomon]

list6 = mkReadingList [books ! Psalms]

list7 = mkReadingList [books ! Proverbs]

list8 =
  mkReadingList
    [ books ! Joshua
    , books ! Judges
    , books ! Ruth
    , books ! FirstSamuel
    , books ! SecondSamuel
    , books ! FirstKings
    , books ! SecondKings
    , books ! FirstChronicles
    , books ! SecondChronicles
    , books ! Ezra
    , books ! Nehemiah
    , books ! Esther
    ]

list9 =
  mkReadingList
    [ books ! Isaiah
    , books ! Jeremiah
    , books ! Lamentations
    , books ! Ezekiel
    , books ! Daniel
    , books ! Hosea
    , books ! Joel
    , books ! Amos
    , books ! Obadiah
    , books ! Jonah
    , books ! Micah
    , books ! Nahum
    , books ! Habakkuk
    , books ! Zephaniah
    , books ! Haggai
    , books ! Zechariah
    , books ! Malachi
    ]

list10 = mkReadingList [books ! Acts]

plan = [list1, list2, list3, list4, list5, list6, list7, list8, list9, list10]

getReadingForDay :: Integer -> [BibleBook]
getReadingForDay day = fmap (getReadingForList day) plan

getReadingForList :: Integer -> [BibleBook]-> BibleBook
getReadingForList day bibleBooks = bibleBooks !! index
  where
    totalNumChapters = toInteger $ length bibleBooks
    index = fromInteger $ (day - 1) `mod` totalNumChapters

--setReadingForList :: Integer -> Integer -> BibleBook -> BibleBook
--setReadingForList day totalNumChapters book = book & over currentChapter setCurrentReading
--  where
--    setCurrentReading = const (calculateReading day (book ^. chapters))
--
--calculateReading :: Integer -> Integer -> Integer
--calculateReading day chapters
--  | chapters == 1 = chapters
--  | day >= chapters = (day + 1) `mod` chapters
--  | otherwise = day `mod` chapters
