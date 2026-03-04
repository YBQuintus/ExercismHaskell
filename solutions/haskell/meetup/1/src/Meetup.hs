module Meetup (Weekday(..), Schedule(..), meetupDay) where


import Data.Time.Calendar (fromGregorian)
import Data.Time.Calendar.WeekDate (dayOfWeek)
import Data.Time.Calendar (Day)
import qualified Data.Time.Calendar.WeekDate as WD

data Weekday = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
             | Sunday

data Schedule = First
              | Second
              | Third
              | Fourth
              | Last
              | Teenth

toDayOfWeek :: Weekday -> WD.DayOfWeek
toDayOfWeek Monday    = WD.Monday
toDayOfWeek Tuesday   = WD.Tuesday
toDayOfWeek Wednesday = WD.Wednesday
toDayOfWeek Thursday  = WD.Thursday
toDayOfWeek Friday    = WD.Friday
toDayOfWeek Saturday  = WD.Saturday
toDayOfWeek Sunday    = WD.Sunday

meetupDayRange :: Schedule -> [Int]
meetupDayRange First = [1..7]
meetupDayRange Second = [8..14]
meetupDayRange Third = [15..21]
meetupDayRange Fourth = [22..28]
meetupDayRange Teenth = [13..19]
meetupDayRange _ = []

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay Last weekday year month = 
  last
    [ day
    | d <- [1..31]
    , let day = fromGregorian year month d
    , toDayOfWeek weekday == dayOfWeek day
    ]
meetupDay schedule weekday year month =
  head
    [ day
    | d <- meetupDayRange schedule
    , let day = fromGregorian year month d
    , toDayOfWeek weekday == dayOfWeek day
    ]

meetupDay _ _ _ _ = fromGregorian 1 1 1
