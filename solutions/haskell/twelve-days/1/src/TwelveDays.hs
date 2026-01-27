module TwelveDays (recite) where

dayToString :: Int -> String
dayToString 1 = "first"
dayToString 2 = "second"
dayToString 3 = "third"
dayToString 4 = "fourth"
dayToString 5 = "fifth"
dayToString 6 = "sixth"
dayToString 7 = "seventh"
dayToString 8 = "eighth"
dayToString 9 = "ninth"
dayToString 10 = "tenth"
dayToString 11 = "eleventh"
dayToString 12 = "twelfth"

dayToPresents :: Int -> String
dayToPresents 1 = "a Partridge in a Pear Tree."
dayToPresents 2 = "two Turtle Doves, and " ++ dayToPresents 1
dayToPresents 3 = "three French Hens, " ++ dayToPresents 2
dayToPresents 4 = "four Calling Birds, " ++ dayToPresents 3
dayToPresents 5 = "five Gold Rings, " ++ dayToPresents 4
dayToPresents 6 = "six Geese-a-Laying, " ++ dayToPresents 5
dayToPresents 7 = "seven Swans-a-Swimming, " ++ dayToPresents 6
dayToPresents 8 = "eight Maids-a-Milking, " ++ dayToPresents 7
dayToPresents 9 = "nine Ladies Dancing, " ++ dayToPresents 8
dayToPresents 10 = "ten Lords-a-Leaping, " ++ dayToPresents 9
dayToPresents 11 = "eleven Pipers Piping, " ++ dayToPresents 10
dayToPresents 12 = "twelve Drummers Drumming, " ++ dayToPresents 11

getString :: Int -> String
getString start = "On the " ++ (dayToString start) ++ " day of Christmas my true love gave to me: " ++ (dayToPresents start)

recite :: Int -> Int -> [String]
recite start stop
  | start == stop = [getString start]
  | start < stop = [getString start] ++ (recite (start+1) stop)
