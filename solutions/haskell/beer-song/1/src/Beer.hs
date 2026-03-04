module Beer (song) where

beerText :: Int -> String
beerText 0 = "no more bottles"
beerText 1 = "1 bottle"
beerText i = (show i) ++ " bottles"

oneOrIt :: Int -> String
oneOrIt 1 = "it"
oneOrIt _ = "one"

generateSong :: Int -> [String]
generateSong 0 = ["No more bottles of beer on the wall, " ++ beerText 0 ++ " of beer.", "Go to the store and buy some more, 99 bottles of beer on the wall."]
generateSong i = [beerText i ++ " of beer on the wall, " ++ beerText i ++ " of beer.", "Take " ++ oneOrIt i ++ " down and pass it around, " ++ beerText (i-1) ++ " of beer on the wall.", ""] ++ generateSong (i-1)

song :: String
song = unlines (generateSong 99)