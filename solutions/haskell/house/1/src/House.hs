module House (rhyme) where

animal :: Int -> String
animal 0 = "Jack"
animal 1 = "malt"
animal 2 = "rat"
animal 3 = "cat"
animal 4 = "dog"
animal 5 = "cow with the crumpled horn"
animal 6 = "maiden all forlorn"
animal 7 = "man all tattered and torn"
animal 8 = "priest all shaven and shorn"
animal 9 = "rooster that crowed in the morn"
animal 10 = "farmer sowing his corn"
animal 11 = "horse and the hound and the horn"
animal _ = ""

rhymeEnd :: String
rhymeEnd = "that lay in the house that Jack built."

rhymeStart :: Int -> String
rhymeStart 0 = "This is the house that Jack built."
rhymeStart n = "This is the " ++ (animal n)

rhymeMid :: Int -> String
rhymeMid 1 = "that ate the " 
rhymeMid 2 = "that killed the " 
rhymeMid 3 = "that worried the "
rhymeMid 4 = "that tossed the "
rhymeMid 5 = "that milked the "
rhymeMid 6 = "that kissed the "
rhymeMid 7 = "that married the "
rhymeMid 8 = "that woke the "
rhymeMid 9 = "that kept the "
rhymeMid 10 = "that belonged to the "
rhymeMid _ = ""

rhymeMidRepeat :: Int -> [String]
rhymeMidRepeat 1 = ["that ate the " ++ animal 1]
rhymeMidRepeat n = (rhymeMid n ++ animal n) : (rhymeMidRepeat (n-1))

rhymeSequence :: Int -> [String]
rhymeSequence 0 = [rhymeStart 0, ""]
rhymeSequence 1 = [rhymeStart 1, rhymeEnd, ""]
rhymeSequence n = [rhymeStart n] ++ (rhymeMidRepeat (n-1)) ++ [rhymeEnd] ++ [""]

rhyme :: String
rhyme = unlines (init (concatMap (rhymeSequence) (take 12 [0,1..])))
