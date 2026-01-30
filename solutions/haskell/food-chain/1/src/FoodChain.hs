module FoodChain (song) where

animalFromLevel :: Int -> String
animalFromLevel 0 = "fly"
animalFromLevel 1 = "spider"
animalFromLevel 2 = "bird"
animalFromLevel 3 = "cat"
animalFromLevel 4 = "dog"
animalFromLevel 5 = "goat"
animalFromLevel 6 = "cow"
animalFromLevel 7 = "horse"
animalFromLevel _ = error "Invalid level"

songInter :: String -> String
songInter "fly"    = "I don't know why she swallowed the fly. Perhaps she'll die."
songInter "spider" = "It wriggled and jiggled and tickled inside her."
songInter "bird"   = "How absurd to swallow a bird!"
songInter "cat"    = "Imagine that, to swallow a cat!"
songInter "dog"    = "What a hog, to swallow a dog!"
songInter "goat"   = "Just opened her throat and swallowed a goat!"
songInter "cow"    = "I don't know how she swallowed a cow!"
songInter "horse"  = "She's dead, of course!"
songInter _        = ""

songStart :: String -> String
songStart animal = "I know an old lady who swallowed a " ++ animal ++ "."

swallowLine :: Int -> String
swallowLine n =
  let a = animalFromLevel n
      b = animalFromLevel (n - 1)
      spiderExtra =
        if b == "spider"
        then " that wriggled and jiggled and tickled inside her"
        else ""
  in "She swallowed the " ++ a ++ " to catch the " ++ b ++ spiderExtra ++ "."

verse :: Int -> String
verse 0 =
  unlines
    [ songStart "fly"
    , songInter "fly"
    , ""
    ]

verse 7 =
  unlines
    [ songStart "horse"
    , songInter "horse"
    ]

verse n =
  unlines $
    [ songStart animal
    , songInter animal
    ]
    ++ map swallowLine [n, n - 1 .. 1]
    ++ [songInter "fly", ""]
  where
    animal = animalFromLevel n


song :: String
song = concatMap verse [0..7]
