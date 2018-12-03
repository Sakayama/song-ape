main = putStrLn $ show $ totalTime''' songs

-- returns the first song
firstSong :: [Song] -> Song
firstSong (x:xs) = x

firstSong' :: [a] -> a
firstSong' = head

-- returns the last song
lastSong :: [Song] -> Song
lastSong [x] = x
lastSong (_:xs) = lastSong xs

lastSong' :: [a] -> a
lastSong' = last

-- takes new title and a list. Returns a new list with a new title1
setFirstTitle :: String -> [Song] -> [Song]
setFirstTitle title ((_, price, duration):xs) = (title, price, duration) : xs

-- takes previous title, a new title, then replaces all titles equal to previous
replaceTitles :: String -> String -> [Song] -> [Song]
replaceTitles targetTitle newTitle = map (\(previousTitle, price, duration) -> 
  let title = if previousTitle == targetTitle then newTitle else previousTitle 
  in (title, price, duration))

-- returns a list of titles
getTitles :: [Song] -> [String]
getTitles = map (\(title, _, _) -> title)

getTitles' :: [Song] -> [String]
getTitles' xs = [title | (title, _, _) <- xs]

getTitles'' :: [Song] -> [String]
getTitles'' [] = []
getTitles'' ((title, _, _):xs) = title : getTitles'' xs

-- returns total time of the playlist

totalTime :: [Song] -> Float
totalTime xs = sum [time | ( _, _, time) <- xs]

totalTime' :: [Song] -> Float
totalTime' = foldl (\acc (_, _, time) -> acc + time) 0

totalTime'' :: [Song] -> Float
totalTime'' [] = 0
totalTime'' ((_, _, time):tail) = time + totalTime'' tail

-- returns the longest song
longestSong :: [Song] -> Song
longestSong [x] = x
longestSong (x@(_, _, timeX):xs) = if timeX > timeY then x else y
  where y@( _, _, timeY) = longestSong xs

-- returns the shortest song
shortestSong :: [Song] -> Song
shortestSong [x] = x
shortestSong (x@(_, _, timeX):xs) = if timeX < timeY then x else y
  where y@( _, _, timeY) = shortestSong xs

--   returns the most pricey song
priceySong :: [Song] -> Song
priceySong [x] = x
priceySong (x@(_, priceX, _):xs) = if priceX > priceY then x else y
  where y@(_, priceY, _) = priceySong xs

-- returns the least expensive song
leastExpensive :: [Song] -> Song
leastExpensive [x] = x
leastExpensive (x@(_, priceX, _):xs) = if priceX < priceY then x else y
  where y@(_, priceY, _) = leastExpensive xs

smallestPrice :: [Song] -> Float
smallestPrice xs = price
  where (_, price, _) = leastExpensive xs

--takes a list of songs, returns a list of prices
getPrices :: [Song] -> [Float]
getPrices = map (\(title, price, duration) -> price)

-- returns the same list of songs sorted by price
sortByPrice :: [Song] -> [Song]
sortByPrice [] = []
sortByPrice (x:xs) = 
  let lte (_, priceX, _) (_, priceY, _) = priceX <= priceY
      gt (_, priceX, _) (_, priceY, _) = priceX > priceY    
      smallerSorted = sortByPrice [a | a <- xs, lte a x]
      biggerSorted = sortByPrice [a | a <- xs, gt a x]
   in smallerSorted ++ [x] ++ biggerSorted


-- types
type Song = (String, Float, Float)

-- data sourses
songs :: [Song]
songs = [
    ("Song for Siren", 15.0, 4.35),
    ("Mamma Mia", 1.0, 4.5),
    ("Lalala", 22.0, 4.0),
    ("Yes", 10.0, 2.35),
    ("Call Me", 39.0, 2.30),
    ("Yesterday", 9.0, 4.22),
    ("In My Head", 13.0, 43.38),
    ("Block", 5.0, 4.35)
        ]

songs2 :: [Song] 
songs2 = [
    ("Song for Siren", 15.0, 4.35),
    ("Lalala", 22.0, 4.0),
    ("Yes", 10.0, 2.35),
    ("Call Me", 39.0, 2.30),
    ("Block", 5.0, 4.35)
        ]
