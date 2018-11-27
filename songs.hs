main = putStrLn $ show $ totalTime songs

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
totalTime' = foldl (\acc (a, b, time) -> acc + time) 0

totalTime'' :: [Song] -> Float
totalTime'' [] = 0
totalTime'' ((a, b, time):tail) = time + totalTime'' tail

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
