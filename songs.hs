main = putStrLn $ show $ shortestSong songs

-- returns a list of titles

getTitles :: [(String, b, c)] -> [String]
getTitles = map (\(title, _, _) -> title)

getTitles' :: [(String, b, c)] -> [String]
getTitles' xs = [title | (title, _, _) <- xs]

getTitles'' :: [(String, b, c)] -> [String]
getTitles'' [] = []
getTitles'' ((title, _, _):xs) = title : getTitles'' xs

-- returns total time of the playlist

totalTime :: [(a, b, Float)] -> Float
totalTime xs = sum [time | ( _, _, time) <- xs]

totalTime' :: [(a, b, Float)] -> Float
totalTime' = foldl (\acc (a, b, time) -> acc + time::Float) 0

totalTime'' :: [(a, b, Float)] -> Float
totalTime'' [] = 0
totalTime'' ((a, b, time):tail) = time + totalTime'' tail

-- returns the longest song
longestSong :: (Ord c) => [(a, b, c)] -> (a, b, c)
longestSong [x] = x
longestSong (x@(_, _, timeX):xs) = if timeX > timeY then x else y
  where y@( _, _, timeY) = longestSong xs

-- returns the shortest song
shortestSong :: (Ord c) => [(a, b, c)] -> (a, b, c)
shortestSong [x] = x
shortestSong (x@(_, _, timeX):xs) = if timeX < timeY then x else y
  where y@( _, _, timeY) = shortestSong xs

-- data sourses
songs :: [(String, Float, Float)]
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

songs2 = [
    ("Song for Siren", 15.0, 4.35),
    ("Lalala", 22.0, 4.0),
    ("Yes", 10.0, 2.35),
    ("Call Me", 39.0, 2.30),
    ("Block", 5.0, 4.35)
        ]
