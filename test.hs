main = putStrLn $ show $ getTitles'' songs

getTitles = map (\(title, _, _) -> title)

getTitles'' [] = []
getTitles'' ((title, _, _):xs) = title : getTitles'' xs


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
