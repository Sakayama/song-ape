module Db where

import Data.Map (Map, fromList)
import qualified Song as S
import qualified Artist
import qualified Order as O
import qualified Album

orders :: [O.Order]
orders = [
    O.Order 33 [("1", 1.0)], 
    O.Order 44 [("3", 1.0)]
    ]

artists :: Map String Artist.Artist
artists = fromList $ (\x -> (Artist.artistId x, x)) <$> [
    Artist.Artist "artist1" "John Frusciante",
    Artist.Artist "artist2" "the Beatles",
    Artist.Artist "artist3" "Vladimir Logachev"
    ]

albums :: Map String Album.Album
albums = fromList $ (\x -> (Album.albumId x, x)) <$> [
    Album.Album "album1" "Empyrean" 2009,
    Album.Album "album2" "Niandra LaDes" 1994, 
    Album.Album "album3" "Curtains" 2005,
    Album.Album "album4" "White Album" 1968
    ]

songs :: Map String S.Song
songs = fromList $ (\x -> (S.songId x, x)) <$> [
    S.Song "0" "artist1" "album1" "Before The Beginning" $ S.diffTime (9, 09),
    S.Song "1" "artist1" "album1" "Song To The Siren" $ S.diffTime (3, 33),
    S.Song "2" "artist1" "album1" "Unreachable" $ S.diffTime (6, 10),
    S.Song "3" "artist1" "album1" "God" $ S.diffTime (3, 23),
    S.Song "4" "artist1" "album1" "Dark / Light" $ S.diffTime (8, 30),
    S.Song "5" "artist1" "album1" "Heaven" $ S.diffTime (4, 03),
    S.Song "6" "artist1" "album1" "Enough Of Me" $ S.diffTime (4, 15),
    S.Song "7" "artist1" "album1" "Central" $ S.diffTime (7, 16),
    S.Song "8" "artist1" "album1" "One More Of Me" $ S.diffTime (4, 06),
    S.Song "9" "artist1" "album1" "After The Ending" $ S.diffTime (3, 57),
    S.Song "a" "artist1" "album2" "As Can Be" $ S.diffTime (2, 57),
    S.Song "b" "artist1" "album2" "My Smile Is A Rifle" $ S.diffTime (3, 48),
    S.Song "c" "artist1" "album2" "Head (Beach Arab)" $ S.diffTime (2, 05),
    S.Song "d" "artist1" "album2" "Big Takeover" $ S.diffTime (3, 18),
    S.Song "e" "artist1" "album2" "Curtains" $ S.diffTime (2, 30),
    S.Song "f" "artist1" "album2" "Running Away Into You" $ S.diffTime (2, 12),
    S.Song "g" "artist1" "album2" "Mascara" $ S.diffTime (3, 40),
    S.Song "h" "artist1" "album2" "Been Insane" $ S.diffTime (1, 41),
    S.Song "i" "artist1" "album2" "Skin Blues" $ S.diffTime (1, 46),
    S.Song "j" "artist1" "album2" "Your Pussy's Glued To A Building On Fire" $ S.diffTime (3, 17),
    S.Song "k" "artist1" "album2" "Blood On My Neck From Success" $ S.diffTime (3, 09),
    S.Song "l" "artist1" "album2" "Ten To Butter Blood Voodoo" $ S.diffTime (1, 59),
    S.Song "m" "artist1" "album3" "The Past Recedes" $ S.diffTime (3, 53),
    S.Song "n" "artist1" "album3" "Lever Pulled" $ S.diffTime (2, 22),
    S.Song "o" "artist1" "album3" "Anne" $ S.diffTime (3, 35),
    S.Song "p" "artist1" "album3" "The Real" $ S.diffTime (3, 07),
    S.Song "q" "artist1" "album3" "A Name" $ S.diffTime (2, 03),
    S.Song "r" "artist1" "album3" "Control" $ S.diffTime (4, 29),
    S.Song "s" "artist1" "album3" "Your Warning" $ S.diffTime (3, 33),
    S.Song "t" "artist1" "album3" "Hope" $ S.diffTime (1, 56),
    S.Song "u" "artist1" "album3" "Ascension" $ S.diffTime (2, 52),
    S.Song "v" "artist1" "album3" "Time Tonight" $ S.diffTime (3, 12),
    S.Song "w" "artist1" "album3" "Leap Your Bar" $ S.diffTime (2, 36),
    S.Song "10" "artist2" "album4" "Back In The U.S.S.R." $ S.diffTime (2, 42),
    S.Song "11" "artist2" "album4" "Dear Prudence" $ S.diffTime (3, 50),
    S.Song "12" "artist2" "album4" "Glass Onion" $ S.diffTime (2, 16),
    S.Song "13" "artist2" "album4" "Ob-La-Di, Ob-La-Da" $ S.diffTime (3, 07),
    S.Song "14" "artist2" "album4" "Wild Honey Pie" $ S.diffTime (0, 53),
    S.Song "15" "artist2" "album4" "The Continuing Story Of Bungalow Bill" $ S.diffTime (3, 11),
    S.Song "16" "artist2" "album4" "While My Guitar Gently Weeps" $ S.diffTime (4, 44),
    S.Song "17" "artist2" "album4" "Happiness Is A Warm Gun" $ S.diffTime (2, 41),
    S.Song "18" "artist2" "album4" "Martha My Dear" $ S.diffTime (2, 26),
    S.Song "19" "artist2" "album4" "I'm So Tired" $ S.diffTime (2, 03),
    S.Song "1a" "artist2" "album4" "Blackbird" $ S.diffTime (2, 17),
    S.Song "1b" "artist2" "album4" "Piggies" $ S.diffTime (2, 03),
    S.Song "1c" "artist2" "album4" "Rocky Raccoon" $ S.diffTime (3, 31),
    S.Song "1d" "artist2" "album4" "Don't Pass Me By" $ S.diffTime (3, 49),
    S.Song "1e" "artist2" "album4" "Why Don't We Do It In The Road?" $ S.diffTime (1, 40),
    S.Song "1f" "artist2" "album4" "I Will" $ S.diffTime (1, 44),
    S.Song "1g" "artist2" "album4" "Julia" $ S.diffTime (2, 52),
    S.Song "1h" "artist2" "album4" "Birthday" $ S.diffTime (2, 41),
    S.Song "1i" "artist2" "album4" "Yer Blues" $ S.diffTime (3, 57),
    S.Song "1j" "artist2" "album4" "Mother Nature's Son" $ S.diffTime (2, 46),
    S.Song "1k" "artist2" "album4" "Everybody's Got Something To Hide Except Me And My Monkey" $ S.diffTime (2, 24),
    S.Song "1l" "artist2" "album4" "Sexy Sadie" $ S.diffTime (3, 14),
    S.Song "1m" "artist2" "album4" "Helter Skelter" $ S.diffTime (3, 40),
    S.Song "1n" "artist2" "album4" "Long, Long, Long" $ S.diffTime (3, 02),
    S.Song "1o" "artist2" "album4" "Revolution 1" $ S.diffTime (4, 12),
    S.Song "1p" "artist2" "album4" "Honey Pie" $ S.diffTime (2, 40),
    S.Song "1q" "artist2" "album4" "Savoy Truffle" $ S.diffTime (2, 57),
    S.Song "1r" "artist2" "album4" "Cry Baby Cry" $ S.diffTime (3, 00),
    S.Song "1s" "artist2" "album4" "Revolution 9" $ S.diffTime (8, 17),
    S.Song "1t" "artist2" "album4" "Good Night" $ S.diffTime (3, 09)
    ]

-- song id, price
songPrices :: [(String, Float)]
songPrices = [
    ("0", 1.00),
    ("1", 1.00),
    ("2", 1.00),
    ("3", 1.00),
    ("4", 1.00),
    ("5", 1.00),
    ("6", 1.00),
    ("7", 1.00),
    ("8", 1.00),
    ("9", 1.00),
    ("10", 0.99),
    ("11", 0.99),
    ("12", 0.99),
    ("13", 0.99),
    ("14", 0.99),
    ("15", 0.99),
    ("16", 0.99),
    ("17", 0.99),
    ("18", 0.99),
    ("19", 0.99),
    ("1a", 0.99),
    ("1b", 0.99),
    ("1c", 0.99),
    ("1d", 0.99),
    ("1e", 0.99),
    ("1f", 0.99),
    ("1g", 0.99),
    ("1h", 0.99),
    ("1i", 0.99),
    ("1j", 0.99),
    ("1k", 0.99),
    ("1l", 0.99),
    ("1m", 0.99),
    ("1n", 0.99),
    ("1o", 0.99),
    ("1p", 0.99),
    ("1q", 0.99),
    ("1r", 0.99),
    ("1s", 0.99),
    ("1t", 0.99)
    ]

-- album id, price
albumPrices :: [(String, Float)]
albumPrices = [
    ("album1", 7.99),
    ("album2", 7.99),
    ("album3", 7.99),
    ("album4", 8.99)
    ]