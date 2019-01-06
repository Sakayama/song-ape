module Db where

import Data.Map (Map, fromList)
import Song
import Artist
import Order
import Album

orders :: [Order]
orders = [
    (33, [("1", 1.0)]), 
    (44, [("3", 1.0)])
    ]

artists :: Map String Artist
artists = fromList $ (\x -> (artistId x, x)) <$> [
    Artist "artist1" "John Frusciante",
    Artist "artist2" "the Beatles",
    Artist "artist3" "Vladimir Logachev"
    ]

albums :: Map String Album
albums = fromList $ (\x -> (fst x, x)) <$> [
    ("album1", "Empyrean"),
    ("album2", "Niandra LaDes"),
    ("album3", "Curtains"),
    ("album4", "White Album")
    ]

songs :: Map String Song
songs = fromList $ (\x@(id, _, _, _, _) -> (id, x)) <$> [
    ("0", "artist1", "album1", "Before The Beginning", duration (9, 09)),
    ("1", "artist1", "album1", "Song To The Siren", duration (3, 33)),
    ("2", "artist1", "album1", "Unreachable", duration (6, 10)),
    ("3", "artist1", "album1", "God", duration (3, 23)),
    ("4", "artist1", "album1", "Dark / Light", duration (8, 30)),
    ("5", "artist1", "album1", "Heaven", duration (4, 03)),
    ("6", "artist1", "album1", "Enough Of Me", duration (4, 15)),
    ("7", "artist1", "album1", "Central", duration (7, 16)),
    ("8", "artist1", "album1", "One More Of Me", duration (4, 06)),
    ("9", "artist1", "album1", "After The Ending", duration (3, 57)),
    ("a", "artist1", "album2", "As Can Be", duration (2, 57)),
    ("b", "artist1", "album2", "My Smile Is A Rifle", duration (3, 48)),
    ("c", "artist1", "album2", "Head (Beach Arab)", duration (2, 05)),
    ("d", "artist1", "album2", "Big Takeover", duration (3, 18)),
    ("e", "artist1", "album2", "Curtains", duration (2, 30)),
    ("f", "artist1", "album2", "Running Away Into You", duration (2, 12)),
    ("g", "artist1", "album2", "Mascara", duration (3, 40)),
    ("h", "artist1", "album2", "Been Insane", duration (1, 41)),
    ("i", "artist1", "album2", "Skin Blues", duration (1, 46)),
    ("j", "artist1", "album2", "Your Pussy's Glued To A Building On Fire", duration (3, 17)),
    ("k", "artist1", "album2", "Blood On My Neck From Success", duration (3, 09)),
    ("l", "artist1", "album2", "Ten To Butter Blood Voodoo", duration (1, 59)),
    ("m", "artist1", "album3", "The Past Recedes", duration (3, 53)),
    ("n", "artist1", "album3", "Lever Pulled", duration (2, 22)),
    ("o", "artist1", "album3", "Anne", duration (3, 35)),
    ("p", "artist1", "album3", "The Real", duration (3, 07)),
    ("q", "artist1", "album3", "A Name", duration (2, 03)),
    ("r", "artist1", "album3", "Control", duration (4, 29)),
    ("s", "artist1", "album3", "Your Warning", duration (3, 33)),
    ("t", "artist1", "album3", "Hope", duration (1, 56)),
    ("u", "artist1", "album3", "Ascension", duration (2, 52)),
    ("v", "artist1", "album3", "Time Tonight", duration (3, 12)),
    ("w", "artist1", "album3", "Leap Your Bar", duration (2, 36)),
    ("10", "artist2", "album4", "Back In The U.S.S.R.", duration (2, 42)),
    ("11", "artist2", "album4", "Dear Prudence", duration (3, 50)),
    ("12", "artist2", "album4", "Glass Onion", duration (2, 16)),
    ("13", "artist2", "album4", "Ob-La-Di, Ob-La-Da", duration (3, 07)),
    ("14", "artist2", "album4", "Wild Honey Pie", duration (0, 53)),
    ("15", "artist2", "album4", "The Continuing Story Of Bungalow Bill", duration (3, 11)),
    ("16", "artist2", "album4", "While My Guitar Gently Weeps", duration (4, 44)),
    ("17", "artist2", "album4", "Happiness Is A Warm Gun", duration (2, 41)),
    ("18", "artist2", "album4", "Martha My Dear", duration (2, 26)),
    ("19", "artist2", "album4", "I'm So Tired", duration (2, 03)),
    ("1a", "artist2", "album4", "Blackbird", duration (2, 17)),
    ("1b", "artist2", "album4", "Piggies", duration (2, 03)),
    ("1c", "artist2", "album4", "Rocky Raccoon", duration (3, 31)),
    ("1d", "artist2", "album4", "Don't Pass Me By", duration (3, 49)),
    ("1e", "artist2", "album4", "Why Don't We Do It In The Road?", duration (1, 40)),
    ("1f", "artist2", "album4", "I Will", duration (1, 44)),
    ("1g", "artist2", "album4", "Julia", duration (2, 52)),
    ("1h", "artist2", "album4", "Birthday", duration (2, 41)),
    ("1i", "artist2", "album4", "Yer Blues", duration (3, 57)),
    ("1j", "artist2", "album4", "Mother Nature's Son", duration (2, 46)),
    ("1k", "artist2", "album4", "Everybody's Got Something To Hide Except Me And My Monkey", duration (2, 24)),
    ("1l", "artist2", "album4", "Sexy Sadie", duration (3, 14)),
    ("1m", "artist2", "album4", "Helter Skelter", duration (3, 40)),
    ("1n", "artist2", "album4", "Long, Long, Long", duration (3, 02)),
    ("1o", "artist2", "album4", "Revolution 1", duration (4, 12)),
    ("1p", "artist2", "album4", "Honey Pie", duration (2, 40)),
    ("1q", "artist2", "album4", "Savoy Truffle", duration (2, 57)),
    ("1r", "artist2", "album4", "Cry Baby Cry", duration (3, 00)),
    ("1s", "artist2", "album4", "Revolution 9", duration (8, 17)),
    ("1t", "artist2", "album4", "Good Night", duration (3, 09))
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