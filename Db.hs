module Db (artists, songs, prices) where

import Song
import Artist

artists :: [Artist]
artists = [
    ("artist1", "John Frusciante"),
    ("artist2", "the Beatles"),
    ("artist3", "Vladimir Logachev")
    ]

songs :: [Song]
songs = [
    ("0", "artist1", "Before The Beginning", duration (9, 09)),
    ("1", "artist1", "Song To The Siren", duration (3, 33)),
    ("2", "artist1", "Unreachable", duration (6, 10)),
    ("3", "artist1", "God", duration (3, 23)),
    ("4", "artist1", "Dark / Light", duration (8, 30)),
    ("5", "artist1", "Heaven", duration (4, 03)),
    ("6", "artist1", "Enough Of Me", duration (4, 15)),
    ("7", "artist1", "Central", duration (7, 16)),
    ("8", "artist1", "One More Of Me", duration (4, 06)),
    ("9", "artist1", "After The Ending", duration (3, 57)),
    ("a", "artist1", "As Can Be", duration (2, 57)),
    ("b", "artist1", "My Smile Is A Rifle", duration (3, 48)),
    ("c", "artist1", "Head (Beach Arab)", duration (2, 05)),
    ("d", "artist1", "Big Takeover", duration (3, 18)),
    ("e", "artist1", "Curtains", duration (2, 30)),
    ("f", "artist1", "Running Away Into You", duration (2, 12)),
    ("g", "artist1", "Mascara", duration (3, 40)),
    ("h", "artist1", "Been Insane", duration (1, 41)),
    ("i", "artist1", "Skin Blues", duration (1, 46)),
    ("j", "artist1", "Your Pussy's Glued To A Building On Fire", duration (3, 17)),
    ("k", "artist1", "Blood On My Neck From Success", duration (3, 09)),
    ("l", "artist1", "Ten To Butter Blood Voodoo", duration (1, 59)),
    ("m", "artist1", "The Past Recedes", duration (3, 53)),
    ("n", "artist1", "Lever Pulled", duration (2, 22)),
    ("o", "artist1", "Anne", duration (3, 35)),
    ("p", "artist1", "The Real", duration (3, 07)),
    ("q", "artist1", "A Name", duration (2, 03)),
    ("r", "artist1", "Control", duration (4, 29)),
    ("s", "artist1", "Your Warning", duration (3, 33)),
    ("t", "artist1", "Hope", duration (1, 56)),
    ("u", "artist1", "Ascension", duration (2, 52)),
    ("v", "artist1", "Time Tonight", duration (3, 12)),
    ("w", "artist1", "Leap Your Bar", duration (2, 36)),
    ("10", "artist2", "Back In The U.S.S.R.", duration (2, 42)),
    ("11", "artist2", "Dear Prudence", duration (3, 50)),
    ("12", "artist2", "Glass Onion", duration (2, 16)),
    ("13", "artist2", "Ob-La-Di, Ob-La-Da", duration (3, 07)),
    ("14", "artist2", "Wild Honey Pie", duration (0, 53)),
    ("15", "artist2", "The Continuing Story Of Bungalow Bill", duration (3, 11)),
    ("16", "artist2", "While My Guitar Gently Weeps", duration (4, 44)),
    ("17", "artist2", "Happiness Is A Warm Gun", duration (2, 41)),
    ("18", "artist2", "Martha My Dear", duration (2, 26)),
    ("19", "artist2", "I'm So Tired", duration (2, 03)),
    ("1a", "artist2", "Blackbird", duration (2, 17)),
    ("1b", "artist2", "Piggies", duration (2, 03)),
    ("1c", "artist2", "Rocky Raccoon", duration (3, 31)),
    ("1d", "artist2", "Don't Pass Me By", duration (3, 49)),
    ("1e", "artist2", "Why Don't We Do It In The Road?", duration (1, 40)),
    ("1f", "artist2", "I Will", duration (1, 44)),
    ("1g", "artist2", "Julia", duration (2, 52)),
    ("1h", "artist2", "Birthday", duration (2, 41)),
    ("1i", "artist2", "Yer Blues", duration (3, 57)),
    ("1j", "artist2", "Mother Nature's Son", duration (2, 46)),
    ("1k", "artist2", "Everybody's Got Something To Hide Except Me And My Monkey", duration (2, 24)),
    ("1l", "artist2", "Sexy Sadie", duration (3, 14)),
    ("1m", "artist2", "Helter Skelter", duration (3, 40)),
    ("1n", "artist2", "Long, Long, Long", duration (3, 02)),
    ("1o", "artist2", "Revolution 1", duration (4, 12)),
    ("1p", "artist2", "Honey Pie", duration (2, 40)),
    ("1q", "artist2", "Savoy Truffle", duration (2, 57)),
    ("1r", "artist2", "Cry Baby Cry", duration (3, 00)),
    ("1s", "artist2", "Revolution 9", duration (8, 17)),
    ("1t", "artist2", "Good Night", duration (3, 09))
    ]

-- song id, price
prices :: [(String, Float)]
prices = [
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