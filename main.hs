import Order
import qualified Db

main = putStrLn $ showOrder Db.artists $ placeOrder ["7", "1a", "a", "z"] Db.songs Db.prices