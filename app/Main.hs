module Main (main) where

import Data.List (partition)
import Control.Monad.Extra (unlessM)
import Control.Monad.Random
import Control.Monad.State
import System.Random.Shuffle (shuffleM)
import System.IO (hFlush, stdout)

class Display a where
  display :: a -> String

instance Display a => Display [a] where
  display = show . map display

instance Display Int where
  display = show

data Player = Dealer | Player

data HandCard = Revealed Card | Unrevealed Card
  deriving stock Show

reveal :: HandCard -> HandCard
reveal (Unrevealed card) = Revealed card
reveal other = other

instance Display HandCard where
  display (Revealed c) = display c
  display (Unrevealed _) = "??"

toCard :: HandCard -> Card
toCard = \case
  Revealed c -> c
  Unrevealed c -> c

data BlackJack = BlackJack
  { blackJackDeck :: Deck
  , blackJackDealer :: [HandCard]
  , blackJackPlayer :: [HandCard]
  }
  deriving stock Show

newGame :: MonadRandom m => m BlackJack
newGame = do
  deck <- shuffle newDeck
  pure $ BlackJack
    { blackJackDeck = deck
    , blackJackDealer = []
    , blackJackPlayer = []
    }

newtype BlackJackT a = BlackJackT { unBlackJackT :: StateT BlackJack IO a }
  deriving newtype (MonadState BlackJack, MonadIO, Functor, Applicative, Monad)

runBlackJackT :: BlackJackT a -> BlackJack -> IO a
runBlackJackT body game = evalStateT (unBlackJackT body) game

data Suit = Clubs | Diamonds | Hearts | Spades
  deriving stock (Show, Eq, Ord, Enum, Bounded)

instance Display Suit where
  display = show

data Value
  = Ace
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  deriving stock (Show, Eq, Ord, Enum, Bounded)

instance Display Value where
  display = show

minValue :: Value -> Int
minValue = \case
  Ace -> 1
  Jack -> 10
  Queen -> 10
  King -> 10
  other -> fromEnum other + 1

maxValue :: Value -> Int
maxValue = \case
  Ace -> 11
  Jack -> 10
  Queen -> 10
  King -> 10
  other -> fromEnum other + 1

minHandValue :: [HandCard] -> Int
minHandValue = sum . map (minValue . cardValue . toCard)

maxHandValue :: [HandCard] -> Int
maxHandValue = sum . map (maxValue . cardValue . toCard)

bestHandValue :: [HandCard] -> Int
bestHandValue hs =
  let (aces, others) = partition isAce cards
  in foldr applyAce (sum $ map (maxValue . cardValue) others) aces
  where
    cards = map toCard hs
    isAce = (== Ace) . cardValue
    applyAce _ acc = if acc >= 11 then acc + 1 else acc + 11

getBestHandValue :: Player -> BlackJackT Int
getBestHandValue p = do
  hand <- gets handFunc
  pure $ bestHandValue hand
  where
    handFunc = case p of
                 Player -> blackJackPlayer
                 Dealer -> blackJackDealer

data Card = Card
  { cardValue :: Value
  , cardSuit :: Suit
  }
  deriving stock (Show, Eq)

instance Display Card where
  display (Card v s) = display v <> " of " <> display s

newtype Deck = Deck [Card]
  deriving stock (Show, Eq)

overDeckM :: Functor m => ([Card] -> m [Card]) -> Deck -> m Deck
overDeckM f (Deck cs) = Deck <$> f cs

newDeck :: Deck
newDeck = Deck $ Card <$> [minBound ..] <*> [minBound ..]

shuffle :: MonadRandom m => Deck -> m Deck
shuffle = overDeckM shuffleM

draw :: BlackJackT (Maybe Card)
draw = do
  Deck cards <- gets blackJackDeck
  case cards of
    [] -> pure Nothing
    (c:cs) -> do
      modify (\s -> s { blackJackDeck = Deck cs })
      pure $ Just c

blackJack :: BlackJackT ()
blackJack = do
  dealInitialCards
  unlessM playerBusted handlePlayer
  unlessM playerBusted handleDealer
  displayResult

dealInitialCards :: BlackJackT ()
dealInitialCards = do
  dealCard Revealed Player
  dealCard Revealed Dealer
  dealCard Revealed Player
  dealCard Unrevealed Dealer

dealCard :: (Card -> HandCard) -> Player -> BlackJackT ()
dealCard f p = do
  mcard <- draw
  case mcard of
    Nothing -> error "empty deck should be impossible"
    Just card -> do
      let c = f card
      case p of
        Dealer -> modify (\s -> s { blackJackDealer = c : blackJackDealer s })
        Player -> modify (\s -> s { blackJackPlayer = c : blackJackPlayer s })

data Command = Hit | Stay

hitOrStay :: IO Command
hitOrStay = do
  putStr "[h]it or [s]tay > "
  hFlush stdout
  c <- getChar <* getLine
  case c of
    'h' -> pure Hit
    's' -> pure Stay
    _ -> hitOrStay

displayGame :: BlackJackT ()
displayGame = do
  g <- get
  playerHandValue <- getBestHandValue Player
  liftIO $ do
    putStrLn $ "Dealer: " <> display (blackJackDealer g)
    putStrLn $ "Player: " <> display (blackJackPlayer g)
    putStrLn $ "Player Hand Value: " <> display playerHandValue

handlePlayer :: BlackJackT ()
handlePlayer = do
  displayGame
  command <- liftIO hitOrStay
  case command of
    Stay -> pure ()
    Hit -> do
      dealCard Revealed Player
      unlessM playerBusted handlePlayer

-- dealer logic is automatic, on dealer's turn the dealer must reveal and if 17
-- or more then stand, otherwise must hit until 17 or more
handleDealer :: BlackJackT ()
handleDealer = do
  modify $ \s -> s { blackJackDealer = map reveal (blackJackDealer s) }
  go
 where
   go = do
     (_, maxHand) <- getHandTotal Dealer
     if maxHand >= 17
        then pure ()
        else do
          dealCard Revealed Dealer
          go

playerBusted :: BlackJackT Bool
playerBusted = do
  (minHand, _) <- getHandTotal Player
  pure $ minHand > 21

getHandTotal :: Player -> BlackJackT (Int, Int)
getHandTotal p = do
  hand <- gets handFunc
  pure (minHandValue hand, maxHandValue hand)
 where
   handFunc = case p of
                Player -> blackJackPlayer
                Dealer -> blackJackDealer

displayResult :: BlackJackT ()
displayResult = do
  g <- get
  playerHandValue <- getBestHandValue Player
  dealerHandValue <- getBestHandValue Dealer
  liftIO $ do
    putStrLn $ "Dealer: " <> display (blackJackDealer g)
    putStrLn $ "Dealer Hand Value: " <> display dealerHandValue
    putStrLn $ "Player: " <> display (blackJackPlayer g)
    putStrLn $ "Player Hand Value: " <> display playerHandValue
    if (dealerHandValue >= playerHandValue && dealerHandValue <= 21) || playerHandValue > 21
       then putStrLn "Dealer Wins"
       else putStrLn "Player Wins"


main :: IO ()
main = do
  game <- newGame
  runBlackJackT blackJack game
