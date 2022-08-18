module Main (main) where

import Control.Lens
import Data.List (partition)
import Control.Monad.Extra (unlessM)
import Control.Monad.Random
import Control.Monad.State
import System.Random.Shuffle (shuffleM)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

class Display a where
  display :: a -> String

instance Display a => Display [a] where
  display = show . map display

instance Display Double where
  display = show

instance Display Int where
  display = show

data PlayerId = Dealer | Player

type Dollars = Double

newtype Dealer = MkDealer { dealerHand :: [HandCard] }
  deriving stock Show

dealerHandL :: Lens' Dealer [HandCard]
dealerHandL = lens dealerHand $ \m x -> m { dealerHand = x }

data Player = MkPlayer
  { playerMoney :: Dollars
  , playerHand :: [HandCard]
  , playerBet :: Dollars
  }
  deriving stock Show

playerHandL :: Lens' Player [HandCard]
playerHandL = lens playerHand $ \m x -> m { playerHand = x }

playerMoneyL :: Lens' Player Dollars
playerMoneyL = lens playerMoney $ \m x -> m { playerMoney = x }

playerBetL :: Lens' Player Dollars
playerBetL = lens playerBet $ \m x -> m { playerBet = x }

data HandCard = Revealed Card | Unrevealed Card
  deriving stock Show

reveal :: HandCard -> HandCard
reveal (Unrevealed card) = Revealed card
reveal other = other

isUnrevealed :: HandCard -> Bool
isUnrevealed (Unrevealed _) = True
isUnrevealed _ = False

instance Display HandCard where
  display (Revealed c) = display c
  display (Unrevealed _) = "??"

toCard :: HandCard -> Card
toCard = \case
  Revealed c -> c
  Unrevealed c -> c

data BlackJack = BlackJack
  { blackJackDeck :: Deck
  , blackJackDiscard :: [Card]
  , blackJackDealer :: Dealer
  , blackJackPlayer :: Player
  }
  deriving stock Show

deckL :: Lens' BlackJack Deck
deckL = lens blackJackDeck $ \m x -> m { blackJackDeck = x }

discardL :: Lens' BlackJack [Card]
discardL = lens blackJackDiscard $ \m x -> m { blackJackDiscard = x }

dealerL :: Lens' BlackJack Dealer
dealerL = lens blackJackDealer $ \m x -> m { blackJackDealer = x }

playerL :: Lens' BlackJack Player
playerL = lens blackJackPlayer $ \m x -> m { blackJackPlayer = x }

newGame :: MonadRandom m => m BlackJack
newGame = do
  deck <- shuffle newDeck
  pure $ BlackJack
    { blackJackDeck = deck
    , blackJackDiscard = []
    , blackJackDealer = MkDealer []
    , blackJackPlayer = MkPlayer 100 [] 0
    }

newtype BlackJackT a = BlackJackT { unBlackJackT :: StateT BlackJack IO a }
  deriving newtype (MonadState BlackJack, MonadIO, Functor, Applicative, Monad, MonadRandom)

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
  other -> minValue other

minHandValue :: [Card] -> Int
minHandValue = sum . map (minValue . cardValue)

maxHandValue :: [Card] -> Int
maxHandValue = sum . map (maxValue . cardValue)

bestHandValue :: [Card] -> Int
bestHandValue cs =
  let
    (aces, others) = partition isAce cs
    maxHand = maxHandValue others
  in
    case aces of
      [] -> maxHand
      (_:xs) -> applyAce (maxHand + length xs)
  where
    isAce = (== Ace) . cardValue
    applyAce acc = if acc >= 11 then acc + 1 else acc + 11

getBestHandValue :: PlayerId -> BlackJackT Int
getBestHandValue p = bestHandValue <$> uses (handL p) (map toCard)

data Card = Card
  { cardValue :: Value
  , cardSuit :: Suit
  }
  deriving stock (Show, Eq)

instance Display Card where
  display (Card v s) = display v <> " of " <> display s

newtype Deck = Deck [Card]
  deriving stock (Show, Eq)

newDeck :: Deck
newDeck = Deck $ Card <$> [minBound ..] <*> [minBound ..]

shuffle :: MonadRandom m => Deck -> m Deck
shuffle (Deck cards) = Deck <$> shuffleM cards

draw :: BlackJackT Card
draw = do
  Deck cards <- use deckL
  case cards of
    [] -> do
      deck' <- shuffle . Deck =<< use discardL
      deckL .= deck'
      discardL .= []
      draw
    (c:cs) -> do
      deckL .= Deck cs
      pure c

blackJack :: BlackJackT ()
blackJack = do
  placeBet
  dealInitialCards
  playerHasNatural <- getHasNatural Player
  dealerHasNatural <- getHasNatural Dealer
  unless (playerHasNatural || dealerHasNatural) $ do
    handlePlayer
    handL Dealer %= map reveal
    unlessM (busted Player) handleDealer
  handleResult
  handlePlayAgain

handlePlayAgain :: BlackJackT ()
handlePlayAgain = do
  remainingMoney <- use $ playerL . playerMoneyL
  when (remainingMoney > 0) $ do
    c <- liftIO $ do
      putStrLn $ "Remaining money: " <> display remainingMoney
      putStr $ "Play again [Y/n] > "
      hFlush stdout
      getLine
    unless (c == "n") $ do
      pHand <- uses (handL Player) (map toCard)
      dHand <- uses (handL Dealer) (map toCard)
      handL Player .= []
      handL Dealer .= []
      discardL %= ((pHand <> dHand) <>)
      blackJack

pay :: (Dollars -> Dollars) -> BlackJackT ()
pay f = do
  winnings <- uses (playerL . playerBetL) f
  takeBackBet
  playerL . playerMoneyL += winnings

collectBet :: BlackJackT ()
collectBet = playerL . playerBetL .= 0

takeBackBet :: BlackJackT ()
takeBackBet = do
  bet <- use $ playerL . playerBetL
  playerL . playerBetL .= 0
  playerL . playerMoneyL += bet

getHasNatural :: PlayerId -> BlackJackT Bool
getHasNatural p = do
  handLength <- uses (handL p) length
  (_, maxHand) <- getHandTotal p
  pure $ maxHand == 21 && handLength == 2

placeBet :: BlackJackT ()
placeBet = do
  maxBet <- use $ playerL . playerMoneyL
  bet <- liftIO $ getBet maxBet
  playerL . playerMoneyL -= bet
  playerL . playerBetL .= bet

getBet :: Dollars -> IO Dollars
getBet maxDollars = do
  putStr $ "Place bet (max: " <> display maxDollars <> ")> "
  hFlush stdout
  mbet <- readMaybe <$> getLine
  case mbet of
    Just bet | bet > 0 -> pure bet
    Just _ -> getBet maxDollars
    Nothing -> getBet maxDollars

dealInitialCards :: BlackJackT ()
dealInitialCards = do
  dealCard Revealed Player
  dealCard Revealed Dealer
  dealCard Revealed Player
  dealCard Unrevealed Dealer

dealCard :: (Card -> HandCard) -> PlayerId -> BlackJackT ()
dealCard f p = do
  card <- draw
  handL p %= (f card :)

data Command = Hit | Stay
  deriving stock Eq

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
  playerHandValue <- getBestHandValue Player
  dealerHandValue <- getBestHandValue Dealer
  dHand <- use (handL Dealer)
  pHand <- use (handL Player)
  liftIO $ do
    putStrLn $ "Dealer: " <> display dHand
    unless (any isUnrevealed dHand) $
      putStrLn $ "Dealer Hand Value: " <> display dealerHandValue
    putStrLn $ "Player: " <> display pHand
    putStrLn $ "Player Hand Value: " <> display playerHandValue
    putStrLn ""

handlePlayer :: BlackJackT ()
handlePlayer = do
  displayGame
  command <- liftIO hitOrStay
  when (command == Hit) $ do
    dealCard Revealed Player
    unlessM (busted Player) handlePlayer

handleDealer :: BlackJackT ()
handleDealer = do
  (_, maxHand) <- getHandTotal Dealer
  when (maxHand < 17) $ do
   dealCard Revealed Dealer
   handleDealer

busted :: PlayerId -> BlackJackT Bool
busted p = do
  (minHand, _) <- getHandTotal p
  pure $ minHand > 21

handL :: PlayerId -> Lens' BlackJack [HandCard]
handL Player = playerL . playerHandL
handL Dealer = dealerL . dealerHandL

getHandTotal :: PlayerId -> BlackJackT (Int, Int)
getHandTotal p = do
  hand <- uses (handL p) (map toCard)
  pure (minHandValue hand, maxHandValue hand)

handleResult :: BlackJackT ()
handleResult = do
  playerBusted <- busted Player
  dealerBusted <- busted Dealer
  displayGame

  when playerBusted $ do
    liftIO $ putStrLn "Player busted, dealer wins"
    collectBet

  when (dealerBusted && not playerBusted) $ do
    liftIO $ putStrLn "Dealer busted, player wins"
    pay (* 1)

  when (not dealerBusted && not playerBusted) $ do
    playerHasNatural <- getHasNatural Player
    dealerHasNatural <- getHasNatural Dealer

    case (playerHasNatural, dealerHasNatural) of
      (True, False) -> do
        liftIO $ putStrLn "Player wins with natural"
        pay (* 1.5)
      (False, True) -> do
        displayGame
        liftIO $ putStrLn "Dealer wins with natural"
        collectBet
      (True, True) -> do
        liftIO $ putStrLn "Natural standoff"
        takeBackBet
      (False, False) -> do
        playerHandValue <- getBestHandValue Player
        dealerHandValue <- getBestHandValue Dealer
        case compare dealerHandValue playerHandValue of
          EQ -> do
            liftIO $ putStrLn "Stand off"
            takeBackBet
          GT -> do
            liftIO $ putStrLn "Dealer wins"
            collectBet
          LT -> do
            liftIO $ putStrLn "Player wins"
            pay (* 1)

main :: IO ()
main = newGame >>= runBlackJackT blackJack
