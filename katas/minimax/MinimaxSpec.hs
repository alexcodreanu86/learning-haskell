module MinimaxSpec where
import Minimax
import Test.Hspec

main :: IO()
main = hspec $ do
  describe "nextMove" $ do
    it "returns a blocking move when it is the case" $
      getMove [(0, 'X'), (6, 'X'), (4, 'O')] 'O' 'O' `shouldBe` 3

    it "finishes a game when it can" $
      getMove [(0, 'O'), (6, 'X'), (1, 'O'), (8, 'X')] 'O' 'O' `shouldBe` 2

    it "creates a fork when it is available" $
      getMove [(0, 'O'), (4, 'X'), (7, 'O'), (2, 'X')] 'O' 'O' `shouldBe` 6

    it "returns the best move for ai opponent" $
      getMove [(4, 'X'), (0, 'O'), (2, 'X')] 'X' 'O' `shouldBe` 6

  describe "getWinner" $ do
    it "returns 'X' when 'X' is the winner" $
      getWinner [(0,'X'), (1,'X'), (2,'X')] `shouldBe` Just 'X'

    it "returns 'O' when 'O' is the winner" $ do
      getWinner [(2,'O'), (4,'O'), (6,'O')] `shouldBe` Just 'O'
      getWinner [(0,'O'), (4,'O'), (8,'O')] `shouldBe` Just 'O'

    it "returns nothing when there is no winner" $
      getWinner [(2,'O'), (4,'X'), (6,'O')] `shouldBe` Nothing

  describe "isWinningCombo" $ do
    it "returns false when a combo is not winning" $
      isWinningCombo [(2,'O'), (4,'O'), (6,'O')] [0, 1, 2] `shouldBe` False

    it "returns true when a combo is winning" $
      isWinningCombo [(2,'O'), (4,'O'), (6,'O')] [2, 4, 6] `shouldBe` True

  describe "scoreBoard" $ do
    it "returns -10 for a board that can be lost the next round" $ do
      scoreBoard [(0,'O'), (1, 'X'), (3, 'X'), (4,'O'), (6,'O')] 'O' 'X' 0 `shouldBe` -8

    it "returns 10 ai is the winner" $
      scoreBoard [(2,'O'), (4,'O'), (6,'O')] 'O' 'O' 0 `shouldBe` 10

    it "returns -10 ai is the loser" $
      scoreBoard [(2,'O'), (4,'O'), (6,'O')] 'X' 'X' 0 `shouldBe` -10

    it "returns 0 if there is no winner" $ do
      let board = [(0,'X'), (1,'O'), (2, 'X') ,(3,'O'), (4,'X'), (5, 'O') ,(6,'O'), (7,'X'), (8, 'O')]
      scoreBoard board 'X' 'X' 0 `shouldBe` 0

  describe "getAvailableMoves" $ do
    it "returns all the moves when the board is empty" $
      getAvailableMoves [] `shouldBe` [0..8]

    it "returns no moves when all moves are taken" $ do
      let board = [(0,'X'), (1,'O'), (2, 'X') ,(3,'O'), (4,'X'), (5, 'O') ,(6,'O'), (7,'X'), (8, 'O')]
      getAvailableMoves board `shouldBe` []

    it "returns only the available moves when board has some moves" $ do
      let board = [(0,'X'), (1,'O'), (2, 'X'),(3,'O')]
      getAvailableMoves board `shouldBe` [4..8]

  describe "getHighestScored" $ do
    it "returns the move with the highest score" $
      getHighestScored (zip [1,6,3] [-10, 10, 0]) `shouldBe` 6

  describe "getLowestScored" $ do
    it "returns the move with the lowest score" $
      getLowestScored (zip [7,2,3] [-10, 10, 0]) `shouldBe` 7

  describe "isFull" $ do
    it "returns True when the board is full" $ do
      let board = [(0,'X'), (1,'O'), (2, 'X') ,(3,'O'), (4,'X'), (5, 'O') ,(6,'O'), (7,'X'), (8, 'O')]
      isFull board `shouldBe` True
    it "returns False when the board is NOT full" $ do
      let board = [(0,'X'), (1,'O'), (2, 'X')]
      isFull board `shouldBe` False

  describe "otherPlayer" $ do
    it "returns 'X' when ai is 'O'" $
      otherPlayer 'O' `shouldBe` 'X'

    it "returns 'O' when ai is 'X'" $
      otherPlayer 'X' `shouldBe` 'O'
