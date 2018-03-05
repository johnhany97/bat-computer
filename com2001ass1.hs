{-
     COM2001 Spring Assignment 1
     Haskell Template
     (c) 2018 Mike Stannett
     Email: m.stannett@sheffield.ac.uk
-}

-- If you would like to add your name and/or registration number
-- to this file, please do so here:
-- Name: John Ayad
-- Registration number: 160153363
--

type Input  = Int
type Output = Int

-- A program is something that tells a computer how to
-- move from one configuration to another, how to
-- recognize when a configuration represents a valid
-- accept state, and so on.

class (Eq cfg) => ProgrammableComputer cfg where
  initialise   :: Program -> [Input] -> cfg
  getOutput    :: cfg -> Output
  acceptState  :: Program -> cfg -> Bool
  doNextMove   :: Program -> cfg -> cfg
  runFrom      :: Program -> cfg -> cfg
  runProgram   :: Program -> [Input] -> cfg
  -- Default implementation
  runProgram p is = runFrom p (initialise p is)



-- The BATcomputer has just 3 types of instruction
-- CLR b        == empty box b
-- INC b        == add a token to box b
-- JEQ b1 b2 t  == if boxes b1 and b2 contain the same
--                 number of tokens, jump to instruction t
--
data Instruction
  = CLR {box :: Int}
  | INC {box :: Int}
  | JEQ {box1   :: Int,
         box2   :: Int,
         target :: Int}
  deriving (Eq, Show)

type Program = [Instruction]



-- PROBLEM 1. YOUR CODE HERE
-- --------------------------
-- Each instruction in a program refers to one or
-- more boxes.  What is the highest box number used
-- anywhere in the program?
maxBoxNum :: Program -> Int
maxBoxNum p = maxBoxNumAux p (length p)

maxBoxNumAux :: Program -> Int -> Int
maxBoxNumAux [] num = num
maxBoxNumAux ((CLR x):t) num = maxBoxNumAux t (max x num)
maxBoxNumAux ((INC x):t) num = maxBoxNumAux t (max x num)
maxBoxNumAux ((JEQ x y _):t) num = maxBoxNumAux t (max (max x y) num)

-- The configuration of a BATcomputer is given once
-- you know how many tokens are in each box, and
-- which instruction should be executed next
data BATConfig = BATConfig {
    boxes   :: [Int],
    counter :: Int
    } deriving (Eq)


-- PROBLEM 2. YOUR CODE HERE
-- --------------------------
instance Show BATConfig where
    show (BATConfig boxes counter) = "boxes = " ++ (show boxes) ++ "; counter = " ++ (show counter)




-- IMPLEMENTING THE BATComputer
-- ============================
-- User inputs run from Box 1 onwards. Output is what ends up in Box 1.
-- Box 0 can be used by programs for calculations.
instance ProgrammableComputer BATConfig  where
    -- PROBLEM 3: initialise   :: Program -> [Input] -> cfg
    initialise _ ins = BATConfig (0:ins) 0
    -- PROBLEM 4: acceptState  :: Program -> cfg -> Bool
    acceptState p (BATConfig _ counter) = (length p) == counter
    -- PROBLEM 5: doNextMove   :: Program -> cfg -> cfg
    doNextMove p (BATConfig boxes counter) = case p!!counter of
      CLR x -> (BATConfig (clearElement x boxes) inc_counter)
      INC x -> (BATConfig (incElement x boxes) inc_counter)
      JEQ x y t -> case (boxes!!x == boxes!!y) of
        True -> (BATConfig boxes t)
        False -> (BATConfig boxes inc_counter)
      where clearElement x (h:t)
              | x == 0 = (0:t)
              | otherwise = (h:clearElement (x-1) t)
            incElement x (h:t)
              | x == 0 = (h+1:t)
              | otherwise = (h:incElement (x-1) t)
            inc_counter = counter + 1
    -- PROBLEM 6: runFrom      :: Program -> cfg -> cfg
    runFrom p cfg
      | acceptState p cfg = cfg
      | otherwise = runFrom p (doNextMove p cfg)
    -- PROBLEM 7: getOutput    :: cfg -> Output
    getOutput (BATConfig (_:(x:_)) _) = x


-- This function is included to help with testing. Running
-- "execute p xs" should show the output generated when
-- running program p with user input(s) xs
execute :: Program -> [Input] -> Output
execute p ins = getOutput ((runProgram p ins) :: BATConfig)

-- PROBLEM 8. YOUR CODE HERE
-- ---------------------------
-- start a program at instruction n instead of 0.  In other
-- words, change Jump instructions from (J x y t) to (J x y (t+n))
-- and leave all other instructions unchanged.
transpose :: Int -> Program -> Program
transpose _ [] = []
transpose x (h:t) = case h of
  JEQ b1 b2 y -> ((JEQ b1 b2 (x+y)):transpose x t)
  _ -> (h:transpose x t)



-- PROBLEM 9. YOUR CODE HERE
-- ---------------------------
-- join two programs together, so as to run one
-- after the other
(*->*) :: Program -> Program -> Program
p1 *->* p2 = let newP2 = transpose (length p1) p2
             in p1 ++ newP2


-- PROBLEM 10. YOUR CODE HERE
-- ---------------------------
-- program to compute B1 = B1 + B2
adder :: Program
adder = [CLR 0,
         JEQ 2 0 5,
         INC 0,
         INC 1,
         JEQ 0 0 1]


-- PROBLEM 11. YOUR CODE HERE
-- ---------------------------
-- create a program to copy the contents of box m to box n (leave box m unchanged)
copyBox :: Int -> Int -> Program
copyBox m n = [CLR n,
               JEQ m n 4,
               INC n,
               JEQ n n 1]


-- PROBLEM 12. YOUR CODE HERE
-- ---------------------------
-- program to compute B1 = Bx + By
addXY :: Int -> Int -> Program
addXY 1 2 = adder
addXY 2 1 = adder
addXY 1 x = [CLR 0,
             JEQ x 0 5,
             INC 0,
             INC 1,
             JEQ 0 0 1]
addXY x 1 = [CLR 0,
             JEQ x 0 5,
             INC 0,
             INC 1,
             JEQ 0 0 1]
addXY x y = [CLR 1,
             CLR 0,
             JEQ x 0 6,
             INC 0,
             INC 1,
             JEQ 0 0 2,
             CLR 0,
             JEQ y 0 11,
             INC 0,
             INC 1,
             JEQ 0 0 7]


-- END OF TEMPLATE FILE
