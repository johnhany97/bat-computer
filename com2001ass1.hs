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
-- E-mail: jayad1@sheffield.ac.uk

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
-- --------------------------
{------------------------------------------
 maxBoxNum function

 Function that given a program returns the the highest box number
 used all around the program in all of it's instructions.

 Parameters:
   Program
 Return:
   Integer representing the highest box number used
 Example Call:
   maxBoxNum [CLR 1, INC 2, JEQ 1 2 1] ~> 3
 Tests:
   maxBoxNum [] ~> 0
   maxBoxNum [CLR 1] ~> 1
   maxBoxNum [CLR 10] ~> 10
   maxBoxNum [INC 1] ~> 1
   maxBoxNum [INC 10] ~> 10
   maxBoxNum [JEQ 1 1 1] ~> 1
   maxBoxNum [JEQ 1 2 1] ~> 2
   maxBoxNum [JEQ 2 1 1] ~> 2
   maxBoxNum [JEQ 1 1 2] ~> 1 (Because even though it jumps to 2, the total instructions are only 1.. Program invalid)
   maxBoxNum [CLR 1, INC 1] ~> 2
   maxBoxNum [INC 1, CLR 1, JEQ 1 1 4, INC 5] ~> 5
------------------------------------------}
maxBoxNum :: Program -> Int
maxBoxNum p = maxBoxNumAux p (length p)

-- Auxilary function used by the above Function
-- Only thing about it is that it keeps track of the maximum box number
-- seen so far through out the program.
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
-- Making BATConfig print out something that's meaningful by making it an instance of Show
instance Show BATConfig where
    show (BATConfig boxes counter) = "boxes = " ++ (show boxes) ++ "; counter = " ++ (show counter)

-- IMPLEMENTING THE BATComputer
-- ============================
-- User inputs run from Box 1 onwards. Output is what ends up in Box 1.
-- Box 0 can be used by programs for calculations.
instance ProgrammableComputer BATConfig  where
    -- PROBLEM 3: initialise   :: Program -> [Input] -> cfg
    {------------------------------------
      initialise Function

      Function that initialises the BATComputer by returning and initialised
      BATConfig (configuration)
      If given an input to fill boxes that suffices what the program is to deal with, it returns the BATConfig with
      the input given (plus box 0 which is used by the program for calculations)
      If given input is not enough to fill boxes that are used by the program, this fills them with zeros after adding
      the given input.

      Parameters:
        Program that is to be run (We extract from it the maxBoxNum)
        Inputs that are to be loaded in the boxes (Box 1 and upwards)
      Returns:
        BATConfig
      Example Call:
        ((initialise [] [1,2]) :: BATConfig) ~> boxes = [0,1,2]; counter = 0
      Tests:
        ((initialise [] []) :: BATConfig) ~> boxes = [0]; counter = 0
        ((initialise [] [1,2]) :: BATConfig) ~> boxes = [0,1,2]; counter = 0
        ((initialise [CLR 1] []) :: BATConfig) ~> boxes = [0,0]; counter = 0
        ((initialise [CLR 1] [1,2]) :: BATConfig) ~> boxes = [0,1,2]; counter = 0
        ((initialise [CLR 1, INC 15] [1,2]) :: BATConfig) ~> boxes = [0,1,2,0,0,0,0,0,0,0,0,0,0,0,0,0]; counter = 0
    -------------------------------------}
    initialise p ins
      | inputLength >= programMax = BATConfig (0:ins) 0
      | otherwise = BATConfig (concatLists [0] (concatLists ins (genEmptyBoxes diff))) 0
      where programMax = maxBoxNum p
            inputLength = length ins
            diff = programMax - inputLength
            genEmptyBoxes num = take num (repeat 0)
            concatLists lis1 lis2 = lis1 ++ lis2
    -- PROBLEM 4: acceptState  :: Program -> cfg -> Bool
    {------------------------------------
      acceptState Function

      Function that tells if the Configuration given is currently in an accept state
      accept state for a BATConfig is considered when the program is done so basically
      when the counter in the config is equal to the length of the program so we finished
      all of the instructions in the program.
      Assumption made as well here is that if the JEQ instruction jumps far away
      to an instruction that is more than what the program has (Ex. Program is of length 2 instructions,
      but the JEQ instruction jumps to instruction 100 which ideally won't happen), We consider the programs
      as in an acceptState and this function will return true

      Parameters:
        Program that is being run
        BATConfig which we're checking if it's currently in the accept state
      Returns:
        Boolean that is true if in accept state
      Example Call:
        acceptState [] (BATConfig [0] 0) ~> True
      Tests:
        acceptState [] (BATConfig [0] 0) ~> True
        acceptState [CLR 1] (BATConfig [0] 0) ~> False
        acceptState [CLR 1] (BATConfig [0,1] 0) ~> False
        acceptState [INC 1, CLR 1] (BATConfig [0,1,2] 0) ~> False
        acceptState [INC 1, CLR 1] (BATConfig [0,1,2] 1) ~> False
        acceptState [INC 1, CLR 1] (BATConfig [0,1,2] 2) ~> True
        acceptState [JEQ 1 1 4] (BATConfig [0,1,0,0,0] 0) ~> False
        acceptState [JEQ 1 1 4] (BATConfig [0,1,0,0,0] 1) ~> True
        acceptState [JEQ 1 1 4] (BATConfig [0,1,0,0,0] 4) ~> True (Edge case)
    -------------------------------------}
    acceptState p (BATConfig _ counter) = (length p) <= counter
    -- PROBLEM 5: doNextMove   :: Program -> cfg -> cfg
    {------------------------------------
      doNextMove Function

      Function that does the next move and increments the counter in the configuration

      Parameters:
        Program that is being run
        BATConfig which we're doing the program's instructions on
      Returns:
        New Configuration
      Example Call:
        ((doNextMove [INC 1] (BATConfig [0,1] 0)) :: BATConfig) ~> boxes = [0,2]; counter = 1
      Tests:
        ((doNextMove [INC 1] (BATConfig [0,1] 0)) :: BATConfig) ~> boxes = [0,2]; counter = 1
        ((doNextMove [CLR 1] (BATConfig [0,1] 0)) :: BATConfig) ~> boxes = [0,0]; counter = 1
        ((doNextMove [JEQ 1 1 2] (BATConfig [0,1] 0)) :: BATConfig) ~> boxes = [0,1]; counter = 2
        ((doNextMove [INC 1, CLR 2, INC 1] (BATConfig [0,1,2,3] 1)) :: BATConfig) ~> boxes = [0,1,0,3]; counter = 2
        ((doNextMove [INC 1, CLR 2, INC 1] (BATConfig [0,1,2,3] 2)) :: BATConfig) ~> boxes = [0,2,2,3]; counter = 3
        ((doNextMove [INC 1, JEQ 0 1 1] (BATConfig [0,1] 1)) :: BATConfig) ~> boxes = [0,1]; counter = 2
        ((doNextMove [INC 1, JEQ 0 1 0] (BATConfig [1,1] 1)) :: BATConfig) ~> boxes = [1,1]; counter = 0
    -------------------------------------}
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
    {-------------------------------------
      runFrom Function

      Function given a program and a configuration runs that program till program ends
      returning in the end the configuration after all of the program's instructions have been
      carried out on it

      Parameters:
        Program that is being run
        BATConfig which we're doing the program's instructions on
      Returns:
        Final Configuration which is in acceptState
      Example Call:
        runFrom [CLR 1, INC 1, INC 1] (BATConfig [0,2] 0) ~> boxes = [0,2]; counter = 3
      Tests:
        runFrom [CLR 1] (BATConfig [0,2] 0) ~> boxes = [0,0]; counter = 1
        runFrom [CLR 1] (BATConfig [0,2] 1) ~> boxes = [0,2]; counter = 1
        runFrom [JEQ 0 1 3, INC 1, JEQ 0 0 0] (BATConfig [10, 0] 0) ~> boxes = [10,10]; counter = 3
        runFrom [INC 1] (BATConfig [0,1] 2) ~> boxes = [0,1]; counter = 2
    -------------------------------------}
    runFrom p cfg
      | acceptState p cfg = cfg
      | otherwise = runFrom p (doNextMove p cfg)
    -- PROBLEM 7: getOutput    :: cfg -> Output
    {-------------------------------------
      getOutput Function

      Function that simply returns the value in Box 1.
      This is an implementation decision where we consider that any given program
      will leave it's output in box 1.
      Assumption made is that if given a config that doesn't have Box1, we return 0

      Parameters:
        BATConfig
      Returns:
        Value of tokens in box 1 in given configuration
      Example Call:
        getOutput (BATConfig [0,1,3] 0) ~> 1
      Tests:
        getOutput (BATConfig [0,1,3] 0) ~> 1
        getOutput (BATConfig [0,2,3] 0) ~> 2
        getOutput (BATConfig [0,1] 0) ~> 1
        getOutput (BATConfig [0,1] 2) ~> 1
        getOutput (BATConfig [0,1] 1) ~> 1
        getOutput (BATConfig [0,0] 0) ~> 0
    -------------------------------------}
    getOutput (BATConfig boxes _)
      | length boxes <= 1 = 0 -- Assumption that if given a batconfig without a box 1
      | otherwise = b1
      where (_:(b1:_)) = boxes --Because we ignore Box 0


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
{-------------------------------------
  transpose Function

  Function that given an integer and a program, makes JEQ instructions target
  instruction get transposed by given integer.

  Parameters:
    Integer how to much jump the instructions with
    Program which we're transposing the instructions in
  Returns:
    Transposed program
  Example call:
    transpose 1 [INC 1, CLR 4, JEQ 1 0 10] ~> [INC {box = 1},CLR {box = 4},JEQ {box1 = 1, box2 = 0, target = 11}]
  Tests:
    transpose 0 [INC 1, JEQ 1 0 10] ~> [INC {box = 1},JEQ {box1 = 1, box2 = 0, target = 10}]
    transpose 1 [INC 1, CLR 1] ~> [INC {box = 1},CLR {box = 1}]
    transpose 1 [JEQ 1 0 2] ~> [JEQ {box1 = 1, box2 = 0, target = 3}]
    transpose 0 [JEQ 1 0 2] ~> [JEQ {box1 = 1, box2 = 0, target = 2}]
-------------------------------------}
transpose :: Int -> Program -> Program
transpose _ [] = []
transpose x (h:t) = case h of
  JEQ b1 b2 y -> ((JEQ b1 b2 (x+y)):transpose x t)
  _ -> (h:transpose x t)


-- PROBLEM 9. YOUR CODE HERE
-- ---------------------------
-- join two programs together, so as to run one
-- after the other
{-------------------------------------
  (*->*) Function

  Function that concatenates two programs and does that by jumping instructions
  in the second program using the transpose function defined above.

  Parameters:
    Program (first program to start with)
    Program (Second program which is joined to first one after it)
  Returns:
    One program that has all instructions from the two other programs
  Example call:
    (*->*) [INC 1, CLR 2] [INC 4, CLR 7, JEQ 0 0 1] ~> [INC {box = 1},CLR {box = 2},INC {box = 4},CLR {box = 7},JEQ {box1 = 0, box2 = 0, target = 3}]
  Tests:
    (*->*) [] [] ~> []
    (*->*) [INC 1] [] ~> [INC {box = 1}]
    (*->*) [] [INC 1] ~> [INC {box = 1}]
    (*->*) [JEQ 0 0 1] [] ~> [JEQ {box1 = 0, box2 = 0, target = 1}]
    (*->*) [] [JEQ 0 0 1] ~> [JEQ {box1 = 0, box2 = 0, target = 1}]
    (*->*) [INC 1] [JEQ 0 0 1] ~> [INC {box = 1},JEQ {box1 = 0, box2 = 0, target = 2}]
    (*->*) [JEQ 0 0 1] [INC 1] ~> [JEQ {box1 = 0, box2 = 0, target = 1},INC {box = 1}]
-------------------------------------}
(*->*) :: Program -> Program -> Program
p1 *->* p2 = let newP2 = transpose (length p1) p2
             in p1 ++ newP2


-- PROBLEM 10. YOUR CODE HERE
-- ---------------------------
-- program to compute B1 = B1 + B2
{-------------------------------------
  adder Program

  Simply a program that adds boxes 1 and 2 and keeps result of addition in box 1
  Tested using the execute Function
  execute adder [] ~> 0
  execute adder [0] ~> 0
  execute adder [0,0] ~> 0
  execute adder [0,0,0] ~> 0
  execute adder [0,1] ~> 1
  execute adder [1,1] ~> 2
  execute adder [1,1,1000] ~> 2
  execute adder [1,0] ~> 1
-------------------------------------}
adder :: Program
adder = [CLR 0, --empty our thinking box
         JEQ 2 0 5, --Loop condition: Did we reach what box two has?
         INC 0, -- Increase box 0 (Used for checking)
         INC 1, -- Increase box 1 (addition)
         JEQ 0 0 1] --Dummy instruction that replicates the looping mechanism and goes back to check loop condition


-- PROBLEM 11. YOUR CODE HERE
-- ---------------------------
-- create a program to copy the contents of box m to box n (leave box m unchanged)
{-------------------------------------
  copyBox Program

  Assumption made: input never contains any negative numbers

  Only special case here is if box m and n are the same boxes, in this case, we basically return an empty program.

  Program that given box m and box n numbers, it copies content of box m to n leaving m unchanged

  Tests;
    copyBox 1 1 ~> []
    copyBox 1 2 ~> [CLR {box = 2},JEQ {box1 = 1, box2 = 2, target = 4},INC {box = 2},JEQ {box1 = 2, box2 = 2, target = 1}]
    copyBox 2 1 ~> [CLR {box = 1},JEQ {box1 = 2, box2 = 1, target = 4},INC {box = 1},JEQ {box1 = 1, box2 = 1, target = 1}]
    execute (copyBox 1 2) [0,1] ~> 0
    execute (copyBox 2 1) [0,1] ~> 1
    execute (copyBox 2 2) [0,1] ~> 0
    execute (copyBox 1 1) [0,1] ~> 0
-------------------------------------}
copyBox :: Int -> Int -> Program
copyBox m n
  | m == n = []
  | otherwise = [CLR n, --Clear n (Because we're not sure if n starts having more tokens than m)
                 JEQ m n 4, --Loop condition: Stop when m and n are equal
                 INC n, --Increase n
                 JEQ n n 1] --Dummy instruction to return back to check loop condition

-- PROBLEM 12. YOUR CODE HERE
-- ---------------------------
-- program to compute B1 = Bx + By
{-------------------------------------
  addXY Program

  Program that given two box numbers, adds them together and saves resul in box 1

  Assumptions made:
    1) Input never contains any negative numbers.
    2) User doesn't input Box 0 as one of the params.

  This program uses box 0 for calculations in most cases.
  Only case where we use another box for calculation as well is when given input Box 1
  and itself. In this case, Box 2 is used for calculation as well.

  Cases:
    - Given boxes 1 and 2 (in any order), simply return the adder program above
    - Given box 1 as one of the inputs, we take that into consideration while adding because we just need to add the other box to 1
    - Given box 1 and itself. This is the only case where we use an extra box other than box 0 which is in this case box 2 to store the old value of box 1
    - Given any other two boxes, We follow the standard algorithm
  Tests: (using execute function)
    Tests that involve boxes 1 and 2 as input params haven't been repeated here as they're same as adder function test cases
    execute (addXY 1 3) [] ~> 0
    execute (addXY 3 1) [] ~> 0
    execute (addXY 1 3) [1] ~> 1
    execute (addXY 3 1) [1] ~> 1
    execute (addXY 1 3) [1, 0, 2] ~> 3
    execute (addXY 3 1) [1, 0, 2] ~> 3
    execute (addXY 1 1) [1, 0, 2] ~> 2
    execute (addXY 2 2) [0, 2] ~> 4
    execute (addXY 2 2) [1, 3] ~> 6
    execute (addXY 4 2) [1,2,3,4] ~> 6
    execute (addXY 2 4) [1,2,2,4] ~> 6
-------------------------------------}
addXY :: Int -> Int -> Program
addXY 1 2 = adder -- Because we already implemented it
addXY 2 1 = adder
addXY 1 1 = (*->*) (copyBox 1 2) [CLR 0, JEQ 2 0 5, INC 0, INC 1, JEQ 0 0 1]  --Joining of two programs where one is copying old value and other one doing addition
addXY 1 x = [CLR 0, -- Clear the 0th box
             JEQ x 0 5, -- Loop condition
             INC 0, -- Increase box 0
             INC 1, -- Increase box 1
             JEQ 0 0 1] -- Return to test loop condition
addXY x 1 = [CLR 0,
             JEQ x 0 5,
             INC 0,
             INC 1,
             JEQ 0 0 1]
addXY x y = [CLR 1, -- Clear box where we put output
             CLR 0, -- Clear box 0 (thinking box)
             JEQ x 0 6, -- First loop condition
             INC 0, -- Add to thinking box
             INC 1, -- Add to box 1
             JEQ 0 0 2, -- Return to first loop condition
             CLR 0, -- Clear thinking box
             JEQ y 0 11, -- Second loop condition
             INC 0, -- Add to thinking box
             INC 1, -- Add to box 1 (final output)
             JEQ 0 0 7] -- Dummy condition to return to second loop condition

-- END OF TEMPLATE FILE
