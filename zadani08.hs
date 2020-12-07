import Data.Typeable
{- HTrees, eighth homework from IB015, semester fall 2019
 * comments are for convenience only, please read the full assignment
 * Write your implementation in place of 'undefined'.
   * If you do not implement a function, you can delete or comment it out,
     or leave it defined as 'undefined'.
 * IMPORTANT: Write type signature for ALL GLOBAL functions.
 * IMPORTANT: Before submitting, test your solution on Aisa/Nymfe.
 * Submit your solution into Homework Vault (“odevzdávárna”) of your seminar group.
-}

-- | 'Integer' numbers extended by positive and negative infinity.
data ZEx = NegInf | Z Integer | PosInf deriving (Eq, Ord, Show) -- also Num, see below

-- | Either a "living" 'Integer' value or a closed range of “dead” 'ZEx' values.
data Hanged = Alive Integer | Grave ZEx ZEx deriving (Eq, Show)

-- | Binary tree definition, as used in seminars.
data BinTree a = Empty | Node a (BinTree a) (BinTree a) deriving (Eq, Show)

-- | Binary tree with hanging “living” values and “dead” ones buried in graves.
type HTree = BinTree Hanged

-- | 'BinTree' in-order traversal (left subtree then value then right subtree).
inorder :: BinTree a -> [a]
inorder  Empty       = []
inorder (Node v l r) = inorder l ++ [v] ++ inorder r

-- --------------------------------------------------------------------------- --
--                      DO NOT CHANGE THE TYPES ABOVE                          --
-- --------------------------------------------------------------------------- --


-- | Compare a value with a (correct) 'Hanged' value.
cmpHanged :: ZEx -> Hanged -> Ordering
cmpHanged (Z num)(Alive a) = if num < a then LT else if num > a then GT else EQ
cmpHanged (NegInf)(Alive a) = LT
cmpHanged (PosInf)(Alive a) = GT
cmpHanged (NegInf) (Grave l r)= if l == NegInf then EQ else LT
cmpHanged (PosInf) (Grave l r)= if r == PosInf then EQ else GT
cmpHanged num (Grave l r) = if num < l then LT else if num > r then GT else EQ
--test EQ (cmpHanged (Z 42) (Grave NegInf PosInf))


isNodeAlive :: Hanged -> Bool
isNodeAlive (Alive _) = True
isNodeAlive _ = False
-- | Check if a value is alive in a (correct) 'HTree'.
isAlive :: Integer -> HTree -> Bool
isAlive _ Empty = False
isAlive valToFind (Node nodeVal lTree rTree) =
    let cmp = (cmpHanged (Z valToFind) nodeVal) in
    if cmp == EQ && (isNodeAlive nodeVal)  then True 
    else if cmp == LT then isAlive valToFind lTree
    else isAlive valToFind rTree

cmpNode :: ZEx -> HTree -> Ordering -> Ordering
cmpNode _ Empty LT = LT
cmpNode _ Empty GT = GT
cmpNode num (Node nodeVal _ _) _ = (cmpHanged num nodeVal)

isCorrectList :: HTree -> Bool
isCorrectList Empty = True

isCorrectList (Node (Alive nodeVal) lTree rTree) = 
    if ((cmpNode (Z nodeVal) lTree GT) == GT)  && ((cmpNode (Z nodeVal) rTree LT) == LT) 
    then isCorrectList lTree && isCorrectList rTree 
    else False

isCorrectList (Node (Grave nodeValLeft nodeValRight) lTree rTree) = 
    if ((cmpNode nodeValLeft lTree GT) == GT)  && ((cmpNode nodeValRight rTree LT) == LT)
    && if (lTree == Empty) && (rTree /= Empty) then  (nodeValLeft == NegInf) else True
    && if (rTree == Empty) && (lTree /= Empty) then (nodeValRight == PosInf) else True
    && if nodeValLeft > nodeValRight then False else True
    && if (nodeValLeft == NegInf) || (nodeValRight == PosInf) then nodeValLeft /= nodeValRight else True
    then isCorrectList lTree && isCorrectList rTree 
    else False

--isCorrectList (Node (Alive nodeVal) lTree rTree) = if ((cmpNode (Z nodeVal) lTree GT) == GT) && ((cmpNode (Z nodeVal) rTree LT) == LT) then isCorrectList lTree ++ isCorrectList rTree else [False]

-- Bude se vracet [Bool, Bool, Bool], kde druhy bool bude True, když v sobě obashuje hodnotu -nekonecno a terti bool bude True, kdyz plus nekonecno

-- | Check if an 'HTree' is correct, i.e., has ordered values and contains all
-- numbers from 'ZEx'.
isCorrect :: HTree -> Bool
isCorrect t = undefined
    
-- | Get all values that are alive in a (not necessarily correct) 'HTree'.
--
-- NOTE: We do not test the order of the values.
livingMembers :: HTree -> [Integer]
livingMembers Empty = []
livingMembers (Node (Grave _ _) lTree rTree) = (livingMembers lTree) ++ (livingMembers rTree)
livingMembers (Node (Alive nodeVal) lTree rTree) = (livingMembers lTree) ++ [nodeVal] ++ (livingMembers rTree)

-- | Add a given number to each value in a 'HTree'.
-- The input tree is expected to be correct (you don't need to check)
moveBy :: HTree -> Integer -> HTree
moveBy Empty _ = Empty
moveBy (Node (Alive nodeVal) lTree rTree) addVal = Node (Alive (nodeVal + addVal)) (moveBy lTree addVal) (moveBy rTree addVal)
moveBy (Node (Grave nodeValLeft nodeValRight) lTree rTree) addVal = Node (Grave (nodeValLeft + (Z addVal)) (nodeValRight + (Z addVal))) (moveBy lTree addVal) (moveBy rTree addVal)


-- | Multiply all values by -1 and move values to make it correct again. The
-- input tree is expected to be correct (you don't need to check)
-- The result must be correct and it must hold that @mirror (mirror tree) == tree@.
mirror :: HTree -> HTree
mirror = undefined

-- --------------------------------------------------------------------------- --
-- FUNCTIONS AND CLASSES BELOW THIS LINE ARE OPTIONAL AND YOU CAN IGNORE THEM  --
-- --------------------------------------------------------------------------- --




-- | Make it possible to use 'ZEx' as normal numbers.
--
-- This gives us 'fromIntegral', thus allowing us to write:
--
-- >>> Grave (6 * 7) 64
-- Grave (Z 42) (Z 64)
--
-- It is not a ring as some operations do not make sense on infinities.
--
-- >>> PosInf + NegInf
-- *** Exception: Can not add infinities of different sign
--
instance Num ZEx where
    -- | Addition on extended integers is partial - @('PosInf' + 'NegInf')@.
    (Z x)  + (Z y)  = Z $ x + y
    NegInf + PosInf = PosInf + NegInf
    PosInf + NegInf = error "Can not add infinities of different sign"
    (Z _)  + inf    = inf
    inf    + (Z _)  = inf
    inf    + _inf   = inf

    -- | Negation (or additive inverse) of extended integers.
    negate PosInf = NegInf
    negate NegInf = PosInf
    negate (Z i)  = Z (-i)

    -- | Multiplication of extended integers is partial - @('PosInf' * 0)@.
    (Z x)  * (Z y) = Z $ x * y
    PosInf * z2    = case signum z2 of
                       Z (-1) -> NegInf
                       Z 0    -> error "Can not multiply infinity by 0"
                       Z 1    -> PosInf
                       _      -> error "Impossible: signum not in {-1,0,1}"
    NegInf * z2    = PosInf * (signum z2 * signum NegInf) -- beware of precedence
    z1     * z2    = z2 * z1

    -- | Extract the sign of an extended integer; a mapping to {-1,0,1}.
    signum (Z i)  = Z $ signum i
    signum NegInf = Z (-1)
    signum PosInf = Z 1

    -- | Absolute value of an extended integer.
    abs z = z * signum z

    -- | Convert from 'Integer' to 'ZEx'.
    fromInteger = Z


-- ----------------------------------------------------------------------- --
--                            SIMPLE TESTING                               --
--                                                                         --
-- This is a template for testing. Write your simple tests here and run:   --
-- >>> main                                                                --
-- RUNNING TESTS                                                           --
-- Trying compare:  OK                                                     --
-- ...                                                                     --
--                                                                         --
-- WE DO NOT TEST THIS and it is provided only for your convenience.       --
-- ----------------------------------------------------------------------- --


-- | Test for equality and output OK/FAIL.
test :: (Eq a, Show a) => a -> a -> IO ()
test good mine = if mine == good
         then putStrLn "\tOK"
         else putStrLn ("\tFAIL:\n\t expected: " ++ show good ++ "\n\t got: " ++ show mine)

-- UNCOMMENT AND START TESTING!  -}

-- | Entry point to the program
--
-- As long as you keep IO here you don't need to understand it :)

main :: IO ()
main = do
    putStrLn "RUNNING TESTS"
    putStrLn "Testing tests:"
    test True (not False)
    putStrLn "Testing cmpHanged:"
    --testCompare

    putStrLn "Testing isAlive:"
    --testAlive

    putStrLn "Testing isCorrect:"
    testCorrect

    putStrLn "Testing livingMembers:"
    testLiving

    putStrLn "Testing moveBy:"
    testMoveBy
{-
    putStrLn "Testing mirror:"
    testMirror
    -- your tests can follow
-}
testCompare :: IO ()
testCompare = do
    -- (good) (yours)
    test EQ (cmpHanged (Z 1) (Alive 1))
    test EQ (cmpHanged (Z 42) (Grave NegInf PosInf))
    test LT (cmpHanged (Z 0) (Grave (Z 6) (Z 9)))

testAlive :: IO ()
testAlive = do
    test False (isAlive 0    correct0)
    test False (isAlive 4    correct1)
    test True  (isAlive 4    correct2)
    test False (isAlive 4    correct3)
    test True  (isAlive (-2) correct3)
    test True  (isAlive 1    exampleCorrect)

-- | Since you are reading this, I might as well show you something COOL!
--
-- The code looks like an inverted for cycles and in fact @forM_ = flip mapM_@.
testCorrect :: IO ()
testCorrect = do
    mapM_ (test False . isCorrect) incorrectTrees
    mapM_ (test True  . isCorrect) correctTrees

testLiving :: IO ()
testLiving = do
    putStr "\tThis test does not check order, "
    putStrLn "import Data.List and uncomment the tests."
    --test [11,17] (sort $ livingMembers treeOfDeath)
    --test [1,2,3] (sort $ livingMembers incorrect5)

testMoveBy :: IO ()
testMoveBy = do
    let movedBySeven = threeSeven `moveBy` (-7)
    test True (isCorrect movedBySeven)
    test [-4,0] (livingMembers movedBySeven)
    test moveBySeven movedBySeven
{-
testMirror :: IO ()
testMirror = do
    test mirrorReflected (mirror reflected)
    test mirrorCorrect (mirror exampleCorrect)
 -}
-- ----------------------------------------------------------------------- --
--                              SAMPLE TREES                               --
--                                                                         --
-- Here you will find HTrees assignment and you can plant your own.        --
-- ----------------------------------------------------------------------- --

-- | Lift a value to 'BinTree' with a single value
root :: a -> BinTree a
root a = Node a Empty Empty

-- | Parametrised Hanged value bordering on infinity.
upTill, upFrom :: Integer -> Hanged
upTill n = Grave NegInf (Z n)
upFrom n = Grave (Z n) PosInf

-- ---------
-- Examples
-- ---------

-- | These correct HTrees are examples in the assignment code.
correctTrees :: [HTree]
correctTrees =
    [ exampleCorrect
    , correct0
    , correct1
    , correct2
    , correct3
    , reflected
    , treeOfDeath
    , mirrorCorrect
    , threeSeven
    , moveBySeven
    ]

-- | These incorrect HTrees are examples in the assignment code.
incorrectTrees :: [HTree]
incorrectTrees =
    [ Empty
    , incorrect1
    , incorrect2
    , incorrect3
    , incorrect4
    , incorrect5
    , incorrect6
    ]

-- | First presented 'HTree'.
--
-- >          2
-- >    ┌─────┴─────┐
-- > [-∞,0]       [4,7]
-- >    └───┐   ┌───┴─────┐
-- >        1   3       [8,∞]
--
exampleCorrect :: HTree
exampleCorrect = Node (Alive 2)
                    (Node (upTill 0)
                        Empty
                        (root $ Alive 1))
                    (Node (Grave 4 7)
                        (root (Alive 3))
                        (root $ upFrom 8))

-- | Example from 'inorder'.
--
-- >>> inorderExample
--         3       
--       ┌─┴─┐     
--       2  [4,∞]
--    ┌──┴┐  
-- [-∞,0] 1
-- >>> inorder inorderExample
-- [Grave NegInf (Z 1),Alive 2,Alive 3,Alive 4,Alive 5,Alive 6,Grave (Z 7) PosInf]
--
inorderExample :: HTree
inorderExample = Node (Alive 4)
                      (Node (Alive 2) (root $ upTill 1) (root (Alive 3)))
                      (root $ upFrom 4)

-- | Incorrect tree from 'isCorrect'.
--
-- >       2
-- >    ┌──┴┐
-- > [-∞,3] 4
--
incorrect1 :: HTree
incorrect1 = Node (Alive 2) (root (Grave NegInf 3)) (root (Alive 4))

incorrect11 :: HTree
incorrect11 = Node (Alive 3) (root (Alive 2)) (root (Alive 4))

incorrect111 :: HTree
incorrect111 = Node (Alive 3) (root (Alive 5)) (root (Alive 4))

-- | Incorrect tree from 'isCorrect'.
--
-- >  [3,∞]
-- > ┌──┴──┐
-- > 2     5
--
incorrect2 :: HTree
incorrect2 = Node (Grave 3 PosInf) (root (Alive 2)) (root (Alive 5))

-- | Incorrect tree from 'isCorrect'
--
-- >       [3,4]
-- >    ┌────┴────┐
-- > [-∞,3]     [4,∞]
--
incorrect3 :: HTree
incorrect3 = Node (Grave 3 4) (root (Grave NegInf 3)) (root (Grave 4 PosInf))

-- | Example incorect tree from 'isCorrect'.
--
-- >           2
-- >        ┌──┴──┐
-- >      [4,3] [1,-∞]
-- >   ┌────┘
-- > [∞,5]
--
incorrect4 :: HTree
incorrect4 =
    Node (Alive 2)
        (Node (Grave 4 3)
            (root $ upTill 5)
            Empty)
        (root $ upFrom 1)

-- | Example incorect tree from 'livingMembers'.
--
-- >            2
-- >        ┌───┴────┐
-- >      [5,4]    [0,-∞]
-- >   ┌────┴──┐ ┌───┘
-- > [∞,6]     3 1
--
incorrect5 :: HTree
incorrect5 =
    Node (Alive 2)
        (Node (Grave 5 4)
            (root (Grave PosInf 6))
            (root $ Alive 3))
        (Node (Grave 0 NegInf) (root $ Alive 1) Empty)

-- | Example incorect tree from 'livingMembers'.
--
-- >  2
-- > ┌┴─┐
-- > 2  3
-- >   ┌┘
-- >   3
incorrect6 :: HTree
incorrect6 =
    Node (Alive 2)
        (root $ Alive 2)
        (Node (Alive 3) (root $ Alive 3) Empty)


-- | Correct tree from 'isCorrect'.
--
-- >  [-∞,∞]
correct0 :: HTree
correct0 = root (Grave NegInf PosInf)

correct31 :: HTree
correct31 = root (Grave 2 5)

correct30 :: HTree
correct30 = root (Alive 5)
-- | Correct tree from 'isCorrect'.
--
-- >       [3,4]
-- >    ┌────┴────┐
-- > [-∞,2]     [5,∞]
--
correct1 :: HTree
correct1 = Node (Grave 3 4) (root (Grave NegInf 2)) (root (Grave 5 PosInf))

-- | Correct tree from 'isCorrect'.
--
-- >       4
-- >    ┌──┴──┐
-- > [-∞,3] [5,∞]
--
correct2 :: HTree
correct2 = Node (Alive 4) (root (Grave NegInf 3)) (root (Grave 5 PosInf))

-- | Correct tree from 'isCorrect'.
--
-- >          [-1,1]
-- >        ┌────┴────┐
-- >        -2      [2,∞]
-- >    ┌───┘
-- > [-∞,-3]
--
correct3 :: HTree
correct3 = Node (Grave (-1) 1)
                (Node (Alive (-2))
                      (root $ Grave NegInf (-3))
                       Empty)
                (root $ Grave 2 PosInf)

-- | Example from 'moveBy'.
--
-- >                7
-- >        ┌───────┴───┐
-- >        3         [8,∞]
-- >    ┌───┴───┐
-- > [-∞,2]   [4,6]
--
threeSeven :: HTree
threeSeven = Node
    (Alive 7)
    (Node (Alive 3) (root (Grave NegInf 2)) (root (Grave 4 6)))
    (root (Grave 8 PosInf))

-- | Example from 'moveBy' using threeSeven.
--
-- >                 0
-- >        ┌────────┴──┐
-- >        -4        [1,∞]
-- >    ┌───┴────┐
-- > [-∞,-5]  [-3,-1]
--
moveBySeven :: HTree
moveBySeven = Node (Alive 0)
                (Node (Alive (-4))
                    (root $ upTill (-5))
                    (root $ Grave (-3) (-1)))
                (root $ upFrom 1)


-- | Example from 'livingMembers'.
--
-- >               [12,12]
-- >         ┌────────┴──────────┐
-- >       [1,10]                17
-- >    ┌────┴───┐           ┌───┴────┐
-- > [-∞,0]      11       [13,16]  [18,∞]
--
treeOfDeath :: HTree
treeOfDeath = Node
    (Grave 12 12)
    (Node (Grave 1 10)
          (root (Grave NegInf 0))
          (root (Alive 11)))
    (Node (Alive 17)
          (root (Grave 13 16))
          (root (Grave 18 PosInf)))

-- | Example from 'mirror'.
--
-- >          [-1,1]
-- >        ┌────┴──┐
-- >        -2      2
-- >    ┌───┘       └──┐
-- > [-∞,-3]         [3,∞]
--
reflected, mirrorReflected :: HTree
mirrorReflected = reflected
reflected = Node (Grave (-1) 1)
                 (Node (Alive (-2))
                       (root $ upTill (-3))
                       Empty)
                 (Node (Alive 2)
                       Empty
                       (root $ upFrom 3))


-- | Example from 'mirror'.
--
-- >                 -2
-- >           ┌─────┴─────┐
-- >        [-7,-4]      [0,∞]
-- >    ┌──────┴───┐   ┌───┘
-- > [-∞,-8]       -3  -1
--
mirrorCorrect :: HTree
mirrorCorrect = Node (Alive (-2))
                    (Node (Grave (-7) (-4))
                        (root $ upTill (-8))
                        (root $ Alive (-3)))
                    (Node (upFrom 0)
                        (root $ Alive (-1))
                        Empty)


-- ------------------------------------------------------------------------- --
--                              PRETTY PRINTING                              --
--                                                                           --
-- Beware, the following code does not shy away from monads and evil magics. --
-- ------------------------------------------------------------------------- --

-- | Prettier version of 'Show'.
--
-- It is intended for terminal output,
-- though @pshow@ may not be readable raw.
class Show a => Pretty a where
    -- | Overwrite this with something pretty
    pshow :: a -> String
    pshow = show
    -- | Print pretty string of value to output
    pprint :: a -> IO ()
    pprint = putStr . pshow

instance Pretty Int
instance Pretty Integer
instance Pretty Bool
instance Pretty ()

-- | Print 'ZEx' in Unicode.
instance Pretty ZEx where
    pshow PosInf = "∞"
    pshow NegInf = "-∞"
    pshow (Z i) = show i

-- | Print 'Grave's on inverted background.
instance Pretty Hanged where
    pshow (Alive i) = show i
    pshow (Grave l r) = '[' : pshow l ++ ',' : pshow r ++ "]"

-- | Quality of life printing of 'BinTree'.
-- 
-- If your type does not have 'PointAtBox' either write it,
-- as it is most likely only
-- 
-- > instance PointAtBox TypeIUseInBinTree
--
-- or use the 'HorizontalT' printing
--
-- > pprint (HorizontalT myTree)
--
instance PointAtBox a => Pretty (BinTree a) where
    pprint t = case t of { Empty -> putStrLn "Empty"; branch -> pprint $ VerticalT branch }
    pshow = pshow . VerticalT

-- ---------------------------------------------------------------------------
-- Horizontal printing - good for very large trees
-- ---------------------------------------------------------------------------

-- | Newtype for printing tree each value on separate line.
newtype HorizontalT a = HorizontalT (BinTree a) deriving (Eq, Show)

instance Pretty a => Pretty (HorizontalT a) where
    pshow (HorizontalT t) = treeShow t "" True False

-- | Unicode print horizontal (hard to read).
treeShow :: Pretty a => BinTree a -- ^ binary tree to be showed
                     -> String    -- ^ prefix
                     -> Bool      -- ^ going down == if going down, then up gets prefix
                     -> Bool      -- ^ Unicode printing
                     -> String    -- ^ showed tree
treeShow Empty prefix _ _ = prefix ++ "\n"
treeShow (Node a l r) prefix goingDown unicode = left ++ self ++ right
    where
        -- optional Unicode strings
        space = "    "
        pipe      = if unicode then "│   " else "|   "
        downAngle = if unicode then "└── " else "`-- "
        upAngle   = if unicode then "┌── " else "r-- "
        -- merge it all to one string
        next b = if b then pipe else space
        lPrefix = prefix ++ next goingDown
        rPrefix = prefix ++ next (not goingDown)
        left  = treeShow l lPrefix False unicode
        right = treeShow r rPrefix True unicode
        self  = prefix ++ (if goingDown then downAngle else upAngle) ++ pshow a ++ "\n"

-- ---------------------------------------------------------------------------
-- Vertical printing
-- ---------------------------------------------------------------------------

-- | Depth of a binary tree. ('Empty' has no depth)
depth :: BinTree a -> Int
depth Empty = 0
depth (Node _ l r) = 1 + max (depth l) (depth r)

-- | Newtype for printing tree vertically, each layer on its line.
newtype VerticalT a = VerticalT { vTree :: BinTree a} deriving (Eq, Show)

instance PointAtBox a => Pretty (VerticalT a) where
    pshow = const "use pprint to print tree vertically"
    pprint = printBoxedTree . deleteRightSpace . snd . boxTree . vTree


-- | Box at which you can point well in space.
-- 
-- Example usage:
-- 
-- >        1_000_000_000
-- >     ┌────────┴────────┐
-- >     L                 R
-- 
-- For the above 'BinTree' the box may be used like this:
--
-- > | ... | ... |.| ... | ... |
-- >    1     2   3   4     5   
-- >
-- > 1) leftSpace  - the width of the left subtree (L)
-- > 2) leftSide   - the width up to where it points at L (6 characters)
-- > -> value      - the actual value (1_000_000_000)
-- > 4) rightSide  - like leftSide
-- > 5) rightSpace - like leftSpace
--
-- As 3 is always width 1, it is omitted.
--
data Box a = Box { leftSpace :: Int  -- ^ 1
                 , leftSide :: Int   -- ^ 2
                 , value :: a  
                 , rightSide :: Int  -- ^ 4
                 , rightSpace :: Int -- ^ 5
                 } deriving (Eq, Show)


-- | Values that can be put easily in a 'Box'.
class Show a => PointAtBox a where
    printBoxValue :: Box a -> IO ()
    printBoxValue = putStr . show . value
    valLength :: a -> Int
    valLength = length . show
    packBox :: a -> Box a
    packBox a = let l = valLength a - 1
                in if l < 0 then error "Nothing to point at"
                   else (stdBox a) { leftSide = div l 2
                                   , rightSide = div l 2 + mod l 2
                                   }

-- | You can make your type instance of 'PointAtBox' this easily (see source).
instance PointAtBox Int
instance PointAtBox Integer
instance PointAtBox Bool
instance PointAtBox ()

-- | 'ZEx' is printed prettily.
instance PointAtBox ZEx where
    printBoxValue = pprint . value 
    valLength NegInf = 2
    valLength PosInf = 1
    valLength (Z i) = length (show i)

-- | 'Grave's are printed prettily and need special formatting.
instance PointAtBox Hanged where
    printBoxValue = pprint . value
    valLength (Alive i)   = valLength i
    valLength g           = let b = packBox g in leftSide b + rightSide b
    packBox (Alive i)     = (\(Box a b _ d e) -> Box a b (Alive i) d e) $ packBox i 
    packBox (Grave z1 z2) = (stdBox (Grave z1 z2)) { leftSide = 1 + valLength z1
                                                   , rightSide = 1 + valLength z2}

-- | Just puts the value in a box.
--
-- WARN: Use 'packBox' instead if possible.
stdBox :: a -> Box a
stdBox a = Box {leftSpace = 0, leftSide = 0, value = a, rightSide = 0, rightSpace = 0}

-- | The total length of the 'Box', including the character pointed at.
boxLength :: Box a -> Int
boxLength b = 1 + leftSpace b + leftSide b + rightSide b + rightSpace b

-- | Print line of characters, based on 'Box' parameters.
boxLine :: (Box a -> Int) -> Box a -> Char -> IO ()
boxLine = (.) ((putStr .) . replicate)

-- | Print character in space of 'Box'.
sidesBlank :: Box a -> Char -> IO ()
sidesBlank a c = boxLine leftSide a c >> putStr [c] >> boxLine rightSide a c

sidePointAtBox :: Box a  -> Char -> Char -> Char -> IO ()
sidePointAtBox a l m r = leftIndent >> putStr [m] >> rightIndent
    where
        -- print line of characters left and right of box
        leftIndent  = boxLine (\b -> leftSpace b + leftSide   b) a l
        rightIndent = boxLine (\b -> rightSide b + rightSpace b) a r

pointFromBox :: Box a  -> Char -> Char -> Char -> IO ()
pointFromBox a l m r = boxLine leftSide a l >> putStr [m] >> boxLine rightSide a r

-- | Unicode print line pointing at the 'Box' from side.
leftPointAtBox, rightPointAtBox :: Box a -> IO ()
leftPointAtBox a = sidePointAtBox a ' ' '┌' '─'
rightPointAtBox a = sidePointAtBox a '─' '┐' ' '

-- | Unicode print line pointing from the 'Box' to side(s).
pointFromBoxBoth, pointFromBoxLeft, pointFromBoxRight :: Box a -> IO ()
pointFromBoxBoth  a = pointFromBox a '─' '┴' '─'
pointFromBoxLeft  a = pointFromBox a '─' '┘' ' '
pointFromBoxRight a = pointFromBox a ' ' '└' '─'

-- | Shorthand for @Either () ()@.
data Side = L | R deriving Eq

-- | Print the Unicode lines above values.
printLineLevel :: PointAtBox a => Int -> Side -> BinTree (Box a) -> IO ()
printLineLevel _ _  Empty               = mempty
printLineLevel 0 s (Node b _l _r)       = case s of { L -> leftPointAtBox b; R -> rightPointAtBox b }
printLineLevel _ _ (Node b Empty Empty) = boxLine boxLength b ' '
printLineLevel 1 _ (Node b Empty right) = pointFromBoxRight b     >> printLineLevel 0 R right
printLineLevel 1 _ (Node b left Empty)  = printLineLevel 0 L left >> pointFromBoxLeft b
printLineLevel i _ (Node b left right)  = do
                                            printLineLevel (i - 1) L left
                                            if i == 1 then pointFromBoxBoth b else sidesBlank b ' '
                                            printLineLevel (i - 1) R right

-- | Print the line with values.
printValueLevel :: PointAtBox a => Int -> BinTree (Box a) -> IO ()
printValueLevel _  Empty       = mempty
printValueLevel 0 (Node b _ _) = boxLine leftSpace b ' '   >> printBoxValue b  >> boxLine rightSpace b ' '
printValueLevel i (Node b l r) = printValueLevel (i - 1) l >> sidesBlank b ' ' >> printValueLevel (i - 1) r

-- | Print the Unicode line above and line with values.
printLevel :: PointAtBox a => BinTree (Box a) -> Int -> IO ()
printLevel t i = printLineLevel i R t >> putStrLn "" >> printValueLevel i t >> putStrLn ""

-- | Print the tree vertically, line after line.
printBoxedTree :: PointAtBox a => BinTree (Box a) -> IO ()
printBoxedTree t = do
    printValueLevel 0 t
    putStrLn ""
    mapM_ (printLevel t) (takeWhile ((`deep` t) . (+1)) [1..])

deep :: Int -> BinTree a -> Bool
deep d Empty = d == 0
deep d (Node _ l r) = d == 0 || deep (d - 1) l || deep (d - 1) r 

-- | Put the tree values into boxes, see example at 'Box'.
boxTree :: PointAtBox a => BinTree a -> (Int, BinTree (Box a))
boxTree Empty = (0, Empty)
boxTree (Node a l r) = (boxLength b, Node b boxedL boxedR)
                         where
                            b = b' {leftSpace = lenL, rightSpace = lenR}
                            b' = packBox a
                            (lenL, boxedL) = boxTree l
                            (lenR, boxedR) = boxTree r

-- | Delete right spaces of rightmost branches.
deleteRightSpace :: BinTree (Box a) -> BinTree (Box a)
deleteRightSpace Empty        = Empty
deleteRightSpace (Node b l r) = Node (b { rightSpace = 0 }) l $ deleteRightSpace r
