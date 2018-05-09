{-# LANGUAGE ParallelListComp #-}

module GaussElim where

type Number = Double


-- Each row has the corresponding constant term
type Row = ([Number],Number)

rowHead (row,b) = head row
rowTail (row,b) = (tail row,b)
rowCons x (row,b) =  (x:row,b)

type Col = [Number]


-- Solve Ax = b
linearSolve (a,b) = 
    gaussElim (a,b)
    |> toUpper
    |> backSubst

gaussElim (a,b) =
    make(a,b)
    |> lift (map normalize)
    |> descend clearColumn
    |> top

-- Create a vector with the same dimensions as the given
-- vector but with all entries set to the given constant
-- Example: if v = [1,2,3], then constVec 0 v = [0,0,0]
constVec x v = map (const x) v

-----------------------------------------------------------------------

-- Create a Matrix structure
make :: ([[Number]],[Number]) -> Matrix
make (a,b)
    | pairwise length (==) a = Matrix { prev = [], curr = rows }
    | otherwise = error "Matrix A is not square"
    where
    rows = [ (row,x) | row <- a | x <- b ]

-- Convert the internal Matrix structure back to (a,b)
revert :: Matrix -> ([[Number]],[Number])
revert = unzip . curr
    
-- assumes upperTriangular with unit diagonal
-- Only the non-zero parts of each row are given
-- Example: a 3x3 upper triangular matrix would be:
--          [ [a12,a13], [a23], [] ]
backSubst :: [Row] -> [Number]
backSubst upper = foldr formula [] upper
    where
    formula (row,b) xs = (b - dot row xs) : xs

-- Given a matrix that is upper triangular, convert the diagonal
-- to all ones, remove the zeros from each row and return a list
-- of partial rows
toUpper :: Matrix -> [Row]
toUpper = map row . map normalize . toSlices
    where
    normalize sl = sl { row = (map (/ d) r, b/d) }
        where
        (r,b) = row sl
        d = diag sl

    
descend :: ([Row] -> [Row]) -> Matrix -> Matrix
descend f dz =
    if isLast dz then dz
    else descend f (down (lift f dz))

lift :: ([Row] -> [Row]) -> Matrix -> Matrix
lift f dz = dz { curr = f (curr dz) }

toSlices :: Matrix -> [Slice]
toSlices dz =
    if isLast dz then reverse (prev dz)
    else toSlices (down dz)

normalize :: Row -> Row
normalize (row,b) =  (map (/ mx) row, b/mx)
    where
    mx = maximum (map abs row)

-----------------------------------------------------------------------
-- Gauss elimination
-----------------------------------------------------------------------

mulRow :: Number -> Row -> Row
mulRow s (row,b) = (map (\x -> s*x) row, s*b)

addRows :: Row -> Row -> Row
addRows (row1,b1) (row2,b2)  = (zipWith (+) row1 row2,b1+b2)

choosePivotRow :: Row -> Row -> (Row,Row)
choosePivotRow row1 row2 =
    if abs(rowHead row1) > abs(rowHead row2) then (row1,row2)
    else (row2,row1)

clearFirstInRow :: Row -> Row -> (Row,Row)
clearFirstInRow pivotRow row =
    (pivotRow,addRows (mulRow s pivotRow) row)
    where
    s = -(rowHead row)/(rowHead pivotRow)

clearColumn = swapMap1 clearFirstInRow . swapMap1 choosePivotRow
    
-----------------------------------------------------------------------
-- List functions
-----------------------------------------------------------------------

swapMap1 :: (Row -> Row -> (Row,Row)) -> [Row] -> [Row]
swapMap1 f [] = []
swapMap1 f (x:xs) = x' : xs'
    where
    (x',xs') = swapMap f x xs
        
swapMap f a [] = (a,[])
swapMap f a (oldx:oldxs) = (newa,newx:newxs)
    where
    (curra,newx) = f a oldx
    (newa,newxs) = swapMap f curra oldxs

pairwise f p (r1:r2:rs) = p (f r1) (f r2) && pairwise f p (r2:rs)
pairwise f p other = True

-----------------------------------------------------------------------
-- Slice manipulation
-----------------------------------------------------------------------

data Slice = Slice
    { diag :: Number
    , row :: Row
    , col :: Col
    }
    deriving Show

cutSlice :: [Row] -> (Slice,[Row])
cutSlice curr =
    let
        row1 = head curr
        rows = tail curr
        col1 = map rowHead rows
        rest = map rowTail rows
    in
        (Slice { diag = rowHead row1
               , row = rowTail row1
               , col = col1
               }
        , rest )

pasteSlice :: Slice -> [Row] -> [Row]
pasteSlice sl sub =
    let
        row1 = rowCons (diag sl) (row sl)
        rows = zipWith rowCons (col sl) sub
    in
        row1 : rows

-----------------------------------------------------------------------
-- Slice navigation
-----------------------------------------------------------------------

data Matrix = Matrix
    { prev :: [Slice]
    , curr :: [Row]
    }

instance Show Matrix where
    show m = show (curr (top m))
    
isEmpty [] = True
isEmpty other = False

isFirst :: Matrix -> Bool
isFirst = isEmpty . prev

isLast :: Matrix -> Bool
isLast = isEmpty . curr

down :: Matrix -> Matrix
down m =
    if isLast m then m
    else Matrix { prev = slice : (prev m), curr = rows }
        where
        (slice,rows) = cutSlice (curr m)

up :: Matrix -> Matrix
up m = case prev m of
    [] -> m
    u:us -> Matrix { prev = us, curr = pasteSlice u (curr m) }

top :: Matrix -> Matrix
top m =
    if isFirst m then m
    else top (up m)

-----------------------------------------------------------------------
-- Num functions
-----------------------------------------------------------------------

matmul rows vec = map (dot vec) rows

dot v1 v2 = sum[x1*x2 | x1 <-v1 | x2 <- v2]

-- Approximate to the given number of decimals (prec >= 0)
approx :: Int -> Number -> Number
approx prec x =
    let
        tol = 10^prec
        up t = t*tol
        dn t = t/tol
    in
        x
        |> up
        |> round
        |> fromIntegral
        |> dn

-- Convenience operator for creating "forward pipes"
(|>) x f = f x
infixl 1 |>
