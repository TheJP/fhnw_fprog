bf2x3 = createState (BF "++[->+++<]>.")
main = putStrLn(show (full bf2x3))

data BF = BF String deriving(Show)
data Data = Data [Int] deriving(Show)
data BFPointer = BFPointer Int deriving(Show)
data DataPointer = DataPointer Int deriving(Show)
data BrackStack = BrackStack [Int] deriving(Show)
data BFState = BFState BF Data BFPointer DataPointer BrackStack deriving(Show)

createState::BF->BFState
createState x = BFState x (Data []) (BFPointer 0) (DataPointer 0) (BrackStack [])

listCalc::(Int->Int)->Int->[Int]->[Int]
listCalc f 0 (d:ds) = (f d):ds
listCalc f x (d:ds) = d:(listCalc f (x-1) ds)
listCalc f x [] = listCalc f x [0] --enlarge list if not long enough

(?!!)::[Int]->Int->Int
(d:_) ?!! 0 = d
(_:ds) ?!! x = ds ?!! (x-1)
[] ?!! _ = 0

(?+-)::(Int->Int)->BFState->BFState
(?+-) f (BFState bf (Data ds) bp (DataPointer dp) bs) = BFState bf (Data (listCalc f dp ds)) bp (DataPointer dp) bs
(?+) = (?+-) (+1)
(?-) = (?+-) (+(-1))
(?>)::BFState->BFState
(?>) (BFState bf ds bp (DataPointer dp) bs) = BFState bf ds bp (DataPointer (dp+1)) bs
(?<)::BFState->BFState
(?<) (BFState bf (Data ds) bp (DataPointer 0) bs) = BFState bf (Data (0:ds)) bp (DataPointer 0) bs
(?<) (BFState bf ds bp (DataPointer dp) bs) = BFState bf ds bp (DataPointer (dp-1)) bs
(?/)::BFState->BFState
(?/) (BFState (BF bf) (Data ds) (BFPointer bp) (DataPointer dp) (BrackStack bs))
    | ds ?!! dp == 0 = skip 1 (readd (BrackStack bs))
    | otherwise = readd (BrackStack (bp:bs))
    where
        readd bs = (BFState (BF bf) (Data ds) (BFPointer bp) (DataPointer dp) bs)
        skip 0 x = x
        skip x (BFState (BF bf) ds (BFPointer bp) dp bs)
            | bf !! bp == '[' = skip (x+1) next
            | bf !! bp == ']' = skip (x-1) next
            | otherwise = skip x next
            where next = BFState (BF bf) ds (BFPointer (bp+1)) dp bs
(?\)::BFState->BFState
(?\) (BFState bf (Data ds) (BFPointer bp) (DataPointer dp) (BrackStack (bs:bss)))
    | ds ?!! dp == 0 = next bp bss
    | otherwise = next bs (bs:bss)
    where next bp bs = BFState bf (Data ds) (BFPointer bp) (DataPointer dp) (BrackStack bs)
(?.)::BFState->IO BFState --TODO
(?.) bfs = do putStrLn("1")
              return bfs

step::BFState->BFState
step (BFState (BF bf) ds (BFPointer bp) dp bs) =
    (
    case bf!!bp of
        '+' -> (?+)
        '-' -> (?-)
        '>' -> (?>)
        '<' -> (?<)
        '[' -> (?/)
        ']' -> (?\)
        --'.' -> (?.) TODO: Input/Output
        _   -> \x -> x --skip unkown char
    )
    (BFState (BF bf) ds (BFPointer (bp+1)) dp bs)

full::BFState->BFState
full (BFState (BF bf) ds (BFPointer bp) dp bs)
    | bp < length bf = full (step bfs)
    | otherwise = bfs
    where bfs = (BFState (BF bf) ds (BFPointer bp) dp bs)