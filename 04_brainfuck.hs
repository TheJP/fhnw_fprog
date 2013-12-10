import Data.Char
import Bottles
bf2x3 = BF "++[->+++<]>."
bf6x10and5 = BF "++++++[->++++++++++<]>+++++."
bfABC = BF "A:++++++[->++++++++++<]>+++++Loop:<+++++[->.+<]"
bfBottles = BF bottles
bfABC2 = BF ",[+.,]"
bfMusicMaker = BF "++[->+++++++[->++++++>+<<]<]>>[->.<]"
main = do bf <- getLine
          is <- getLine
          let bfResult = full (createState2 (BF bf) is)
          --putStrLn(show bfResult)
          printBF bfResult
              

data BF = BF String deriving(Show)
data Data = Data [Int] deriving(Show)
data BFPointer = BFPointer Int deriving(Show)
data DataPointer = DataPointer Int deriving(Show)
data BrackStack = BrackStack [Int] deriving(Show)
data BFIO = BFIO ([Char],[Char]) deriving(Show)
data BFState = BFState BF Data BFPointer DataPointer BrackStack BFIO deriving(Show)

createState::BF->BFState
createState x = BFState x (Data []) (BFPointer 0) (DataPointer 0) (BrackStack []) (BFIO ([],[]))

createState2::BF->[Char]->BFState
createState2 x y = BFState x (Data []) (BFPointer 0) (DataPointer 0) (BrackStack []) (BFIO (y,[]))

listCalc::(Int->Int)->Int->[Int]->[Int]
listCalc f 0 (d:ds) = (f d):ds
listCalc f x (d:ds) = d:(listCalc f (x-1) ds)
listCalc f x [] = listCalc f x [0] --enlarge list if not long enough

(?!!)::[Int]->Int->Int
(d:_) ?!! 0 = d
(_:ds) ?!! x = ds ?!! (x-1)
[] ?!! _ = 0

(?+-)::(Int->Int)->BFState->BFState
(?+-) f (BFState bf (Data ds) bp (DataPointer dp) bs io) = BFState bf (Data (listCalc f dp ds)) bp (DataPointer dp) bs io
(?+) = (?+-) (+1)
(?-) = (?+-) (+(-1))
(?>)::BFState->BFState
(?>) (BFState bf ds bp (DataPointer dp) bs io) = BFState bf ds bp (DataPointer (dp+1)) bs io
(?<)::BFState->BFState
(?<) (BFState bf (Data ds) bp (DataPointer 0) bs io) = BFState bf (Data (0:ds)) bp (DataPointer 0) bs io
(?<) (BFState bf ds bp (DataPointer dp) bs io) = BFState bf ds bp (DataPointer (dp-1)) bs io
(?/)::BFState->BFState
(?/) (BFState (BF bf) (Data ds) (BFPointer bp) (DataPointer dp) (BrackStack bs) io)
    | ds ?!! dp == 0 = skip 1 (readd (BrackStack bs))
    | otherwise = readd (BrackStack (bp:bs))
    where
        readd bs = (BFState (BF bf) (Data ds) (BFPointer bp) (DataPointer dp) bs io)
        skip 0 x = x
        skip x (BFState (BF bf) ds (BFPointer bp) dp bs io)
            | bf !! bp == '[' = skip (x+1) next
            | bf !! bp == ']' = skip (x-1) next
            | otherwise = skip x next
            where next = BFState (BF bf) ds (BFPointer (bp+1)) dp bs io
(?\)::BFState->BFState
(?\) (BFState bf (Data ds) (BFPointer bp) (DataPointer dp) (BrackStack (bs:bss)) io)
    | ds ?!! dp == 0 = next bp bss
    | otherwise = next bs (bs:bss)
    where next bp bs = BFState bf (Data ds) (BFPointer bp) (DataPointer dp) (BrackStack bs) io
(?.)::BFState->BFState
(?.) (BFState bf (Data ds) bp (DataPointer dp) bs (BFIO (is,os))) = BFState bf (Data ds) bp (DataPointer dp) bs (BFIO (is,(chr (ds?!!dp)):os))
(?:)::BFState->BFState
(?:) (BFState bf ds bp dp bs (BFIO (is,os))) =
    case is of
        [] -> bfs 0 []
        i:nis -> bfs (ord i) nis
    where addData::Int->Data->DataPointer->Data
          addData i (Data ds) (DataPointer x) = Data (listCalc (\_ -> i) x ds)
          bfs::Int->[Char]->BFState
          bfs i nis = (BFState bf (addData i ds dp) bp dp bs (BFIO (nis,os)))
          

printBF::BFState->IO ()
printBF (BFState bf ds bp dp bs (BFIO (is,os))) = putStrLn (reverse os)

step::BFState->BFState
step (BFState (BF bf) ds (BFPointer bp) dp bs io) =
    (
    case bf!!bp of
        '+' -> (?+)
        '-' -> (?-)
        '>' -> (?>)
        '<' -> (?<)
        '[' -> (?/)
        ']' -> (?\)
        '.' -> (?.)
        ',' -> (?:)
        _   -> \x -> x --skip unkown char
    )
    (BFState (BF bf) ds (BFPointer (bp+1)) dp bs io)

full::BFState->BFState
full (BFState (BF bf) ds (BFPointer bp) dp bs io)
    | bp < length bf = full (step bfs)
    | otherwise = bfs
    where bfs = (BFState (BF bf) ds (BFPointer bp) dp bs io)