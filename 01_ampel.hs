data Color = Red | Yellow | Green deriving ( Show )
data Ampel = Stop | Wait | Go deriving ( Show )

f1::Color->Ampel
f1 Red = Stop
f1 Yellow = Wait
f1 Green = Go

f2::Ampel->Color
f2 Stop = Red
f2 Wait = Yellow
f2 Go = Green

main = print (f10 Green)

f4::Color->Ampel
f4 Red = Stop
f4 Yellow = f4 Red
f4 Green = Go

f5::Color->Ampel
f5 Red = Stop
f5 Yellow = f5 Yellow

{-

f4
Domain = {Red, Yellow, Green}
Source = Color = {Red, Yellow, Green}
-> total function, partial function

f5
Domain = {Red}
Source = Color = {Red, Yellow, Green}
-> non-total function, partial function

-}

zweiplus = (+) 2

f6_1::Color->Ampel
f6_1 Red = Stop
f6_1 Green = Go

--Variable input
f6::Color->Ampel
f6 Yellow = Wait
f6 c = f6_1 c

--Fail function
--f7::Color->Ampel
--f7 c = f6_1 c
--f7 Yellow = Wait

f8::Color->Ampel
f8 Green = Go
f8 c = Stop

f9::Color->Ampel
f9 Green = Go
f9 _ = Stop --Wildcard (_ anstatt c, damit keine Variable gesetzt wird)

f10 Green = 'g'
f10 Red = 'r'
f10 Yellow = 'y'

police::Color->Ampel->Bool
police Red Go = False
police Yellow Go = False
police _ _ = True

(&&&) True True = True
(&&&) _ _ = False

(|||) False False = False
(|||) _ _ = True

data Rectangle = R Double Double deriving ( Show )
data Circle = C Double deriving ( Show )
data Square = S Double deriving ( Show )
data Triangle = T Double deriving ( Show )
data Figure = Fc Circle | Fs Square | Ft Triangle | Fr Rectangle deriving ( Show )

area::Figure->Double
area (Fc (C c)) = c*c*pi
area (Fs (S s)) = s*s
area (Ft (T t)) = (t*t*sqrt 3) / 4.0
area (Fr (R a b)) = a*b


data Figure2 = Circle2 Double | Rectangle2 Double Double deriving ( Show )
area2::Figure2->Double
area2 (Circle2 c) = c*c*pi
area2 (Rectangle2 a b) = a*b

maybenothing::Maybe a -> Maybe b -> Maybe (a,b)
maybenothing (Just a) (Just b) = Just (a,b)
maybenothing _ _ = Nothing

addv::(Double,Double)->(Double,Double)->(Double,Double)
addv (x1,x2) (y1,y2) = (x1+y1, x2+y2)

(+&)::(Double,Double)->(Double,Double)->(Double,Double)
(+&) x y = addv x y

mulv::Double->(Double,Double)->(Double,Double)
mulv f (x1, x2) = (f*x1,f*x2)

(*&)::Double->(Double,Double)->(Double,Double)
(*&) x y = mulv x y

dotv::(Double,Double)->(Double,Double)->Double
dotv (x1,x2) (y1,y2) = x1*y1 + x2*y2

(.&)::(Double,Double)->(Double,Double)->Double
(.&) x y = dotv x y

negv::(Double,Double)->(Double,Double)
negv x = mulv (-1) x

normv::(Double,Double)->Double
normv (x1,x2) = sqrt(x1*x1+x2*x2)

subv::(Double,Double)->(Double,Double)->(Double,Double)
subv x y = addv (negv y) x

distancev::(Double,Double)->(Double,Double)->Double
distancev x y = normv (subv x y)

twice f x = f(f x)
twice2 f x = (f . f) x
twice3 f = (f . f)

addC x y = x + y
-- ::a->b->c
addP (x, y) = x + y
-- ::(a,b)->c

addP2 (x,y,z) = x+y+z

curry1::((a,b)->c)->a->b->c
curry1 f a b = f (a,b)

uncurry1::(a->b->c)->(a,b)->c
uncurry1 f (a,b) = f a b

id1::a->a
id1 a = a

g1 f g = \x -> (f x, g x)
g2 f g = \x -> f x + g x

either1 f _ (Left x) = f x
either1 _ g (Right y) = g y

solve03 :: Double -> Double -> Double -> Maybe (Double, Double)
solve03 a b c = if disk < 0 then Nothing else Just (factor + disk, factor - disk)
    where
        disk = b^2 - 4*a*c
        factor = (-b)/(2*a)
		
solve01 = let a = 3
              b = 2::Double
		  in a+b
          
sort (x,y) = (min x y, max x y)
sort02 (x,y) = if x < y then (x, y) else (y, x)

data ZeroOneTwo a = Zero | One a | Two a a deriving (Show)
solve04 :: Double -> Double -> Double -> ZeroOneTwo Double
solve04 a b c
    | disk < 0  = Zero
    | disk == 0 = One factor
    | otherwise = Two (factor + disk) (factor - disk)
    where
        disk = b^2 - 4*a*c
        factor = (-b)/(2*a)

sort03 :: Ord a => (a,a)->(a,a)
sort03 (x,y) = (min x y, max x y)
bubbleSort :: Ord a => (a, a, a) -> (a, a, a)
bubbleSort (x,y,z) = let
                        (x1,y1) = sort03(x,y)
                        (y2,c) = sort03(y1,z)
                        (a,b) = sort03(x1,y2)
                     in (a,b,c)
            
bubbleSort02 :: Ord a => (a, a, a) -> (a, a, a)         
bubbleSort02 (x,y,z)
    | x <= y && y <= z = (x,y,z)
    | y <= x && x <= z = (y,x,z)
    | x <= z && z <= y = (x,z,y)
    | z <= x && x <= y = (z,x,y)
    | y <= z && z <= x = (y,z,x)
    | z <= y && y <= x = (z,y,x)
    
--bubbleSort03 :: Ord a => (a, a, a) -> (a, a, a) 
--bubbleSort03 (x,y,z)
--    | y <= z = if x <= y then (x,y,z) else (y,x,z)
--    | z <= y = if x <= z then (x,z,y) else (z,x,y)
--    | y <= x = if y <= z then (y,z,x) else (z,y,x)

sqrtF :: Double -> Maybe Double
sqrtF x
    | x < 0 = Nothing
    | otherwise = Just (sqrt x)
recipF :: Double -> Maybe Double
recipF x
    | x == 0 = Nothing
    | otherwise = Just (recip x)

(>=>) :: (Eq b) => (b -> Maybe c)->(a -> Maybe b)->(a->Maybe c)
(>=>) f1 f2 x
    | f2 x == Nothing = Nothing
    | otherwise = f1(unNothinging(f2(x)))
    where
        unNothinging :: (Maybe a)->a
        unNothinging (Just x) = x

(>=>>) f1 f2 x =
    case (f2 x) of
        Just y -> f1 y
        Nothing -> Nothing
        
data CoolType = E1 | E2 | E3

instance Eq CoolType where
    E1 == E1 = True
    E2 == E2 = True
    E3 == E3 = True
    _  ==  _ = False
    
data Mod3 = Z0 | O1 | T2
toNum Z0 = 0
toNum O1 = 1
toNum T2 = 2
instance Num Mod3 where
    fromInteger a
        | a `mod` 3 == 0 = Z0
        | a `mod` 3 == 1 = O1
        | a `mod` 3 == 2 = T2
    a + b = fromInteger ((toNum a) + (toNum b))
    a - b = fromInteger ((toNum a) - (toNum b))
    a * b = fromInteger ((toNum a) * (toNum b))
    abs a = a
    signum Z0 = 0
    signum _ = 1
    
instance Show Mod3 where
    show Z0 = "0"
    show O1 = "1"
    show T2 = "2"
    
lambda a = (\x -> a + x, \y -> a-y)

fib::(Num a, Num b, Eq a)=>a->b
fib 0 = 0
fib 1 = 1
fib x = fib(x-1)+fib(x-2)

faq::(Num a, Eq a)=>a->a
faq 0 = 1
faq n = n * (faq (n-1))

data List a = Nil | Cons a (List a) deriving (Show)

m_head::List a->a
m_head (Cons x _) = x
m_tail::List a->List a
m_tail (Cons _ x) = x
m_init::List a->List a
m_init (Cons x Nil) = Nil
m_init (Cons x y) = Cons x (m_init y)
m_last::List a->a
m_last (Cons x Nil) = x
m_last (Cons x y) = m_last y

m_null::List a->Bool
m_null Nil = True
m_null _ = False

m_length::List a->Int
m_length Nil = 0
m_length (Cons _ x) = 1 + (m_length x)

(!!!)::List a->Int->a
(Cons x _) !!! 0 = x
(Cons _ y) !!! x = y !!! (x-1)

m_replicate::Int->a->List a
m_replicate 1 x = Cons x Nil
m_replicate x y = Cons y (m_replicate (x-1) y)

m_prepend::List a->List a->List a
m_prepend Nil        x   = x
m_prepend (Cons a b) x   = Cons a (m_prepend b x)

m_apend::List a->List a->List a
m_apend a b = m_prepend b a

m_reverse::List a->List a
m_reverse (Cons a Nil) = Cons a Nil
m_reverse (Cons a b) = m_prepend (m_reverse b) (Cons a Nil)


m_reverseIttr::List a->List a
m_reverseIttr x = m_reverseHelp x Nil
    where    
        m_reverseHelp::List a->List a->List a
        m_reverseHelp Nil xs = xs
        m_reverseHelp (Cons a bs) xs = m_reverseHelp bs (Cons a xs)

m_max::Ord a=>List a->a
m_max (Cons x Nil) = x
m_max (Cons x y) = max x $ m_max y

m_sum::Num a=>List a->a
m_sum Nil = 0
m_sum (Cons x y) = x + m_sum y

m_rep::(Integral a)=>a->b->List b
m_rep 0 _ = Nil
m_rep x y = Cons y (m_rep (x-1) y)

m_map::(a->b)->[a]->[b]
m_map _ [] = []
m_map f (x:xs) = (f x):(m_map f xs)

m_filter::(a->Bool)->[a]->[a]
m_filter _ [] = []
m_filter f (x:xs) = if (f x) then x:(m_filter f xs) else (m_filter f xs)

m_take::(Integral a)=>a->[b]->[b]
m_take 0 _ = []
m_take _ [] = []
m_take x (y:ys) = y:(m_take (x-1) ys)

m_drop::(Integral a)=>a->[b]->[b]
m_drop 0 x = x
m_drop _ [] = []
m_drop x (y:ys) = (m_drop (x-1) ys)

m_zip::[a]->[b]->[(a,b)]
m_zip [] _ = []
m_zip _ [] = []
m_zip (x:xs) (y:ys) = (x,y):(m_zip xs ys)

m_concat::[[a]]->[a]
m_concat [] = []
m_concat ([]:xs) = m_concat xs
m_concat ((x:xs):ys) = x:(m_concat (xs:ys)) 

--Monads
--usage:
--  • grandfathers (persons!!0)
--  • father (persons!!0)
--  • mother (persons!!0)
--                   Name   Father Mother
data Person = Person String Person Person | Noone deriving (Show)
persons = [Person "A" (Person "A.F" (Person "A.F.F" Noone Noone) Noone) (Person "A.M" (Person "A.M.F" Noone Noone) Noone), Person "B" Noone Noone, Person "C" Noone Noone]
father::Person->Maybe Person
father (Person _ Noone _) = Nothing
father (Person _ x _) = Just x --Just == return in this case
mother::Person->Maybe Person
mother (Person _ _ Noone) = Nothing
mother (Person _ _ x) = return x

grandfathers::Person->Maybe (Person, Person)
grandfathers p = father p >>= father >>= \gff -> mother p >>= father >>= \gmf -> return (gff,gmf)