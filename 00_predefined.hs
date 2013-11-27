{-

data Bool = False | True
data Ordering = LT | EQ | GT
data Char = ... ’a’ | ’b’ ... -- Unicode values
data Int = minBound ... -1 | 0 | 1 ... maxBound --begrentzt
data Integer = ... -1 | 0 | 1 ... --nicht begrentzt
data Float
data Double

Doing 5 * -3 will make GHCI yell at you but doing 5 * (-3) will work just fine.


-}


main = do
	putStr("False && True: ")
	print(False && True)
	
	putStr("not (True && True): ")
	print(not (True && True))
	
	putStr("/= same as [c#]!=: ")
	let a = 6 /= 4
	print(a)
	
	putStr("i++ => succ 8: ")
	print(succ 8)
	
	putStr("min, max => min 8 4: ")
	print(min 8 4)
	