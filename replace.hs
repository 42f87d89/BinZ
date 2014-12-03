replace' (a,b) c = if c == a then b else c

replace _ [] = []
replace n (x:xs) = (replace' n x):(replace n xs)

replaces [] xs = xs
replaces (n:ns) xs = replaces ns (replace n xs)
