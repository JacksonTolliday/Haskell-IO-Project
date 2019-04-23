import Data.List
import Data.Char (toLower)

{-
    Write and submit a Haskell program (distribution.hs) that computes and displays 
    the distribution of characters in a given sample of text.
    
    Output of your program should look like this:
    
    Please enter a string of text (the bigger the better): 
    The rain in Spain stays mainly in the plain.
    The distribution of characters in "The rain in Spain stays mainly in the plain." is:
    iiiiii
    nnnnnn
    aaaaa
    sss
    ttt
    ee
    hh
    ll
    pp
    yy
    m
    r
    
    Notice about this example:
    * The text: 'The rain ... plain' is provided by the user as input to your program.
    * Uppercase characters are converted to lowercase
    * Spaces and punctuation marks are ignored completely.
    * Characters that are more common appear first in the list.
    * Where the same number of characters occur, the lines are ordered alphabetically. 
      For example, in the printout above, the letters e, h, l, p and y both occur twice 
      in the text and they are listed in the output in alphabetical order.
    * Letters that do not occur in the text are not listed in the output at all.
-}


main = do
    putStrLn "Please enter a string of text (the bigger the better):"
    sentence <- getLine
    putStrLn $ "The distribution of characters in " ++ (show sentence) ++ " is:"
    let usal = map toLower sentence
    let uses = sort (filter (\x -> elem x ['a'..'z']) (usal))
    let hmm = uses
    setup hmm 'a'

setup hmm 'a' = do
    let tuplfile = []
    count hmm 'a' tuplfile
    
count x 'z' tuplfile = do
    let fx = (filter (\el -> 'z' `elem` x) x)
    if null fx 
        then printit tuplfile
        else countit x fx 'z' tuplfile

count x y tuplfile = do
    let fx = (filter (\el -> y == el) x)
    if null fx 
        then count x (succ y) tuplfile
        else countit x fx y tuplfile

countit x fx y tuplfile = do
    let newtuplfile = quicksort (((length fx), (y)):tuplfile)
    if y == 'z' 
        then printit newtuplfile
        else count x (succ y) newtuplfile
    
printit tuplfile = do
    let n = map (\x -> (replicate (fst x) (snd x))) tuplfile
    mapM_ putStrLn n

customsort x = (reverse (sort (x)))

quicksort :: (Ord a, Ord b) => [(a, b)] -> [(a, b)]
quicksort [] = []
quicksort ((x, y):xs) =
    let smallerSorted = quicksort [(a, b) | (a, b) <- xs, if a == x then b > y else a < x]
        biggerSorted = quicksort [(a, b) | (a, b) <- xs, if a == x then b < y else a > x]
    in  biggerSorted ++ [(x, y)] ++ smallerSorted