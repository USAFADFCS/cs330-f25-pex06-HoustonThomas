import Distribution.Simple.Utils (xargs)
-- pex6.hs 
-- unKnot Haskell

-- name: C2C Houston Thomas

{- DOCUMENTATION: None
-}
unKnot :: [(Char, Char)] -> String
unKnot tripCode
   | null tripCode = "unknot"
   | typeOneMoveExists tripCode = unKnot (makeTypeOneMove tripCode)
   | typeTwoMoveExists tripCode = unKnot (makeTypeTwoMove tripCode)
   | otherwise = "tangle - resulting trip code: " ++ show tripCode

typeOneMoveExists :: [(Char, Char)] -> Bool
typeOneMoveExists [] = False
typeOneMoveExists [_] = False
typeOneMoveExists ((x1, y1):(x2, y2):xs)
   | x1 == x2 = True
   | otherwise = typeOneMoveExists ((x2,y2):xs)

makeTypeOneMove :: [(Char, Char)] -> [(Char, Char)]
makeTypeOneMove [] = []
makeTypeOneMove [x] = [x]
makeTypeOneMove ((x1, y1):(x2, y2):xs)
   | x1 == x2 = xs
   | otherwise = (x1,y1) : makeTypeOneMove ((x2,y2):xs)

typeTwoMoveExists :: [(Char, Char)] -> Bool
typeTwoMoveExists [] = False
typeTwoMoveExists [_] = False
typeTwoMoveExists ((x1,y1):(x2,y2):(x3,y3):(x4,y4):xs)
   | (y1 == y2 && y3 == y4 && y1 /= y3 && ((x1 == x3 && x2 == x4) || (x1 == x4 && x2 == x3))) || (x1 == x2 && x3 == x4 && y1 == y4 && y2 == y3) = True
   | otherwise = typeTwoMoveExists ((x2,y2):xs)

makeTypeTwoMove :: [(Char, Char)] -> [(Char, Char)]
makeTypeTwoMove [] = []
makeTypeTwoMove [x] = [x]
makeTypeTwoMove ((x1,y1):(x2,y2):(x3,y3):(x4,y4):xs)
   | (y1 == y2 && y3 == y4 && y1 /= y3 && ((x1 == x3 && x2 == x4) || (x1 == x4 && x2 == x3))) || (x1 == x2 && x3 == x4 && y1 == y4 && y2 == y3) = xs
   | otherwise = (x1,y1) : makeTypeTwoMove ((x2,y2):(x3,y3):(x4,y4):xs)

main :: IO ()
main = do
   let t01 = [('a','o'),('a','u'),('b','o')]
   print("   typeI test case t01 - tripcode: " )
   print(t01)
   print("   result:" ++ unKnot t01)
   let t02 = [('a','o'),('b','o'),('a','u'),('b','u'),('c','o')]
   print("   typeII test case t02 - tripcode: " )
   print(t02)
   print("   result:" ++ unKnot t02)
   

