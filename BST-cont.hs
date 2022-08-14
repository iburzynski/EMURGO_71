fromSorted :: [a] -> Tree a
fromSorted xs = foldl' mkTree Empty (divide 1 xs)
    where mkTree l (n:ns) = Node l n (fromSorted ns)
          divide _ [] = []
          divide c xs = take c xs : divide (c*2) (drop c xs)
--                      [1] : divide 2 [2, 3, 4, 5, 6, 7]
--                            [2, 3] : divide 4 [4, 5, 6, 7]
--                                     [4, 5, 6, 7] : divide 8 []
--                                                    []
--                      [[1], [2, 3], [4, 5, 6, 7]]
