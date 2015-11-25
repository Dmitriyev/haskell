module Lab2 where  

data BinaryTree = Empty | Node BinaryTree Integer BinaryTree 

-- Создание пустого дерева 
emptyTree :: BinaryTree
emptyTree = Empty

-- Добавления элемента 
insert :: BinaryTree -> Integer -> BinaryTree
insert (Empty) a = Node Empty a Empty
insert (Node left x right) a
    | a > x = Node left x (insert right a)
    | a < x = Node (insert left a) x right
    | a == x = Node left x right

--Удаление элемента
remove :: BinaryTree -> Integer -> BinaryTree
remove (Empty) a = Empty
remove (Node left x right) a
    | a > x = Node left x (remove right a)
    | a < x = Node (remove left a) x right
    | a == x =
        if isEmpty right
        then left
        else Node left leftmost right'
            where
                isEmpty Empty = True
                isEmpty _ = False
                (leftmost, right') = deleteleftmost right
                    where
                        deleteleftmost (Node Empty x right) = (x, right)
                        deleteleftmost (Node left x right) = deleteleftmost left

--Поиск элемента
containsElement :: BinaryTree -> Integer -> Bool
containsElement (Empty) a  = False
containsElement (Node left x right) a 
    | a == x = True
    | a < x  = containsElement left a
    | a > x  = containsElement right a

--Поиск в дереве наименьшего элемента, который больше или равен заданному
nearestGE :: BinaryTree -> Integer -> Integer
nearestGE Empty x = error "No such element"
nearestGE (Node left x right) a
    | a == x = a
    | a > x = if isEmpty right then error "No such element" else nearestGE right a 
    | a < x = findMoreThenA'ButLessThenX left x a
        where
            isEmpty Empty = True
            isEmpty _ = False
            findMoreThenA'ButLessThenX (Node left b right) x a 
                | b < a = a
                | b > a = findMoreThenA'ButLessThenX left a b

--Создание дерева из списка
treeFromList :: [Integer] -> BinaryTree
treeFromList [] = emptyTree
treeFromList (h : t) = insert (treeFromList t) h

--Создания списка из дерева
listFromTree :: BinaryTree -> [Integer]
listFromTree Empty=[]
listFromTree (Node left x right) = (listFromTree left ++ [x]) ++ listFromTree right

--Проверка
test = listFromTree (emptyTree `insert` 1 `insert` 2 `insert` 4 `insert` 3 `remove`4 `insert` 9 `insert` 12 `insert` 14)
a = treeFromList test
b = nearestGE a 11

main = do
    putStrLn(show test)
    putStrLn(show b)