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
                    
                
        