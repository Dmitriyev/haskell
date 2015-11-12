module Lab2 where  

data BinaryTree = Empty | Node BinaryTree Integer BinaryTree 

-- Создание пустого дерева 
emptyTree :: BinaryTree
emptyTree = Empty

-- Добавления элемента 
insert :: BinaryTree -> Integer -> BinaryTree
insert (Empty) a = Node Empty a Empty
insert (Node left x right) a = if a < x then a = Node left x (insert right a)
    else if a > x then Node right x (insert left a)
    else if a = x then Node left a right

    Status API Training Shop Blog About Pricing 

