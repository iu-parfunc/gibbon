module IntOrderedList where
import Common
import Clock

data IntOrderedNode = Bin Clock Int Timestamp Timestamp IntOrderedNode IntOrderedNode
                    | Tip

singleton :: Int -> IntOrderedNode

insert :: Int -> IntOrderedNode -> Timestamp -> Timestamp -> IntOrderedNode

remove :: Timestamp -> IntOrderedNode -> Timestamp -> Timestamp -> IntOrderedNode

value :: IntOrderedNode -> Intlist