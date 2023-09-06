import Clock
import DistributedSet
--import IO

main :: IO ()
main = do
    let localtime0 = Clock.step 0 $ Clock.init 0
    let set0 = DistributedSet.init (Clock.stamp 0 localtime0) "H"
    putStrLn "Step 0 ---"
    putStr "Clock:" 
    print localtime0
    putStr "Set:" 
    print set0
    putStr "Value: "
    print $ DistributedSet.value set0
    putStrLn "\n"

    let localtime1 = Clock.step 0 localtime0
    let set1 = DistributedSet.add (Clock.stamp 0 localtime1) "e" set0
    putStrLn "Step 1 ---"
    putStr "Clock:" 
    print localtime1
    putStr "Set:" 
    print set1
    putStr "Value: "
    print $ DistributedSet.value set1
    putStrLn "\n"

    let localtime2 = Clock.step 0 localtime1
    let set2 = DistributedSet.add (Clock.stamp 0 localtime2) "l" set1
    putStrLn "Step 2 ---"
    putStr "Clock:" 
    print localtime2
    putStr "Set:" 
    print set2
    putStr "Value: "
    print $ DistributedSet.value set2
    putStrLn "\n"

    let localtime3 = Clock.step 0 localtime2
    let set3 = DistributedSet.add (Clock.stamp 0 localtime3) "l" set2
    putStrLn "Step 3 ---"
    putStr "Clock:" 
    print localtime3
    putStr "Set:" 
    print set3
    putStr "Value: "
    print $ DistributedSet.value set3
    putStrLn "\n"

    let localtime4 = Clock.step 0 localtime3
    let set4 = DistributedSet.add (Clock.stamp 0 localtime4) "o" set3
    putStrLn "Step 4 ---"
    putStr "Clock:" 
    print localtime4
    putStr "Set:" 
    print set4
    putStr "Value: "
    print $ DistributedSet.value set4

