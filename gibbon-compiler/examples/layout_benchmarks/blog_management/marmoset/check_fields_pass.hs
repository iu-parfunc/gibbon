data Recursive = Constructor (Recursive) (Float) (Int)

foo :: Recursive -> Recursive 
foo rec = case rec of 
    Constructor rst fl val -> let newRst = foo rst 
			      in Constructor (newRst) (fl) (val+1)
