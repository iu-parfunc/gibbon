type Label = (String, Pid)
type Pid = Int
type Circuit a = (Int, PList Label, PList Label, PList (State a))
type InPort a = (Pid, Int, a)
type OutPort a = (Int, a, Bool, Int, Bool, Int)
type Packet a = (Pid, Int, a, Bool, Int, Bool, Int, Int)

data State = PS Int Component Int (PList (Pid, Int, Boolean)) (PList (Int, Boolean, Bool, Int, Bool, Int))

data State a = PS Int Component Int (PList (Pid, Int, a)) (PList (Int, a, Bool, Int, Bool, Int))