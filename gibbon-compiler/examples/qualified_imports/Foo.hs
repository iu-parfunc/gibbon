module Foo where 

import qualified Bar as Sq 
import qualified Baz 

gibbon_main = (Sq.foo 2, Baz.foo 3)

main = print gibbon_main