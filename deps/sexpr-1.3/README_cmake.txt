Using sexpr library from cmake-based projects:
----------------------------------------------

Presently, the library does not come with a proper FindSexpr.cmake module
file.  In the meantime, you can add the following lines to a CMakeLists.txt
file.  Assuming the source tree for the sexpr library exists in
/Users/matt/Research/sexpr/src  :

INCLUDE_DIRECTORIES(
  /Users/matt/Research/sexpr/src/src
)

LINK_DIRECTORIES(
  /Users/matt/Research/sexpr/src/src
)

And then just add "sexp" to the TARGET_LINK_LIBRARIES list.
