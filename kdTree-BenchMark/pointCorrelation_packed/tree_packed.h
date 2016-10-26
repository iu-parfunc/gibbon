
#include <cstdlib>
#include <cstdint>
#include <cfloat>
#include <cmath>
#include <iostream>
#include <fstream>
#define LEAF_TAG 0
#define INNER_TAG 1

using namespace std;


// Tag can be a bool , 0=leaf , 1=inner
// leaf     [Tag Node_Leaf]
// non-leaf [Tag Node_Inner [LeftChild....][RightChild...]
// index of right child = index of last left decenedent + 1

//used to represent the input  data read from file and used to build the tree i
struct Point{
    float x_val;
    float y_val;
};

struct Node_Leaf{
    float x_val;
    float y_val;
    //out 
    int   corr;
};


//Tree Data Structure
struct Node_Inner{
    bool     splitAxis; // 0:'x' , 1:'y'
    float    splitLoc;
    //define rectangle that includes all points in this subtree
    int min_x;
    int max_x;
    int min_y;
    int max_y;
    char *   RightChild;
};



/*TREE_H_*/
