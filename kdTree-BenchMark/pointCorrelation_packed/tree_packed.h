
#include <cstdlib>
#include <cstdint>
#include <cfloat>
#include <cmath>
#include <iostream>
#include <fstream>
#define LEAF_TAG 'l'
#define INNER_TAG 'i'
#define SIZE_OF_LEAF sizeof(char ) +sizeof(Node_Leaf)
#define SIZE_OF_INNER sizeof(char ) +sizeof(Node_Inner)

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
    float min_x;
    float max_x;
    float min_y;
    float max_y;
    char *   RightChild;
};


void readPoint(FILE *in, Point &p);
void readInput(int argc, char **argv,Point * & data , int & rad, int & npoints);
char *  buildTree(int n , Point * data );
char *  printPackedTree(char *  cur);


/*TREE_H_*/
