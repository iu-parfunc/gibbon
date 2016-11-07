//
//  tree_unpacked.h
//  PackedTrees
//
//  Created by lsakka on 10/27/16.
//  Copyright © 2016 lsakka. All rights reserved.
//

#ifndef tree_unpacked_h
#define tree_unpacked_h
#include <cstdlib>
#include <cstdint>
#include <cfloat>
#include <cmath>
#include <iostream>
#include <fstream>
#include <iostream>
#define LEAF_TAG 'l'
#define INNER_TAG 'i'



class BumpAllocator{
public:
   static char* heap_ptr ;
 
    static char * alloc(int n){
        char * ret=heap_ptr;
        BumpAllocator:: heap_ptr += n;
        return ret;
    }
    
  
};




using namespace std;

struct Point{
    float x_val;
    float y_val;
};

struct __attribute__((packed))  Node_Leaf{
    float x_val;
    float y_val;
    //out
    int   corr;
};
struct Tree_Node;



struct __attribute__((packed))  Node_Inner{
    bool     splitAxis;
    float    splitLoc;
    float min_x;
    float max_x;
    float min_y;
    float max_y;
    Tree_Node *   rightChild;
    Tree_Node *   leftChild;
};

struct __attribute__((packed))  Tree_Node{
    char tag; //"i" vs "l"
    union{
        Node_Leaf leafData;
        Node_Inner innerData;
    };
};

extern int counter;

void readInput(int argc, char **argv,Point * & data , float & rad, int & npoints);
Tree_Node *  buildTree(int n , Point * data );
void performPointCorr_OnTree(Point & p,Tree_Node * node ,float rad);
void   printUnpackedTree(Tree_Node * node);
int performPointCorr_IntOut(Point & p,Tree_Node * node ,float rad);

#endif /* tree_unpacked_h */
