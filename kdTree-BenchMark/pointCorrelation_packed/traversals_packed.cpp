//
//  traversals_packed.c
//  PackedTrees
//
//  Created by lsakka on 10/26/16.
//  Copyright © 2016 lsakka. All rights reserved.
//

#include <iostream>
#include "tree_packed.h"
#include <cassert>

using  namespace std;

char *  printPackedTree(char *  cur){
    //print Tag
    
    cout<<"address:"<<(void * )cur;
    cout<<" ,tag:"<<*(char*) (cur)<<endl;
    if(*((char*) (cur))==LEAF_TAG){
        cur +=sizeof(char);
        struct Node_Leaf * leaf=(struct Node_Leaf*) (cur);
        cout<<"x:"<<leaf->x_val<<" ,y:"<<leaf->y_val<<endl;
        cur+=sizeof(Node_Leaf);
        return cur;
    }
    else{
        cur +=sizeof(char);
        struct Node_Inner * inner=(struct Node_Inner*) (cur);
        cout<<" ,splitAxis:"<<inner->splitAxis<<" ,splitLocation:"<<inner->splitLoc<<endl;
        cout<<" ,min_x:"<<inner->min_x;
        cout<<" ,max_x:"<<inner->max_x<<endl;
        cout<<" ,min_y:"<<inner->min_y;
        cout<<" ,max_y:"<<inner->max_y<<endl;
        cout<<" ,right child address:"<<(void * )inner->RightChild<<endl;;
        cur+=sizeof(Node_Inner);
        
        char * rightChild=printPackedTree(cur);
        assert(rightChild==inner->RightChild && "error in right child address in the built tree\n");
        return printPackedTree(rightChild);
    }
    
}