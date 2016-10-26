//
//  tree_packed.c
//  PackedTrees
//
//  Created by lsakka on 10/25/16.
//  Copyright © 2016 lsakka. All rights reserved.
//

#include "tree_packed.h"

float max(float a, float b){
    return a > b ? a : b;
}

float min(float a, float b){
    return a < b ? a : b;
}

int treeSize(int n){
    return (sizeof(Node_Leaf))* n +  (sizeof(Node_Inner))*n;
}

int comparePointX(const void *a, const void *b){
    if(((Point *)a)->x_val < ((Point *)b)->x_val)
        return -1;
    else if(((Point *)a)->x_val> ((Point *)b)->x_val)
        return 1;
    else
        return 0;
}

int comparePointY(const void *a, const void *b)
{
    if(((Point *)a)->y_val < ((Point *)b)->y_val)
        return -1;
    else if(((Point *)a)->y_val > ((Point *)b)->y_val)
        return 1;
    else
        return 0;
}
//return the index of the last created leaf node
void buildTreeRec(int startIndx ,int endIndx ,Point * data ,char * &cur ,int depth  ){
    
    int size = endIndx - startIndx + 1;
    int mid  = (startIndx + endIndx) / 2;
    
    //leaf node
    if (size ==1) {
        
        * ((char*)cur)=LEAF_TAG;
        cur +=sizeof(char);
        
        //set x val
        *((float*)cur) = data[startIndx].x_val;
        cur += sizeof(float);
        
        //set y val
        * ((float*)cur)=data[endIndx].y_val;
        cur += sizeof(float);
        
        //skip the output (// do we need to create a new tree ? just for this feild !!)
        *cur +=sizeof(int);
        return ;
    }
    
    //inner Node
    
    //this implementation has some derefernecing , not sure how much geeting rid of them will is better
    //since we are intereted in the traversal performance not the built its ok for now
    else if(size>1){
        * ((char*)cur)=INNER_TAG;
        cur +=sizeof(char);
        
        
        //we can do it with out innerData by tracking cur step like the leaf , mm shal we?
        Node_Inner * innerData= (Node_Inner *)cur;
        //splitAxis
        innerData->splitAxis= (depth%2 == 0);
        //cur_dat-> +=sizeof(bool);
        
        //splitLoc
        //Todo : no need to resort each time! still this is not part of what we are trying to optimize now (rather than restoring keep two sorted arrays (x, y))
        
        if(depth%2==0){
            qsort(&data[startIndx], size, sizeof(Point), comparePointX);
            innerData->splitLoc=data[mid].x_val;
            
        }
        else{
            qsort(&data[startIndx], size, sizeof(Point), comparePointY);
            innerData->splitLoc = data[mid].y_val;
            
        }
        
        cur += sizeof(Node_Inner);
        
        char * leftChild = cur;
        
        buildTreeRec(startIndx, mid, data, cur, depth+1);
        
        innerData->RightChild=cur;
        
        buildTreeRec(mid+1, endIndx, data, cur, depth+1);
        
        
        
        //any way to write these in a better way !
        
        if(*leftChild==LEAF_TAG){
            if(* innerData->RightChild==LEAF_TAG){
                innerData->max_x=max(*(float *)(leftChild+1),*(float *)(innerData->RightChild+1));
                innerData->min_x=min(*(float *)(leftChild+1),*(float *)(innerData->RightChild+1));
                innerData->max_y=max(*(float *)(leftChild+2),*(float *)(innerData->RightChild+2));
                innerData->min_y=min(*(float *)(leftChild+2),*(float *)(innerData->RightChild+2));
                
            }else{
                innerData->max_x=max(*(float *)(leftChild+1),((Node_Inner*) (innerData->RightChild+1))->max_x);
                innerData->min_x=min(*(float *)(leftChild+1),((Node_Inner*) (innerData->RightChild+1))->min_x);
                innerData->max_y=max(*(float *)(leftChild+2),((Node_Inner*) (innerData->RightChild+1))->max_y);
                innerData->min_y=min(*(float *)(leftChild+2),((Node_Inner*) (innerData->RightChild+1))->min_y);
                
            }
        }else{
            if(* innerData->RightChild==LEAF_TAG){
                innerData->max_x=max(((Node_Inner*) (leftChild+1))->max_x,*(float *)(innerData->RightChild+1));
                innerData->min_x=min(((Node_Inner*) (leftChild+1))->min_x,*(float *)(innerData->RightChild+1));
                innerData->max_y=max(((Node_Inner*) (leftChild+1))->max_y,*(float *)(innerData->RightChild+2));
                innerData->min_y=min(((Node_Inner*) (leftChild+1))->min_y,*(float *)(innerData->RightChild+2));
                
            }else{
                innerData->max_x=max(((Node_Inner*) (leftChild+1))->max_x,((Node_Inner*) (innerData->RightChild+1))->max_x);
                innerData->min_x=min(((Node_Inner*) (leftChild+1))->min_x,((Node_Inner*) (innerData->RightChild+1))->min_x);
                innerData->max_y=max(((Node_Inner*) (leftChild+1))->max_y,((Node_Inner*) (innerData->RightChild+1))->max_y);
                innerData->min_y=min(((Node_Inner*) (leftChild+1))->min_y,((Node_Inner*) (innerData->RightChild+1))->min_y);
            }
        }
    }
    
    
}

char *  buildTree(int n , Point * data ){
    //reserve the memory layout
    int bytes = treeSize(n);
    char * buf = (char*) malloc(bytes);
    buildTreeRec(0, n-1, data, buf, 0);
    
    return buf;
    
}

