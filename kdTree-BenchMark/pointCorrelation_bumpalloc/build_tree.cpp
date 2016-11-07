//
//  tree_packed.c
//  PackedTrees
//
//  Created by lsakka on 10/25/16.
//  Copyright © 2016 lsakka. All rights reserved.
//

#include "tree_bumpalloc.h"
#include <assert.h>

char* BumpAllocator::heap_ptr= (char *)malloc(20000000 * sizeof(Node_Inner));

void readPoint(FILE *in, Point & p){
    int dummy;
    if(fscanf(in, "%d", &dummy) != 1) {
        fprintf(stderr, "Input file not large enough.\n");
        exit(1);
    }
    
    if(fscanf(in, "%f", &p.x_val) != 1) {
        fprintf(stderr, "Input file not large enough.\n");
        exit(1);
    }
    if(fscanf(in, "%f", &p.y_val) != 1) {
        fprintf(stderr, "Input file not large enough.\n");
        exit(1);
    }
    
}
void readInput(int argc, char **argv,Point * & data , float & rad, int & npoints){
    FILE *in;
    
    if(argc != 4 && argc != 3) {
        fprintf(stderr, "usage: pointcorr <DIM> <rad> <npoints> [input_file]\n");
        exit(1);
    }
    
    
    rad = atof(argv[1]);
    
    npoints = atol(argv[2]);
    
    if(npoints <= 0) {
        fprintf(stderr, "Not enough points.\n");
        exit(1);
    }
    
    data = new Point[npoints];
    
    if(argc == 4) {
        in = fopen(argv[3], "r");
        /*
         if(in == NULL) {
         fprintf(stderr, "Could not open %s\n", argv[4]);
         exit(1);
         }
         */
        for(int i = 0; i < npoints; i++) {
            readPoint(in, data[i]);
        }
        fclose(in);
    } else {
        //generate random points ( no file name provided)
        srand(0);
        for(int i = 0; i < npoints; i++) {
            data[i].x_val= (float)rand() / RAND_MAX;
            data[i].y_val= (float)rand() / RAND_MAX;
            
        }
    }
}


float max(float a, float b){
    return a > b ? a : b;
}

float min(float a, float b){
    return a < b ? a : b;
}

int treeSize(int n){
    return (sizeof(Node_Leaf))* n +  (sizeof(Node_Inner))*n + (2*n)*sizeof(char * )+100;
}


int comparePointX(const void *a, const void *b){
    if(((Point *)a)->x_val < ((Point *)b)->x_val)
        return -1;
    else if(((Point *)a)->x_val> ((Point *)b)->x_val)
        return 1;
    else
        return 0;
}

int comparePointY(const void *a, const void *b){
    if(((Point *)a)->y_val < ((Point *)b)->y_val)
        return -1;
    else if(((Point *)a)->y_val > ((Point *)b)->y_val)
        return 1;
    else
        return 0;
}



//return the index of the last created leaf node
Tree_Node * buildTreeRec(int startIndx ,int endIndx ,Point * data ,int depth  ){
    
    int size = endIndx - startIndx + 1;
    int mid  = (startIndx + endIndx) / 2;
    
    //leaf node
    if (size ==1) {
        
        Tree_Node * node= (Tree_Node *)(BumpAllocator::alloc(sizeof(Tree_Node)));
        node->tag=LEAF_TAG;
        
        node->leafData.x_val=data[startIndx].x_val;
        node->leafData.y_val=data[startIndx].y_val;
        return node;
    }
    else if(size>1){
        Tree_Node * node= (Tree_Node *)BumpAllocator::alloc(sizeof(Tree_Node));
        node->tag=INNER_TAG;
        node->innerData.splitAxis= (depth%2 == 0);
        
        if(depth%2==0){
            qsort(&data[startIndx], size, sizeof(Point), comparePointX);
            node->innerData.splitLoc=data[mid].x_val;
        }
        else{
            qsort(&data[startIndx], size, sizeof(Point), comparePointY);
            node->innerData.splitLoc = data[mid].y_val;
        }
        
        node->innerData.leftChild = buildTreeRec(startIndx, mid, data, depth+1);
        
        node->innerData.rightChild=buildTreeRec(mid+1, endIndx, data, depth+1);
        
        if(node->innerData.leftChild->tag==LEAF_TAG){
            if(node->innerData.rightChild->tag==LEAF_TAG){
                node->innerData.max_x=max(node->innerData.leftChild->leafData.x_val,node->innerData.rightChild->leafData.x_val);
                node->innerData.min_x=min(node->innerData.leftChild->leafData.x_val,node->innerData.rightChild->leafData.x_val);
                node->innerData.max_y=max(node->innerData.leftChild->leafData.y_val,node->innerData.rightChild->leafData.y_val);
                node->innerData.min_y=min(node->innerData.leftChild->leafData.y_val,node->innerData.rightChild->leafData.y_val);
                
            }else{
                node->innerData.max_x=max(node->innerData.leftChild->leafData.x_val,node->innerData.rightChild->innerData.max_x);
                node->innerData.min_x=min(node->innerData.leftChild->leafData.x_val,node->innerData.rightChild->innerData.min_x);
                node->innerData.max_y=max(node->innerData.leftChild->leafData.y_val,node->innerData.rightChild->innerData.max_y);
                node->innerData.min_y=min(node->innerData.leftChild->leafData.y_val,node->innerData.rightChild->innerData.min_y);
            }
        }
        else{
            if(node->innerData.rightChild->tag==LEAF_TAG){
                node->innerData.max_x=max(node->innerData.leftChild->innerData.max_x,node->innerData.rightChild->leafData.x_val);
                node->innerData.min_x=min(node->innerData.leftChild->innerData.min_x,node->innerData.rightChild->leafData.x_val);
                node->innerData.max_y=max(node->innerData.leftChild->innerData.max_y,node->innerData.rightChild->leafData.y_val);
                node->innerData.min_y=min(node->innerData.leftChild->innerData.min_y,node->innerData.rightChild->leafData.y_val);
            }else{
                node->innerData.max_x=max(node->innerData.leftChild->innerData.max_x,node->innerData.rightChild->innerData.max_x);
                node->innerData.min_x=min(node->innerData.leftChild->innerData.min_x,node->innerData.rightChild->innerData.min_x);
                node->innerData.max_y=max(node->innerData.leftChild->innerData.max_y,node->innerData.rightChild->innerData.max_y);
                node->innerData.min_y=min(node->innerData.leftChild->innerData.min_y,node->innerData.rightChild->innerData.min_y);
            }
        }
        
        return node;
    }
    else{
        assert(false &&"error!");
    }
}

Tree_Node *  buildTree(int n , Point * data ){
    
    return    buildTreeRec(0, n-1, data, 0);
    
}



