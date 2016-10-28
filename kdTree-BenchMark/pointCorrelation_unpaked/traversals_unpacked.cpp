//
//  traversals_unpacked.cpp
//  PackedTrees
//
//  Created by lsakka on 10/27/16.
//  Copyright © 2016 lsakka. All rights reserved.
//

#include <stdio.h>
#include "tree_unpacked.h"
//#define TEST

void   printUnpackedTree(Tree_Node * node){
    //print Tag
    
  
    cout<<" ,tag:"<<node->tag<<endl;
    
    if(node->tag==LEAF_TAG){
        cout<<"x:"<<node->leafData.x_val<<" ,y:"<<node->leafData.y_val<<endl;
    }
    else{
        cout<<" ,splitAxis:"<<node->innerData.splitAxis<<" ,splitLocation:"<<node->innerData.splitLoc<<endl;
        cout<<" ,min_x:"<<node->innerData.min_x;
        cout<<" ,max_x:"<<node->innerData.max_x<<endl;
        cout<<" ,min_y:"<<node->innerData.min_y;
        cout<<" ,max_y:"<<node->innerData.max_y<<endl;

       printUnpackedTree(node->innerData.leftChild);
       printUnpackedTree(node->innerData.rightChild);
    }
    
}
void performPointCorr_OnTree(Point & p,Tree_Node * node ,float rad){
    
    if(node->tag == LEAF_TAG){
        float d = 0;
        
        d +=(p.x_val - node->leafData.x_val) *(p.x_val - node->leafData.x_val);
        d +=(p.y_val - node->leafData.y_val) *(p.y_val - node->leafData.y_val);
        
        if(sqrt(d) < rad){
            node->leafData.corr++;
#ifdef TEST
            counter++;
#endif
        }
    }else {
        
        float sum    = 0.0;
        float boxsum = 0.0;
        float center_x  =
        ( node->innerData.max_x + node->innerData.min_x )/ 2;
        float boxdist_x  =
        ( node->innerData.max_x - node->innerData.min_x)/ 2;
        
        float dist_x    = p.x_val - center_x;
        sum    += dist_x * dist_x;
        boxsum += boxdist_x * boxdist_x;
        //do same thing for y
        
        float center_y  =
        ( node->innerData.max_y + node->innerData.min_y )/ 2;
        
        float boxdist_y  =
        ( node->innerData.max_y - node->innerData.min_y)/ 2;
        
        
        float dist_y    = p.y_val - center_y;
        sum    += dist_y * dist_y;
        boxsum += boxdist_y * boxdist_y;
        
        bool canCorrelate = sqrt(sum) - sqrt(boxsum) < rad;
        
        if(!(canCorrelate)){
            return ;
            
            
        }else{
            //call left
            performPointCorr_OnTree(p, node->innerData.leftChild, rad);
            
            //call right child
            performPointCorr_OnTree(p,  node->innerData.rightChild, rad);
            
            
        }
        
        
    }
    
}