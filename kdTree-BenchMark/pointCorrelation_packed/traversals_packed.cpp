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
#define TEST
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

void performPointCorr_OnTree(Point & p,char *  cur,float rad){
    
    if(*cur == LEAF_TAG){
        cur++;
        float d = 0;
        float leaf_x = *((float *)cur) ;
        cur += sizeof(float);
        float  leaf_y = *((float *)cur );
        cur += sizeof(float);
        
        d +=(p.x_val - leaf_x) *(p.x_val - leaf_x);
        d +=(p.y_val - leaf_y) *(p.y_val - leaf_y);
        
        if(sqrt(d) < rad){
            (*(int * )cur)++;
            #ifdef TEST
            counter++;
            #endif
            
        }
        cur += sizeof(int);
        return;
        
    }else {
        cur++;

        //well the performance be effected if at this point
        //cur is casted to pointer of Node_Inner and members where accessed using ->?? should try it!
        
        float sum    = 0.0;
        float boxsum = 0.0;
        cur += sizeof(int);
        cur += sizeof(float);
        float center_x  =
        ( (*(float*) cur) + *(float*)( cur+sizeof(float)) )/ 2;
        float boxdist_x  =
        ( -(*(float*) cur) + *(float*)( cur+sizeof(float)) )/ 2;
        
        float dist_x    = p.x_val - center_x;
        sum    += dist_x * dist_x;
        boxsum += boxdist_x * boxdist_x;
        //do same thing for y
        cur += sizeof(float)+sizeof(float);

        float center_y  =
        ( (*(float*) cur) + *(float*)( cur+sizeof(float)) )/ 2;
        
        float boxdist_y  =
        ( -(*(float*) cur) + *(float*)( cur+sizeof(float)) )/ 2;
        
        float dist_y    = p.y_val - center_y;
        sum    += dist_y * dist_y;
        boxsum += boxdist_y * boxdist_y;
        cur += sizeof(float)+sizeof(float);

        bool canCorrelate = sqrt(sum) - sqrt(boxsum) < rad;
       
        if(!(canCorrelate)){
            return ;

        
        }else{
            //call left
            performPointCorr_OnTree(p, cur+sizeof(char *), rad);
            
            //call right child
            performPointCorr_OnTree(p, (*(char * *)cur), rad);

            
        }
        
        
    }
    
}

int performPointCorr_IntOut(Point & p,char *  cur,float rad){
    
    if(*cur == LEAF_TAG){
        cur++;
        float d = 0;
        float leaf_x = *((float *)cur) ;
        cur += sizeof(float);
        float  leaf_y = *((float *)cur );
        cur += sizeof(float);
        
        d +=(p.x_val - leaf_x) *(p.x_val - leaf_x);
        d +=(p.y_val - leaf_y) *(p.y_val - leaf_y);
        
        if(sqrt(d) < rad){
         //   (*(int * )cur)++;
            return 1;
        }
        cur += sizeof(int);
        return 0 ;
        
    }else {
        cur++;
        
        //well the performance be effected if at this point
        //cur is casted to pointer of Node_Inner and members where accessed using ->?? should try it!
        
        float sum    = 0.0;
        float boxsum = 0.0;
        cur += sizeof(bool);
        cur += sizeof(float);
        float center_x  =
        ( (*(float*) cur) + *(float*)( cur+sizeof(float)) )/ 2;
        float boxdist_x  =
        ( -(*(float*) cur) + *(float*)( cur+sizeof(float)) )/ 2;
        
        float dist_x    = p.x_val - center_x;
        sum    += dist_x * dist_x;
        boxsum += boxdist_x * boxdist_x;
        //do same thing for y
        cur += sizeof(float)+sizeof(float);
        
        float center_y  =
        ( (*(float*) cur) + *(float*)( cur+sizeof(float)) )/ 2;
        
        float boxdist_y  =
        ( -(*(float*) cur) + *(float*)( cur+sizeof(float)) )/ 2;
        
        float dist_y    = p.y_val - center_y;
        sum    += dist_y * dist_y;
        boxsum += boxdist_y * boxdist_y;
        cur += sizeof(float)+sizeof(float);
        
        bool canCorrelate = sqrt(sum) - sqrt(boxsum) < rad;
        
        if(!(canCorrelate)){
            return 0;
            
            
        }else{
            return performPointCorr_IntOut(p, cur+sizeof(char *), rad)+ performPointCorr_IntOut(p, (*(char * *)cur), rad);
            
        }
        
        
    }
    
}