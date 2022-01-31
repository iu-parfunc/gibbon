#include <cstdlib>
#include <emmintrin.h>
#include <fstream>
#include <smmintrin.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <sys/types.h>
#include <time.h>
#include <limits.h>
#include <iostream>
#include <vector>
#include <mutex>
#include <bits/types/struct_timespec.h>
#include <math.h>
#include <immintrin.h>
#include <mmintrin.h>

typedef float DataType;
typedef char* CursorTy;
typedef char TagTyPacked;

std::mutex mylock;

#define NODE_SIZE 4
#define LANE_LEN  8
#define MAX_BLOCK_SIZE 100

int loopLength;

DataType CONSTANT = 1.0001;

typedef struct CursorIntProd_struct {
    CursorTy field0;
    DataType    field1;
} CursorIntProd;

unsigned isA(CursorTy node);
bool isB(CursorTy node);
DataType fib_recursive(DataType n);
CursorTy PrintTree(CursorTy cur);
CursorTy _cilk(CursorTy packedData);
CursorTy dfs_recurse(CursorTy packedData);
DataType doMath(DataType number, DataType constantVal);
CursorIntProd createBalancedTree(CursorTy out, DataType n);
CursorIntProd createFibonacciTree(CursorTy out, DataType n);
void bfs_parallel(std::vector<CursorTy> Blocks);
void bfs_vectorized(std::vector<CursorTy> Blocks);
double difftimespecs(struct timespec* t0, struct timespec* t1);
void forloopVectorized(CursorTy bufferIn, DataType size);

// DataType doMath(DataType number, DataType constantVal){

//     //multiply for 100,000 times
//     for (int i=0; i<=loopLength; i+=1){

//         number *= constantVal;
//         //number *= constantVal;
//         //number *= constantVal;        
//         //number *= constantVal;        
//         //number *= constantVal;        
//         //number *= constantVal;        
//         //number *= constantVal;        
//         //number *= constantVal;        
//         //number *= constantVal; 
//         //number *= constantVal;

//     }

//     return number;
// }

double difftimespecs(struct timespec* t0, struct timespec* t1)
{
    return (double)((t1->tv_sec * 1000000000.0) - (t0->tv_sec * 1000000000.0))
      + ((double)(t1->tv_nsec - t0->tv_nsec));
}

CursorIntProd createFibonacciTree(CursorTy out, DataType n){
    
    CursorIntProd tmp;

    //create a leaf Node
    if (n <= 0) {

        *out  ='A';             
        out += NODE_SIZE;          
        *(DataType *) out = 1;  
        out += NODE_SIZE;             
        return (CursorIntProd) {out, 2*NODE_SIZE };
    }
    //else create a non-leaf Node
    else if (n > 0) {
        
        *out = 'B';
        CursorTy out0 = out + NODE_SIZE;  
        *(DataType *) out0 = n;     
        out0 += NODE_SIZE;

        //write an indirection pointer to the right subtree
        //*out0 = 'C';
        //out0 += NODE_SIZE;
        //CursorTy indirectionPointer = out0;
        //out0 += NODE_SIZE;

        //rec 1
        tmp = createFibonacciTree(out0, (n - 1));
        CursorTy out1 = tmp.field0;
        DataType size1   = tmp.field1;

        //rec 2
        tmp = createFibonacciTree(out1, (n - 2));
        CursorTy out2 = tmp.field0;
        DataType size2   = tmp.field1;
        
        //write address to the right subtree 
        //(out 1) points to the right subtree
        //*(CursorTy*) indirectionPointer = out1; 

        return (CursorIntProd) {out2, NODE_SIZE + size1 + size2 /*+ 2*NODE_SIZE*/ };  

    }
}

CursorIntProd createBalancedTree(CursorTy out, DataType n){

    CursorIntProd tmp;    
    
    //create a leaf node
    if (n <= 0) {

        *out  ='A';             
        out += NODE_SIZE;          
        *(DataType *) out = 1;  
        out += NODE_SIZE;             
        return (CursorIntProd) {out, 2*NODE_SIZE };
    }
    //create a non-leaf node
    else if (n > 0) {
        
        *out = 'B';
        CursorTy out0 = out + NODE_SIZE;  
        *(DataType *) out0 = n;     
        out0 += NODE_SIZE;

        //write an indirection pointer
        //*out0 = 'C';
        //out0 += NODE_SIZE;
        //CursorTy indirectionPointer = out0;
        //out0 += NODE_SIZE;

        //rec 1
        tmp = createBalancedTree(out0, (n - 1));
        CursorTy out1 = tmp.field0;
        DataType size1   = tmp.field1;

        //rec 2
        tmp = createBalancedTree(out1, (n - 1));
        CursorTy out2 = tmp.field0;
        DataType size2   = tmp.field1;
        
        //write address to the right subtree
        //*(CursorTy*) indirectionPointer = out1;

        return (CursorIntProd) {out2, 2*NODE_SIZE + size1 + size2 /*+ 2*NODE_SIZE*/ };

    }
}

// a recursive implementation of the Fibonacci series
// DataType fib_recursive(DataType n)
// {
//     if (n <= 1)
//         return n;
//     return fib_recursive(n-1) + fib_recursive(n-2);
// }

//recurive depth first function
CursorTy dfs_recurse(CursorTy packedData){

    CursorTy tmp;
    CursorTy dataCursor = packedData + NODE_SIZE;
    
    //encountered a leaf node
    if(isA(packedData)){

        DataType data = *((DataType*) dataCursor);

        //DataType modifiedData = doMath(data, CONSTANT);
        
        for (int i=0; i<=loopLength; i+=1){
            data *= CONSTANT;
        }

        *((DataType*) dataCursor) = data;

        CursorTy newCursor = dataCursor + NODE_SIZE;

        return newCursor;
    }
    //non leaf node
    else if ( isB(packedData) ){
        
        //child 1
        CursorTy child1 = dataCursor + NODE_SIZE;
        tmp = dfs_recurse(child1);
        //child 2
        tmp = dfs_recurse(tmp);

        return tmp;
    }

}

//parallel implementation using cilk
// CursorTy _cilk(CursorTy packedData){

//     CursorTy tmp;
//     CursorTy dataCursor = packedData + NODE_SIZE;
    
//     //encountered a leaf node
//     if( isA(packedData) ){
        
//         DataType data = *((DataType*) dataCursor);
//         DataType modifiedData = doMath(data, CONSTANT);
//         *((DataType*) dataCursor) = modifiedData;

//         CursorTy newCursor = dataCursor + NODE_SIZE;
//         return newCursor;
//     }
//     //non leaf node
//     else if ( isB(packedData) ){

//         CursorTy child1 = dataCursor + 3*NODE_SIZE;
//         CursorTy child2 =  *((CursorTy*) (dataCursor + 2*NODE_SIZE));

//         tmp = /*cilk_spawn*/ _cilk(child1);
//         tmp = /*cilk_spawn*/ _cilk(child2);
//         //cilk_sync;

//         return tmp;
//     }

// }

// void bfs_parallel(CursorTy* Blocks, int length){

//     CursorTy newBlocks[100];

//     //To compare with Label A
//     __m256i label_compareA = _mm256_set_epi32(65, 65, 65, 65, 65, 65, 65, 65);

//     //To compare with Label B
//     __m256i label_compareB = _mm256_set_epi32(66, 66, 66, 66, 66, 66, 66, 66);


//     //#pragma omp parallel for
//     for(int i=0; i<length; i++){
    
//         //encountered a leaf node
//         if( isA(Blocks[i]) ){
            
//             DataType data = *((DataType*) (Blocks[i] + NODE_SIZE) );
//             DataType modifiedData = doMath(data, CONSTANT);
//             *((DataType*) (Blocks[i] + NODE_SIZE) ) = modifiedData;

//         }
//         //non leaf node
//         else if ( isB(Blocks[i]) ){
        
//             //child 2 stored in the indirection pointer
//             CursorTy* child2 = (CursorTy*) (Blocks[i] + 3*NODE_SIZE);        
//             //child 1
//             CursorTy child1 = Blocks[i] + 4*NODE_SIZE;
            
//             mylock.lock();
//             newBlocks.push_back(child1);
//             newBlocks.push_back(*child2);
//             mylock.unlock();
//         }
//     }

//     if(!newBlocks.empty()){
//         bfs_parallel(newBlocks);
//     }

// }

unsigned int isA(CursorTy node){

    TagTyPacked start = *node;
    if(start == 'A'){return 1;}
    else{return 0;}
}

bool isB(CursorTy node){

    TagTyPacked start = *node;
    if(start == 'B'){return true;}
    else{return false;}
}



// void forLoopNoVec(CursorTy bufferIn, DataType size){
    
//     //#pragma omp parallel for
//     for(int i=0; i < size; i++){
//         if (isA(bufferIn + i*NODE_SIZE)){
//             //grab next node
//             DataType * nextNode = (DataType*)(bufferIn + (i+1)*NODE_SIZE);
//             DataType modifiedData = doMath(*nextNode, CONSTANT);
//             *nextNode = modifiedData;
//         }
//     }

// }

void forloopVectorized(CursorTy bufferIn,DataType size){
    
    //All these intructions in the beginning before the for loop contribure to the extra overhead for the vectorized implementation
    //const mask for blending instruction
    __m256i constMask = _mm256_set_epi32 (0, 1, 2, 3, 4, 5, 6, 7);

    //const index into the main buffer where the integer values reside  
    __m256i vindex = _mm256_set_epi32(1, 3, 5, 7, 9, 11, 13, 15);

    //const index into the main buffer where the labels reside
    __m256i vindex_labels = _mm256_set_epi32(0, 2, 4, 6, 8, 10, 12, 14);

    //label compare, to compare with A, we need to set value as 65 (Ascii A = 65)
    __m256i label_compare = _mm256_set_epi32(65, 65, 65, 65, 65, 65, 65, 65);

    //floating constant for exponentiation
    __m256 mulConst = _mm256_set_ps(CONSTANT, CONSTANT, CONSTANT, CONSTANT, CONSTANT, CONSTANT, CONSTANT, CONSTANT);

    for (int i=NODE_SIZE; i<(size * NODE_SIZE); i += 2*LANE_LEN*NODE_SIZE){
        
        //scale factor is node size, loading 32 bit integers in a 256 bit vector
        //Load the values of the leaves into a 256 bit vector array 
        __m256 leafValuesVec = _mm256_i32gather_ps(&bufferIn[(i - NODE_SIZE)], vindex, NODE_SIZE);

        
        //scale factor is node size, loading 32 bit integers into a 256 bit vector array
        //Load the values of the labels into a 256 bit vector array
        __m256i labelValuesVec = _mm256_i32gather_epi32(&bufferIn[(i - NODE_SIZE)], vindex_labels, NODE_SIZE);
        
        // //DEBUG: Print the leaf values extracted from the buffer
        // std::cout << std::endl;
        // std::cout << "*****************************************************" << std::endl;
        // std::cout << "The values extracted from the leaves are" << std::endl;
        // DataType *res = (DataType*) &leafValuesVec; 
        // for (int j=0; j < LANE_LEN; j++){
        //     std::cout << res[LANE_LEN - j - 1] << " ";
        // }
        // std::cout << std::endl;
        // //////////////////////////////////////////////////////////////////////////////////////////
    
        
        //Calculate the masked bits in a vector, using vector compare
        __m256i maskBits = _mm256_cmpeq_epi32 (labelValuesVec, label_compare);

        // //DEBUG: Print the mask vector
        // std::cout << "Print the masked bit vector" << std::endl;
        // unsigned int *res2 = (unsigned int*) &maskBits; 
        // for (int j=0; j < LANE_LEN; j++){
        //     std::cout << res2[LANE_LEN - j - 1] << " ";
        // }
        // std::cout << std::endl;
        ////////////////////////////////////////////////////////////////////////////////////////
        
        
        //Load the value of the leaves again, as temp for the exponentiation
        //This load intruction is extra compared to the DFS implementation, I need to load the values again so that I don't modify the original buffer,
        //This is needed because i need to do a blend. In AVX 512, this can be optimized since it has a masked multiplication...
        __m256 mulResults = _mm256_loadu_ps ((DataType*)&leafValuesVec);

        for(int k=0; k<=loopLength; k+=1){
            //multiply 10 times
            mulResults = _mm256_mul_ps (mulResults,mulConst);
            //mulResults = _mm256_mul_ps (mulResults,mulConst);
            //mulResults = _mm256_mul_ps (mulResults,mulConst);
            //mulResults = _mm256_mul_ps (mulResults,mulConst);
            //mulResults = _mm256_mul_ps (mulResults,mulConst);
            //mulResults = _mm256_mul_ps (mulResults,mulConst);
            //mulResults = _mm256_mul_ps (mulResults,mulConst);
            //mulResults = _mm256_mul_ps (mulResults,mulConst);
            //mulResults = _mm256_mul_ps (mulResults,mulConst);
            //mulResults = _mm256_mul_ps (mulResults,mulConst);
        }


        //blend the addition values with the non-leaf values
        //This blend instruction is extra compared to the DFS implementation. Contributes to the increased overhead
        __m256 blendedResults  = _mm256_blendv_ps (leafValuesVec, mulResults,maskBits);

        //DEBUG: Print the final blended results
        // std::cout << "Print the values for addition results" << std::endl;
        // DataType *res3 = (DataType*) &blendedResults; 
        // for (int j=0; j < LANE_LEN; j++){
        //     std::cout << res3[LANE_LEN - j - 1] << " ";
        // }
        // std::cout << std::endl;
        // std::cout << "*****************************************************" << std::endl;
        //////////////////////////////////////////////////////////////////////////////////////////

         //cast vector to DataType memory address 
         DataType *result = (DataType*) &blendedResults; 
         //write the results back to memory
         for(int j=0; j < LANE_LEN; j++){
             *((DataType*) (bufferIn + i + j*(2*NODE_SIZE)) ) = result[LANE_LEN - j - 1];
         }         
    }

}

// void bfs_vectorized(std::vector<CursorTy> Blocks){

//     std::vector<CursorTy> newBlocks;
//     //std::vector<DataType*> leafNodes;
//     //std::vector<CursorTy> nonLeafNodes;
//     //std::vector<bool> booleanFlags(Blocks.size());
//     u_int8_t * booleanFlags = new unsigned char [Blocks.size()];
    
//     //#pragma omp simd
//     for (int i=0; i<Blocks.size(); i++){
//         booleanFlags[i] = isA(Blocks[i]);
//     }

//     //for(int i=0; i<Blocks.size(); i++){
//     //    if(booleanFlags[i]){leafNodes.push_back( (DataType*)(Blocks[i] + NODE_SIZE) );}
//     //    else{nonLeafNodes.push_back(Blocks[i]);}
//     //}

//     int factorOfLane = Blocks.size() - (Blocks.size() % LANE_LEN);

//     //#pragma omp parallel for
//     for(int i=0; i<factorOfLane; i=i+LANE_LEN){

//         int arr[LANE_LEN];
//         arr[0] = *(Blocks[i]);
//         arr[1] = *(Blocks[i+1]);
//         arr[2] = *(Blocks[i+2]);
//         arr[3] = *(Blocks[i+3]);
         
//         // // load the values from the blocks as a 128 bit packed vector 
//         // __m128i leafValuesVec = _mm_load_si128((__m128i *) arr);
          
//         //   // load constant values of 100 a 128 bit vector
//         // __m128i constantVec   = _mm_set_epi32 (100, 100, 100, 100);
        
//         // //load the 4 bits of flags into the lower part of the __mmask8
//         // __mmask8 myMask = _load_mask8 ((__mmask8*) &booleanFlags[i]);

//         //std::cout << myMask << std::endl;


//         //__m128i add1  = _mm_mask_add_epi32 (leafValuesVec, myMask, leafValuesVec, constantVec);
//         // __m128i add2  = _mm_mask_add_epi32 (leafValuesVec, myMask, add1, constantVec);
//         // __m128i add3  = _mm_mask_add_epi32 (leafValuesVec, myMask, add2, constantVec);
//         // __m128i add4  = _mm_mask_add_epi32 (leafValuesVec, myMask, add3, constantVec);
//         // __m128i add5  = _mm_mask_add_epi32 (leafValuesVec, myMask, add4, constantVec);
//         // __m128i add6  = _mm_mask_add_epi32 (leafValuesVec, myMask, add5, constantVec);
//         // __m128i add7  = _mm_mask_add_epi32 (leafValuesVec, myMask, add6, constantVec);
//         // __m128i add8  = _mm_mask_add_epi32 (leafValuesVec, myMask, add7, constantVec);
//         // __m128i add9  = _mm_mask_add_epi32 (leafValuesVec, myMask, add8, constantVec);
//         // __m128i add10 = _mm_mask_add_epi32 (leafValuesVec, myMask, add9, constantVec);
        
//         //_mm_mask_store_epi32 ((__m128i *) arr, myMask, add10);

//         // int *res = (int*) &leafValuesVec;

//         // *(Blocks[i])     =  res[0];
//         // *(Blocks[i+1])   =  res[1];
//         // *(Blocks[i+2])   =  res[2];
//         // *(Blocks[i+3])   =  res[3]; 


//         //multiply 
//         //__m128i multiply = _mm_mullo_epi32 (leafValuesVec, constantVec);
//         //subtract
//         //__m128i subtract = _mm_sub_epi32 (multiply, constantVec);
//         //add
//         //__m128i addition = _mm_add_epi32 (subtract, constantVec);
//         //square
//         //__m128i square = _mm_mullo_epi32 (addition, addition);
//         //Shift packed 32-bit integers in a left by imm8 while shifting in zeros, and store the results in dst.
//         //__m128i leftShift = _mm_slli_epi32 (square, 3);
//         //Shift packed 32-bit integers in a right by imm8 while shifting in sign bits, and store the results in dst.
//         //__m128i rightShift = _mm_srai_epi32 (leftShift, 1);
//         //Cast vector of type __m128i to type __m128d. This intrinsic is only used for compilation and does not generate any instructions, thus it has zero latency.
//         //__m128d doubleCasted = _mm_castsi128_pd (rightShift);
//         //Compute the square root of packed double-precision (64-bit) floating-point elements in a, and store the results in dst.
//         //__m128d squareRoot = _mm_sqrt_pd (doubleCasted);

//         //cast back to int
//         //Cast vector of type __m128d to type __m128i. This intrinsic is only used for compilation and does not generate any instructions, thus it has zero latency.
//         //__m128i castToInt = _mm_castpd_si128 (squareRoot);
//         //compute bitwise AND of 128 bits in a and b
//         //__m128i bitwiseAND = _mm_and_si128 (castToInt, constantVec);
//         //compute bitwise NOT of 128 bits in a, then AND with b.
//         //__m128i bitwiseNOTand = _mm_andnot_si128 (bitwiseAND, constantVec);
//         //Compute the bitwise OR of 128 bits (representing integer data) in a and b, and store the result in dst. 
//         //__m128i bitwiseOR = _mm_or_si128 (bitwiseNOTand, constantVec);
//         //Compute the bitwise XOR of 128 bits (representing integer data) in a and b, and store the result in dst.
//         //__m128i bitwiseXOR = _mm_xor_si128 (bitwiseOR, constantVec);
//         //Compare packed signed 32-bit integers in a and b, and store packed maximum values in dst.
//         //__m128i findMax = _mm_max_epi32 (bitwiseXOR, constantVec);
//         //Compare packed signed 32-bit integers in a and b, and store packed minimum values in dst.
//         //__m128i findMin = _mm_min_epi32 (findMax, constantVec);
        

//         //int *res = (int*) &findMin;
           
//         //*(leafNodes[i])     =  res[0];
//         //*(leafNodes[i+1])   =  res[1];
//         //*(leafNodes[i+2])   =  res[2];
//         //*(leafNodes[i+3])   =  res[3]; 

//     }

//     //sequential computation of the remaning part of the array that won't fit in the lane.
//     //#pragma omp parallel for 
//     for(int i=factorOfLane; i< Blocks.size(); i++){

//         DataType data = *((DataType*) (Blocks[i] + NODE_SIZE) );
//         DataType modifiedData = doMath(data, CONSTANT);
//         *((DataType*) (Blocks[i] + NODE_SIZE) ) = modifiedData;      

//     }

//     for (int i=0; i<Blocks.size(); i++){

//         if(!booleanFlags[i]){
//             newBlocks.push_back( *( (CursorTy*) (Blocks[i] + 3*NODE_SIZE) ) );
//             newBlocks.push_back( ( (CursorTy) (Blocks[i] + 4*NODE_SIZE) ) );
//         }
//     }

//     if(!newBlocks.empty()){
//         bfs_vectorized(newBlocks);
//     }

// }

CursorTy PrintTree(CursorTy cur) {
    if (*cur == 'A') {
        printf("(A ");
        cur += NODE_SIZE;
        DataType val = *(DataType *) cur;
        cur += NODE_SIZE;
        printf("%.3f",val);
        printf(") ");
        //printf("\n");
        return cur;
    } else if (*cur  == 'B') {
        printf("(B ");
        cur += NODE_SIZE;     //int value
        DataType val = *(DataType *) cur;
        cur += NODE_SIZE;
        printf("%.3f ", val);
        cur = PrintTree(cur);
        cur = PrintTree(cur);
        printf(") ");
        //printf("\n");
        return cur;
    }
    
}

int main (int argc, char** argv){

    if (argc < 3){
        printf("USAGE: executable SIZE loopLength\n");
        exit(1);
    }
    
    //get the loop length from user input
    loopLength = std::atoi(argv[2]);
    
    double time = 0;
    struct timespec beginTime;
    struct timespec endTime;

    DataType tree_depth = std::atoi(argv[1]); //depth of the tree
   
    //allocate a huge data array for now
    CursorTy dataArray = (CursorTy) malloc(UINT_MAX);

    CursorTy dataArrayOut = (CursorTy) malloc(UINT_MAX);

    CursorTy dataArrayOutforNV = (CursorTy) malloc(UINT_MAX);

    CursorTy dataArrayOutDfs = (CursorTy) malloc(UINT_MAX);

    //tree structure
    CursorIntProd myTree;
    myTree = createBalancedTree(dataArray, tree_depth);
    DataType size;     
    size = myTree.field1 / NODE_SIZE;
    
    //to print the number of nodes in the buffer
    printf("The length of the data buffer is: %.2f\n", size);

    //print the original tree
    std::cout << "Print the original tree that was created" << std::endl; 
    //PrintTree(dataArray);
    std::cout << std::endl;

    memcpy(dataArrayOut, dataArray, myTree.field1);
    memcpy(dataArrayOutDfs, dataArray, myTree.field1);
    memcpy(dataArrayOutforNV, dataArray, myTree.field1);

    clock_gettime(CLOCK_MONOTONIC_RAW, &beginTime);
    forloopVectorized(dataArrayOut, size);
    clock_gettime(CLOCK_MONOTONIC_RAW, &endTime);
    time = difftimespecs(&beginTime, &endTime);

   

    double timedfs = 0;
    struct timespec beginTimedfs;
    struct timespec endTimedfs;

    clock_gettime(CLOCK_MONOTONIC_RAW, &beginTimedfs);
    dfs_recurse(dataArrayOutDfs);
    clock_gettime(CLOCK_MONOTONIC_RAW, &endTimedfs);
    timedfs = difftimespecs(&beginTimedfs, &endTimedfs); 

    double timefnv = 0;
    struct timespec beginTimefnv;
    struct timespec endTimedfnv;

    //clock_gettime(CLOCK_MONOTONIC_RAW, &beginTimefnv);
    //forLoopNoVec(dataArrayOutforNV,size);
    //clock_gettime(CLOCK_MONOTONIC_RAW, &endTimedfnv);
    //timefnv = difftimespecs(&beginTimefnv, &endTimedfnv); 


    // std::vector<CursorTy> startBlock;
    // startBlock.push_back(dataArray);

    // //double time1 = 0;
    // //struct timespec beginTime1;
    // //struct timespec endTime1;

    // //clock_gettime(CLOCK_MONOTONIC_RAW, &beginTime1);
    // //bfs_parallel(startBlock);
    // //clock_gettime(CLOCK_MONOTONIC_RAW, &endTime1);
    // //time1 = difftimespecs(&beginTime1, &endTime1);  

    // double time2 = 0;
    // struct timespec beginTime2;
    // struct timespec endTime2;

    // clock_gettime(CLOCK_MONOTONIC_RAW, &beginTime2);
    // //bfs_vectorized(startBlock);
    // clock_gettime(CLOCK_MONOTONIC_RAW, &endTime2);
    // time2 = difftimespecs(&beginTime2, &endTime2); 

    // //double time3 = 0;
    // //struct timespec beginTime3;
    // //struct timespec endTime3;

    // //clock_gettime(CLOCK_MONOTONIC_RAW, &beginTime3);
    // //_cilk(dataArray);
    // //clock_gettime(CLOCK_MONOTONIC_RAW, &endTime3);
    // //time3 = difftimespecs(&beginTime3, &endTime3); 

    std::cout << std::endl;
    printf("Printing the tree after the vectorized for loop implementation\n");
    //std::cout << dataArrayOut[0] << std::endl;
    PrintTree(dataArrayOut);
    std::cout << std::endl;

    std::cout << std::endl;
    printf("Printing the tree after the dfs implementation\n");
    //std::cout << dataArrayOutDfs[0] << std::endl;
    PrintTree(dataArrayOutDfs);
    std::cout << std::endl;

    std::cout << std::endl;
    printf("Printing the tree after the Non vectorized for loop implementation\n");
    //std::cout << dataArrayOut[0] << std::endl;
    PrintTree(dataArrayOutforNV);
    std::cout << std::endl;


    std::ofstream forVecFile;
    std::ofstream dfsFile;

    forVecFile.open("forVecFile.txt");
    dfsFile.open("dfsFile.txt");

    forVecFile << time;
    dfsFile << timedfs;

    printf("\n");
    printf("========================================================================================\n");
    printf("Time taken for for loop vectorized implementation was (%lf) nanoseconds\n", time);
    printf("========================================================================================\n"); 
    printf("\n");

    printf("\n");
    printf("========================================================================================\n");
    printf("Time taken for dfs implementation was (%lf) nanoseconds\n", timedfs);
    printf("========================================================================================\n"); 
    printf("\n");

    printf("\n");
    printf("========================================================================================\n");
    printf("Time taken for non vectorized for loop implementation was (%lf) nanoseconds\n", timefnv);
    printf("========================================================================================\n"); 
    printf("\n");

    // // printf("\n");
    // // printf("========================================================================================\n");
    // // printf("Time taken for breath first parallel implementation was (%lf) nanoseconds\n", time1);
    // // printf("========================================================================================\n"); 
    // // printf("\n");

    // // printf("\n");
    // // printf("========================================================================================\n");
    // // printf("Time taken for Cilk implementation was (%lf) nanoseconds\n", time3);
    // // printf("========================================================================================\n"); 
    // // printf("\n");

    // printf("\n");
    // printf("========================================================================================\n");
    // printf("Time taken for breath first vectorized implementation was (%lf) nanoseconds\n", time2);
    // printf("========================================================================================\n"); 
    // printf("\n");

}