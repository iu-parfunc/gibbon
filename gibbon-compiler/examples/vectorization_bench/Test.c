#pragma GCC optimize("O3","unroll-loops","omit-frame-pointer","inline" /*, "no-optimize-sibling-calls"*/) //Optimization flags
#pragma GCC option("arch=native","tune=native","no-zeroupper") //Enable AVX
#pragma GCC target("avx2")  //Enable AVX
#include <x86intrin.h> //SSE Extensions

#include <stdio.h>
#include <stdlib.h>
#include <time.h>

clock_t start, end;
double cpu_time_used;

int arraySize = 1000000;

__attribute__((optimize("no-tree-vectorize")))
void vectorized(int * array){

    __m256i ones = _mm256_set_epi32(1, 1, 1, 1, 1, 1, 1, 1);
    __m256i __attribute__((aligned(32))) loadEightElements;
    __m256i __attribute__((aligned(32))) addOneEightElements; 
    for (int i=0; i<arraySize; i=i+8){
        loadEightElements   = _mm256_loadu_si256((__m256i const*)(&array[i]));
        addOneEightElements = _mm256_add_epi32(loadEightElements, ones);
        _mm256_storeu_si256 ((__m256i*)(&array[i]), addOneEightElements);
    }

}

void recursive_vectorized(int *array, __m256i ones, int i){

    if (i >= arraySize){
        return;
    }

    __m256i __attribute__((aligned(32))) loadEightElements = _mm256_loadu_si256((__m256i const*)(&array[i]));
    __m256i __attribute__((aligned(32))) addOneEightElements = _mm256_add_epi32(loadEightElements, ones);
    _mm256_storeu_si256 ((__m256i*)(&array[i]), addOneEightElements);

    recursive_vectorized(array, ones, i+8);
}

__attribute__((optimize("no-optimize-sibling-calls")))
//the above attribute disables tail call optimization. 
void no_tail_recursive_vectorized(int *array, __m256i ones, int i){

    if (i >= arraySize){
        return;
    }

    __m256i __attribute__((aligned(32))) loadEightElements = _mm256_loadu_si256((__m256i const*)(&array[i]));
    __m256i __attribute__((aligned(32))) addOneEightElements = _mm256_add_epi32(loadEightElements, ones);
    _mm256_storeu_si256 ((__m256i*)(&array[i]), addOneEightElements);

    no_tail_recursive_vectorized(array, ones, i+8);
}

__attribute__((optimize("no-optimize-sibling-calls")))
void scalar_recursive(int *array, int i){

    if (i >= arraySize){
        return;
    }

    array[i] = array[i] + 1;
    scalar_recursive(array, i+1);
}


void initArray(int * array){

    for(int i=0; i<arraySize; i++){
        array[i] = i;
    }

}

void printArray(int * array){

    for(int i=0; i<arraySize; i++){
        printf(" %d ", array[i]);
    }

}


__attribute__((optimize("no-tree-vectorize")))
void scalar(int * array){
    for (int i=0; i<arraySize; i++){
        array[i] = array[i] + 1;
    }
}

int main (){

    int *array = (int*)malloc(sizeof(int)*arraySize);
    initArray(array);

    start = clock();
    vectorized(array); 
    end = clock();
    cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;
    printf("Manually Vectorized for loop add1 took %f seconds to execute \n", cpu_time_used);

    //refresh array
    initArray(array);
    start = clock();
    scalar(array); 
    end = clock();
    cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;
    printf("Scalar add 1 took %f seconds to execute \n", cpu_time_used);
    
    //refresh array 
    initArray(array);
    start = clock();
    __m256i ones = _mm256_set_epi32(1, 1, 1, 1, 1, 1, 1, 1);
    recursive_vectorized(array, ones, 0);
    end = clock();
    cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;
    printf("Recrusive tail optimized add 1 took %f seconds to execute \n", cpu_time_used);
    
    initArray(array);
    start = clock();
    __m256i _ones = _mm256_set_epi32(1, 1, 1, 1, 1, 1, 1, 1);
    no_tail_recursive_vectorized(array, _ones, 0);
    end = clock();
    cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;
    printf("Recrusive non-tail optimized add 1 took %f seconds to execute \n", cpu_time_used);
    
    //scalar array recursive
    initArray(array);
    start = clock();
    scalar_recursive(array, 0);
    end = clock();
    cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;
    printf("scalar recursive non tail optimized add 1 took %f seconds to execute \n", cpu_time_used);

}