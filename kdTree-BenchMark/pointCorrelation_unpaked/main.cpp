#include "tree_unpacked.h"


//#define TEST
#define  TRIALS 10
int counter=0;
int compare_doubles (const void *a, const void *b);

int main(int argc, char **argv)
{
    Point * data;
    float rad;
    int npoints;
    
    readInput(argc,  argv, data, rad, npoints);
    cout<<"points data of size " <<npoints<<" is loaded "<<endl;
    
    clock_t begin = clock();
    Tree_Node * root=buildTree(npoints ,  data );
    clock_t end = clock();
    double time_spent = (double)(end - begin) / CLOCKS_PER_SEC;
    printf("done building, took %lf seconds\n\n", time_spent);
    
    double trials[TRIALS];
    for(int i=0; i<TRIALS; i++) {
        begin = clock();
        for(int i=0; i<npoints; i++)
            performPointCorr_OnTree(data[i], root, rad);
        end = clock();
        time_spent = (double)(end - begin) / CLOCKS_PER_SEC;
        printf("  run(%d): %lf\n", i, time_spent);
        trials[i] = time_spent;
        
#ifdef TEST
        cout << "counter :" << counter << endl;
#endif
        
    }
    qsort(trials, TRIALS, sizeof(double), compare_doubles);
    printf("Sorted: ");
    for(int i=0; i<TRIALS; i++)
        printf(" %lf", trials[i]);
    printf("\nSELFTIMED: %lf\n", trials[TRIALS / 2]);
    
    delete [] data;
    free(root);
    
    return 0;
}

int compare_doubles (const void *a, const void *b){
    const double *da = (const double *) a;
    const double *db = (const double *) b;
    return (*da > *db) - (*da < *db);
}