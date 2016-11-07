#include "tree_unpacked.h"

#define  TRIALS 1
#define TEST
int counter=0;
int compare_doubles (const void *a, const void *b);

int main(int argc, char **argv)
{
    Point * data;
    float rad;
    int npoints;
    
    readInput(argc,  argv, data, rad, npoints);
    cout<<"points data of size " <<npoints<<" is loaded "<<endl;
   /*
    cout<<"(define pointList (vector";
    for(int i=0; i<npoints; i++){
    cout<<" ( cons "<< data[i].x_val<<" "<<data[i].y_val<<")";
        
    }
    cout<<")"<<endl;
    */
    
    clock_t begin = clock();
    Tree_Node * root=buildTree(npoints ,  data );
    clock_t end = clock();
    double time_spent = (double)(end - begin) / CLOCKS_PER_SEC;
    printf("done building, took %lf seconds\n\n", time_spent);
    
    
    double trials[TRIALS];
    for(int i=0; i<TRIALS; i++) {
        begin = clock();
        for(int j=0; j<min(10,npoints); j++){

            int res=performPointCorr_IntOut( data[j], root, rad);
#ifdef TEST
            cout<<"res :"<<res<<endl;
#endif
        }
        end = clock();
        time_spent = (double)(end - begin) / CLOCKS_PER_SEC;
     //   printf("  run(%d): %lf\n", i, time_spent);
        trials[i] = time_spent;
        
    }
    qsort(trials, TRIALS, sizeof(double), compare_doubles);
    double avg;
    double sum;
    
    for(int i=0; i<TRIALS; i++){
        sum+=trials[i];
    }
    avg=sum/TRIALS;
    
    printf("res: %s, %s, %d, %d, %lf\n", "kd-tree","handwritten-c-pointer",npoints,TRIALS,avg);
    
    delete [] data;
    free(root);
    
    return 0;
}

int compare_doubles (const void *a, const void *b){
    const double *da = (const double *) a;
    const double *db = (const double *) b;
    return (*da > *db) - (*da < *db);
}