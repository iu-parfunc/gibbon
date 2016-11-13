#include "tree_unpacked.h"
#include <assert.h>
#define  TRIALS 10

int counter=0;
int counter2=0;
int counter3=0;
int nodeCount=0;
int nodeCount_test=0;
int compare_doubles (const void *a, const void *b);

int main(int argc, char **argv)
{
    
    //reading inputs and building point list
    Point * data;
    float rad;
    int npoints;
    Varient mode;
    
    readInput(argc,  argv, data, rad, npoints,mode);
    cout<<"benchmarking mode: "<<(mode==Varient::intOut? "intout":(mode== Varient::treeOut ? ("treeout"): ( mode== Varient::updateTree? ("updatetree"):("error!") )))<<endl;
    cout<<"points data of size " <<npoints<<" are loaded "<<endl;
    
    
    //build the treed
    clock_t begin = clock();
    Tree_Node * root=buildTree(npoints ,  data );
    clock_t end = clock();
    double time_spent = (double)(end - begin) / CLOCKS_PER_SEC;
    printf("done building, took %lf seconds\n\n", time_spent);
    
    
#ifdef TEST
    for(int j=0; j<min(10,npoints); j++){
        
        int res;
        res=performPointCorr_IntOut( data[j], root, rad);
        counter3+=res;
        performPointCorr_OnTree(data[j], root, rad);
        Tree_Node * out= performPointCorr_TreeOut(data[j], root, rad);
        deleteTree(out);
        
    }
    cout<<counter<<","<<counter2<<","<<counter3<<endl;
    assert((counter==counter2&& counter2==counter3) && "error");
    cout<<nodeCount_test/min(10,npoints)<<","<<nodeCount<<endl;
    assert(nodeCount_test/min(10,npoints)==nodeCount &&"error2");
#else
    
    
    //running the traversal
    double trials[TRIALS];
    for(int i=0; i<TRIALS; i++) {
        
        begin = clock();
        time_spent=0;

        for(int j=0; j<min(10,npoints); j++){
            int res;
            if(mode==Varient::intOut){
                begin = clock();
                res=performPointCorr_IntOut( data[j], root, rad);
                end = clock();
                time_spent+=(double)(end - begin);
            }
            else if(mode==Varient::updateTree){
                begin = clock();
                performPointCorr_OnTree(data[j], root, rad);
                end = clock();
                time_spent+=(double)(end - begin);
                
            
            }
            else if(mode==Varient::treeOut){
                begin = clock();
                Tree_Node * outTree=performPointCorr_TreeOut(data[j], root, rad);
                end = clock();
                deleteTree(outTree);
                time_spent+=(double)(end - begin);
            
            }
            else
                assert(false && "wrong arg 3");
        }
        
        time_spent = (double)time_spent / CLOCKS_PER_SEC;
        printf("  run(%d): %lf\n", i, time_spent);
        trials[i] = time_spent;
        
    }
    
    
    qsort(trials, TRIALS, sizeof(double), compare_doubles);
    double avg;
    double sum=0;
    for(int i=0; i<TRIALS; i++){
        sum+=trials[i];
    }
    avg=sum/TRIALS;
    //write results
    string benchamrk_name=(mode==Varient::intOut)?
    "handwritten-c-pointer-intout":((mode==Varient::treeOut)? "handwritten-c-pointer-treeout":"handwritten-c-pointer-updatetree");
    
    
    long long memoryUsage=sizeof(Tree_Node)*nodeCount;
    cout<<"number of nodes in tree:"<<nodeCount<<endl;
    printf("res:[%s,%s,%d,%d,%llu,%f]\n", "kd-tree", &benchamrk_name[0], npoints, TRIALS,  memoryUsage ,avg);
#endif
    
    
    //free memory
    delete [] data;
    deleteTree(root);
    //deallocation of tree ignored and left to OS :D :D .
    
    return 0;
}

int compare_doubles (const void *a, const void *b){
    const double *da = (const double *) a;
    const double *db = (const double *) b;
    return (*da > *db) - (*da < *db);
}