#include "tree_packed.h"
#include <assert.h>
#include <fstream>
#include <iomanip>
#define  TRIALS 10

int counter=0;
int counter2=0;
int counter3=0;
int nodeCount=0;
int nodeCount_INNER=0;
int compare_doubles (const void *a, const void *b);

int main(int argc, char **argv){
    
    
    //reading inputs and building point list
    Point * data;
    float rad;
    int npoints;
    Varient mode;
    readInput(argc,  argv, data, rad, npoints,mode);
    
    cout<<"points data of size " <<npoints<<" are loaded "<<endl;
    
    clock_t begin = clock();
    char *root_withIndirection=NULL;
    char * root_noIndirection=NULL;
    /*
     #ifdef TEST
     
     root_withIndirection=buildTree(npoints ,  data, true);
     root_noIndirection=buildTree(npoints ,  data, false);
     #else
     
     if(mode==Varient::intOut || mode==Varient::updateTree)
     root_withIndirection=buildTree(npoints ,  data, true);
     
     else if(mode==Varient::treeOut)
     root_noIndirection=buildTree(npoints ,  data, false);
     #endif
     */
    //print the packed tree to file
    
    ofstream myfile;
    //myfile.open ("tree_"+to_string(npoints)+".txt");
    
    myfile.open ("inputs/in_"+to_string(npoints)+".txt");
    myfile<<"#"<<npoints<<"(" ;
    
    for(int i=0; i<npoints ; i++){
        myfile<<std::fixed<<std::setw(11)<<std::setprecision(6)<<std::setfill('0') <<" ("<<data[i].x_val<<" . "<<data[i].y_val<<") ";
    }
    
    myfile<<")";
    
    
    
}

int compare_doubles (const void *a, const void *b){
    const double *da = (const double *) a;
    const double *db = (const double *) b;
    return (*da > *db) - (*da < *db);
}
