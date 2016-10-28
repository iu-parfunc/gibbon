#ifndef TREE_H_
#define TREE_H_
#include <cstdlib>
#include <cstdint>
#include <cfloat>
#include <cmath>
#include <iostream>
#include <fstream>
#include "defines.h"
using namespace std;
//Constants
const int MAX_POINTS_IN_CELL = 1;
//Point Data Structure
class Point{
		public:
				Point();
				~Point();
				//float coord[DIM];
				float *coord;
				int corr;
#ifdef DEBUG
				int pid;
				static int pcount;
#endif
};
//Tree Data Structure
class Node{
		public:
				Node();
				~Node();
				//Point in the node
				Point *data;
				Point *points[MAX_POINTS_IN_CELL];
				//splitting dimentions
				int axis;
				//splitting value
				float splitval;
				//#successors below
				int succnum;
				//Root flag
				bool isRoot;
				//Links to subtrees and parents
				Node *l;
				Node *r;
				//bounding boxes
    //?
				float *min;
				float *max;
		
    //truncation flag
	
                bool trunc;
				bool subtrunc;
#ifdef DEBUG
				int nid;
				static int ncount;
#endif
};

//Traversal Terminating Conditions
bool isLeaf(Node *n);
bool isEnd(Node *n);
bool isInner(Node *n);
//Tree Building
Node * buildTrees(Point *points, int lb, int ub, int depth);
void destroyTrees(Node *n);
//Point Utility Functions
float distanceAxis(Point *a, Point *b, int axis);
float distanceEuclid(Point *a, Point *b); 
int comparePoint(const void *a, const void *b); 
bool canCorrelate(Point *p, Node *n);
void drawTree(Node* n, ofstream& file);
#endif
/*TREE_H_*/
