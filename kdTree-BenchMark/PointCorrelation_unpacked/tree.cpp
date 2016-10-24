#include "tree.h"
#include "util.h"
#include "common.h"
Node *root1, *root2;
int sort_split;

#ifdef DEBUG
int Point::pcount = 0;	
int Node::ncount = 0;
#endif
//functions
Point::Point() 
{
		coord = (float*)malloc(sizeof(float)*dim);
		corr = 0;
#ifdef DEBUG
		pid = pcount++;
#endif
}

Point::~Point(){
		free(coord);
}

Node::Node() 
{
		data = NULL;
		min = (float*)malloc(sizeof(float)*dim);
		max = (float*)malloc(sizeof(float)*dim);
		for (int i = 0; i < dim; i++) {
				min[i] = FLT_MAX;
				max[i] = 0;
		}
		for (int i = 0; i < MAX_POINTS_IN_CELL; i++) {
				points[i] = NULL;
		}
		l = NULL;
		r = NULL;

		succnum = 0;

		trunc = false;

		subtrunc = false;
#ifdef DEBUG
		nid = ncount++;
#endif
}

Node::~Node(){
		free(min);
		free(max);
}

bool isLeaf(Node *n)
{
		return (n->l == NULL && n->r == NULL);
}

bool isEnd(Node *n)
{
		return n == NULL;
}

bool isInner(Node *n)
{
		return (isLeaf(n->l) || isLeaf(n->r));
}

Node * buildTrees(Point *points, int lb, int ub, int depth)
{
		Node *node = new Node();
		
		int size = ub - lb + 1;
		int mid  = (ub + lb) / 2;

		if (size <= MAX_POINTS_IN_CELL) {
				
				if (size == 0) return NULL;
				qsort(&points[lb], ub - lb + 1, sizeof(Point), comparePoint);
				
				node->data     = &points[mid];
				node->axis     = dim; 
				node->splitval = 0.0;
				
				node->l = NULL;
				node->r = NULL;

				node->succnum  = 0;
				
				for (int i = 0; i < size; i++) {
						node->points[i] = &points[lb + i];
						for (int j = 0; j < dim; j++) {
								node->min[j] = min(node->min[j], points[lb + i].coord[j]);
								node->max[j] = max(node->max[j], points[lb + i].coord[j]);
						}
				}
				return node;
		} 
		else {
			
				sort_split = depth % dim;
				qsort(&points[lb], ub - lb + 1, sizeof(Point), comparePoint);

				node->data     = &points[mid];
				node->axis     = sort_split;
				node->splitval = points[mid].coord[node->axis];
				
				node->l        = buildTrees(points, lb, mid-1, depth + 1);
				node->r        = buildTrees(points, mid+1, ub, depth + 1);

				if      (node->l != NULL && node->r != NULL) node->succnum = node->l->succnum + node->r->succnum + 2;
				else if (node->l != NULL && node->r == NULL) node->succnum = node->l->succnum + 1;
				else if (node->l == NULL && node->r != NULL) node->succnum = node->r->succnum + 1;
				else node->succnum = 0;

				if (node->l != NULL && node->r != NULL){
						for(int j = 0; j < dim; j++) {
								node->min[j] = min(min(node->l->min[j], node->r->min[j]), points[mid].coord[j]);
								node->max[j] = max(max(node->l->max[j], node->r->max[j]), points[mid].coord[j]);
						}
				}
				else if (node->l != NULL && node->r == NULL){
						for(int j = 0; j < dim; j++) {
								node->min[j] = min(node->l->min[j], points[mid].coord[j]);
								node->max[j] = max(node->l->max[j], points[mid].coord[j]);
						}
				}
				else if (node->l == NULL && node->r != NULL){
						for(int j = 0; j < dim; j++) {
								node->min[j] = min(node->r->min[j], points[mid].coord[j]);
								node->max[j] = max(node->r->max[j], points[mid].coord[j]);
						}
				}
				else {
						for(int j = 0; j < dim; j++) {
								node->min[j] = points[mid].coord[j];
								node->max[j] = points[mid].coord[j];
						}
				}
				return node;
		}	
}

void destroyTrees(Node *n)
{
		if (n->l != NULL) destroyTrees(n->l);
		if (n->r != NULL) destroyTrees(n->r);
		delete n;
}

float distanceAxis(Point *a, Point *b, int axis) 
{
		return (a->coord[axis] - b->coord[axis]) * (a->coord[axis] - b->coord[axis]);
}

float distanceEuclid(Point *a, Point *b) 
{
		float d = 0;
		for(int i = 0; i < dim; i++) {
				d += distanceAxis(a,b,i);
		}
		return d;
}

int comparePoint(const void *a, const void *b) 
{
		if(((Point *)a)->coord[sort_split] < ((Point *)b)->coord[sort_split]) 
				return -1;
		else if(((Point *)a)->coord[sort_split] > ((Point *)b)->coord[sort_split]) 
				return 1;
		else 
				return 0;
}

bool canCorrelate(Point *p, Node *n)
{
		float sum    = 0.0;
		float boxsum = 0.0;
		for(int i = 0; i < dim; i++){
				float center  = (n->max[i] + n->min[i]) / 2;
				float boxdist = (n->max[i] - n->min[i]) / 2;
				float dist    = p->coord[i] - center;
				sum    += dist * dist;
				boxsum += boxdist * boxdist;
		}
		if(sqrt(sum) - sqrt(boxsum) < rad)
				return true;
		else
				return false;
}
#ifdef DEBUG
void drawTree(Node* n, ofstream& file){
		if(n == NULL) return;
		if(n->l != NULL) file << setw(ceil(log10(Node::ncount))) << setfill('0') << n->nid << " -> " << setw(ceil(log10(Node::ncount))) << setfill('0') << n->l->nid << ";" << endl;
		drawTree(n->l, file);
		if(n->r != NULL) file << setw(ceil(log10(Node::ncount))) << setfill('0') << n->nid << " -> " << setw(ceil(log10(Node::ncount))) << setfill('0') << n->r->nid << ";" << endl;
		drawTree(n->r, file);
}
#endif
