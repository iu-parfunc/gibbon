#include "traversal.h"
#include "common.h"

void recurse(Point *t1, Node *t2)
{
		if (t2 == NULL || !canCorrelate(t1, t2)) return;
		
		nNodes++;
		nWorkNodes++;
		float dist = distanceEuclid(t1, t2->data);
		if(sqrt(dist) < rad) t1->corr++; 
		
		recurse(t1, t2->l);
		recurse(t1, t2->r);

}
