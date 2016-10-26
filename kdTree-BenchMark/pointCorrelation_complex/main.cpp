#include "main.h"
#include "common.h"
#ifdef PERF
#define NUM 8
#endif
//Globals
Point *inPoints;
int dim;
float rad;
int npoints;
long long nNodes;
long long nWorkNodes;

int main(int argc, char **argv) 
{
		nNodes = 0;
		nWorkNodes = 0;
		readInput(argc, argv);
#ifdef DEBUG	
		cout << "rad " << rad << endl;
		cout << "dim " << dim << endl;
#endif

#ifdef PERF
		string instance("Original");
		int ret;
		int events[] = {PAPI_TOT_CYC, PAPI_TOT_INS, PAPI_L2_DCA, PAPI_L2_DCM, PAPI_L2_ICA, PAPI_L2_ICM, PAPI_L3_TCA, PAPI_L3_TCM};
		string defs[] = {"Cycle Count", "Instruction Count", "L2 Data Access Count", "L2 Data Miss Count", "L2 Instruction Access Count", "L2 Instruction Miss Count", "L3 Total Access Count", "L3 Total Miss Count"};
		long long values[NUM];
		long long rcyc0, rcyc1, rusec0, rusec1;
		long long vcyc0, vcyc1, vusec0, vusec1;

		ret = PAPI_library_init(PAPI_VER_CURRENT); 
		if (ret != PAPI_VER_CURRENT){ 
				cerr << "PAPI Init Error" << endl;
				exit(1);
		}
		for(int i=0; i<NUM; ++i){
				ret = PAPI_query_event(events[i]);
				if(ret != PAPI_OK){
						cerr << "PAPI Event " << i << " does not exist" << endl;
					    handleError(ret);
				}
		}
#endif
	
		root1 = buildTrees(inPoints, 0, npoints-1, 0);
		root1->isRoot = true;
		
#ifdef PERF
		//Performance Counters Start
		rcyc0  = PAPI_get_real_cyc();
		rusec0 = PAPI_get_real_usec();
		
		vcyc0  = PAPI_get_virt_cyc();
		vusec0 = PAPI_get_virt_usec();

		ret = PAPI_start_counters(events, NUM);
		if (ret != PAPI_OK){
				cerr << "PAPI Error starting counters" << endl;
			   	handleError(ret);
		}
#endif
		for(int i=0; i<npoints; i++) recurse(&inPoints[i], root1);
#ifdef PERF
		//Performance Counters Read
		ret = PAPI_read_counters(values, NUM);
		if (ret != PAPI_OK){
				cerr << "PAPI Error reading counters" << endl;
			   	handleError(ret);
		}
		
		rcyc1  = PAPI_get_real_cyc();
		rusec1 = PAPI_get_real_usec();
		
		vcyc1  = PAPI_get_virt_cyc();
		vusec1 = PAPI_get_virt_usec();
#endif

#ifdef PERF
		//Performance Counters Printed
		cout << instance <<  " Performance Counters" << endl;
		for(int i=0; i<NUM; ++i){
				cout << instance << " " << defs[i] << ": " << values[i] << endl;
		}
		cout << endl;
		cout << instance << " Wall Clock Cycles: "  << rcyc1-rcyc0 << endl;
		cout << instance << " Wall Clock Time(s): " << (rusec1-rusec0)/1000000.0 << endl;
		cout << endl;
		cout << instance << " Virtual Clock Cycles: "  << vcyc1-vcyc0 << endl;
		cout << instance << " Virtual Clock Time(s): " << (vusec1-vusec0)/1000000.0 << endl;
#endif

#ifdef PERF
		cout << instance << " Work: " << nWorkNodes << endl;
#endif

		long long sum = 0;
		for (int i = 0; i < npoints; i++) {
				sum += inPoints[i].corr;
		}
		
#ifdef DEBUG	
		cout << "sum " << sum << endl;
		cout << "avg corr: " << (float)sum / npoints << endl;
		cout << "successors of root " << root1->succnum << endl;
		cout << "traversed nodes " << nNodes << endl;
		cout << "worked nodes " << nWorkNodes << endl;
#endif

#if !(defined(DEBUG) || defined(PERF))
		cout << nNodes << endl;
#endif
		
		delete [] inPoints;
		destroyTrees(root1);
		return 0;
}
