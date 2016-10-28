#include "util.h"
#include "common.h"

float max(float a, float b)
{
		return a > b ? a : b;
}

float min(float a, float b)
{
		return a < b ? a : b;
}

void readInput(int argc, char **argv)
{
		FILE *in;

		if(argc != 5 && argc != 4) {
				fprintf(stderr, "usage: pointcorr <DIM> <rad> <npoints> [input_file]\n");
				exit(1);
		}
		
		dim = atoi(argv[1]);
		if(dim <= 0) {
				fprintf(stderr, "Invalid DIM\n");
				exit(1);
		}

		rad = atof(argv[2]);

		npoints = atol(argv[3]);
		if(npoints <= 0) {
				fprintf(stderr, "Not enough points.\n");
				exit(1);
		}

		inPoints = new Point[npoints];

		if(argc == 5) {
				in = fopen(argv[4], "r");
				if(in == NULL) {
						fprintf(stderr, "Could not open %s\n", argv[4]);
						exit(1);
				}

				for(int i = 0; i < npoints; i++) {
						readPoint(in, &inPoints[i]);
				}
				fclose(in);
		} else {
				srand(0);
				for(int i = 0; i < npoints; i++) {
						for(int j = 0; j < dim; j++) {
								inPoints[i].coord[j] = (float)rand() / RAND_MAX;
						}
				}
		}
}

void readPoint(FILE *in, Point *p)
{
		int dummy;
		if(fscanf(in, "%d", &dummy) != 1) {
				fprintf(stderr, "Input file not large enough.\n");
				exit(1);
		}
		for(int j = 0; j < dim; j++) {
				if(fscanf(in, "%f", &p->coord[j]) != 1) {
						fprintf(stderr, "Input file not large enough.\n");
						exit(1);
				}
		}
}

#ifdef PERF
void handleError(int retval)
{
		cout << "PAPI Error " << retval << ": " << PAPI_strerror(retval) << endl;
		exit(1);
}
#endif
