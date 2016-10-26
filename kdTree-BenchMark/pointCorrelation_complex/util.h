#ifndef UTIL_H_
#define UTIL_H_
#include <cstdlib>
#include <cmath>
#include <utility>
#include <vector>
#include <map>
#include <set>
#include <algorithm>
#include <iostream>
#include <fstream>
#include <sstream>
#include <iomanip>
#include "defines.h"
#include "tree.h"
#ifdef PERF
#include <papi.h>
void handleError(int retval);
#endif
using namespace std;
float max(float a, float b);
float min(float a, float b);
void readInput(int argc, char **argv);
void readPoint(FILE *in, Point *p);
#endif
