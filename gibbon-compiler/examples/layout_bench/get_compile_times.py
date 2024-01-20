import subprocess
import time
import statistics
import numpy as np
import scipy
import re

iterations = 9

def mean_confidence_interval(data, confidence=0.95):
    a = 1.0 * np.array(data)
    n = len(a)
    m, se = np.mean(a), scipy.stats.sem(a)
    h = se * scipy.stats.t.ppf((1 + confidence) / 2., n-1)
    return m, m-h, m+h

rootdir = "/home/vidush/workdisk/git/gibbon-main/gibbon-compiler/examples/layout_bench/ECOOP-2024-Bench/"

Passes = ["ContentSearch", "TagSearch", "FilterBlogs"]

layouts = ["layout1", "layout2", "layout3", "layout4", "layout5", "layout6", "layout7", "layout8"]

#Compilation phase #gibbon
for myPass in Passes:
    for layout in layouts:

        gibbon_file_name = layout + myPass

        #print()
        #print("Trying compilation for file " + gibbon_file_name + ".hs")
        #print()

        filename_haskell = gibbon_file_name + ".hs"

        haskell_cmd = "gibbon --packed --no-gc --toC " + filename_haskell

        #print("The haskell command was: ")
        #print()
        #print(haskell_cmd)
        #print()
         
        iterTimes = [] 

        for i in range(0, iterations):
            start = time.time()
            gibbon_cmd_haskell = subprocess.run(["gibbon", "--packed", "--no-gc", "--to-exe", filename_haskell])
            elapsed = time.time()

            iterTimes.append(float(elapsed-start))

        #print("The exit code for the haskell command was %d" % gibbon_cmd_haskell.returncode)
        #print("Time taken by " + gibbon_file_name + "was: " + str(float(elapsed-start)))
        
        #print(iterTimes)
        #filename_c = gibbon_file_name + ".c"

        #gibbon_cmd = "gcc" + " -O3 " + "-I" + ut_hash_include + " " + filename_c + " -o " + gibbon_file_name

        #print("The gcc command was: ")
        #print()
        #print(gibbon_cmd)
        #print()

        #gibbon_cmd_c = subprocess.run(["gcc", "-O3", "-I" + ut_hash_include , filename_c,  "-o", gibbon_file_name])
        #print()

        #print("The exit code for the gcc compilation was %d" % gibbon_cmd_c.returncode)

        m, lb, ub = mean_confidence_interval(iterTimes)
        median = statistics.median(iterTimes) 
        print(gibbon_file_name + " : " + "Median: {}, Mean: {}, lb: {}, ub: {}".format(median, m, lb, ub))

def parse_solver_times(array):
    
    solver_times = []
    for line in array:

        result = re.findall("iter time: ((\d+).(\d+))", line)
        print(float(result[0][0]))
        solver_times.append(float(result[0][0]))

    return sum(solver_times)

#Compilation phase #solver
for myPass in Passes:
    for layout in layouts:

        gibbon_file_name = layout + myPass

        #print()
        #print("Trying compilation for file " + gibbon_file_name + ".hs")
        #print()

        filename_haskell = gibbon_file_name + ".hs"

        #haskell_cmd = "gibbon --packed --no-gc --toC " + filename_haskell

        #print("The haskell command was: ")
        #print()
        #print(haskell_cmd)
        #print()

        iterTimes = []
        solver_times = []
        
        for i in range(0, iterations):
            file_handle = open("stats.txt", "w")
            start = time.time()
            gibbon_cmd_haskell = subprocess.run(["gibbon", "--packed", "--no-gc", "--opt-layout-use-solver", "--opt-layout-global", "--to-exe", filename_haskell], stdout=file_handle, stderr=file_handle)
            elapsed = time.time()
            file_handle.close()
            iterTimes.append(float(elapsed-start)) 
            read_file_handle = open("stats.txt", "r")
            lines = read_file_handle.readlines()
            read_file_handle.close()
            solver_time = parse_solver_times(lines)
            print()
            print(solver_time)
            print()
            solver_times.append(solver_time)
            print()

        print(iterTimes)
        print(solver_times)

        file_handle.close()
        read_file_handle.close()

        #print("The exit code for the haskell command was %d" % gibbon_cmd_haskell.returncode)
        #print("Time taken by " + gibbon_file_name + "was: " + str(float(elapsed-start)))

        #print(iterTimes)
        #filename_c = gibbon_file_name + ".c"

        #gibbon_cmd = "gcc" + " -O3 " + "-I" + ut_hash_include + " " + filename_c + " -o " + gibbon_file_name

        #print("The gcc command was: ")
        #print()
        #print(gibbon_cmd)
        #print()

        #gibbon_cmd_c = subprocess.run(["gcc", "-O3", "-I" + ut_hash_include , filename_c,  "-o", gibbon_file_name])
        #print()

        #print("The exit code for the gcc compilation was %d" % gibbon_cmd_c.returncode)

        m, lb, ub = mean_confidence_interval(iterTimes)
        median = statistics.median(iterTimes)

        mm, lbb, ubb = mean_confidence_interval(solver_times)
        mediann = statistics.median(solver_times)
        print(gibbon_file_name + " (total_solver_time) : " + "Median: {}, Mean: {}, lb: {}, ub: {}".format(median, m, lb, ub))
        print(gibbon_file_name + " (only_solver_time) : " + "Median: {}, Mean: {}, lb: {}, ub: {}".format(mediann, mm, lbb, ubb))


#def parse_solver_times(array):
#
#    for line in array:
#
#        result = re.findall("iter:\d+", line)
#        print(result)


#Compilation phase greedy
for myPass in Passes:
    for layout in layouts:

        gibbon_file_name = layout + myPass

        #print()
        #print("Trying compilation for file " + gibbon_file_name + ".hs")
        #print()

        filename_haskell = gibbon_file_name + ".hs"

        haskell_cmd = "gibbon --packed --no-gc --toC " + filename_haskell

        #print("The haskell command was: ")
        #print()
        #print(haskell_cmd)
        #print()

        iterTimes = []

        for i in range(0, iterations):
            start = time.time()
            gibbon_cmd_haskell = subprocess.run(["gibbon", "--packed", "--no-gc", "--opt-layout-global", "--to-exe", filename_haskell])
            elapsed = time.time()

            iterTimes.append(float(elapsed-start))

        #print("The exit code for the haskell command was %d" % gibbon_cmd_haskell.returncode)
        #print("Time taken by " + gibbon_file_name + "was: " + str(float(elapsed-start)))

        #print(iterTimes)
        #filename_c = gibbon_file_name + ".c"

        #gibbon_cmd = "gcc" + " -O3 " + "-I" + ut_hash_include + " " + filename_c + " -o " + gibbon_file_name

        #print("The gcc command was: ")
        #print()
        #print(gibbon_cmd)
        #print()

        #gibbon_cmd_c = subprocess.run(["gcc", "-O3", "-I" + ut_hash_include , filename_c,  "-o", gibbon_file_name])
        #print()

        #print("The exit code for the gcc compilation was %d" % gibbon_cmd_c.returncode)

        m, lb, ub = mean_confidence_interval(iterTimes)
        median = statistics.median(iterTimes)
        print(gibbon_file_name + " (Greedy Times) : " + "Median: {}, Mean: {}, lb: {}, ub: {}".format(median, m, lb, ub))



'''Timings = {}

#run the files and get the timings

for myPass in Passes:
    for layout in layouts:

        gibbon_binary = layout + myPass

        print()
        print("Running the binary " + str(gibbon_binary))
        print()

        file_stats = gibbon_binary + ".txt"

        cmd =  "(" + "cd " + rootdir + " && " + "(" + "./" + gibbon_binary + " --RUN " + str(iterations) + " > " + file_stats + ")" + ")"

        print(cmd)

        gibbon_binary_cmd = subprocess.call(cmd, shell=True)

        data = open(file_stats, 'r').read()

        batch_time  = re.findall("BATCHTIME: (.*)", data)
        median_time = re.findall("SELFTIMED: (.*)", data)

        print()
        print(batch_time)
        print(median_time)

        print(float(batch_time[0]))
        print(float(median_time[0]))

        batchTimes = float(batch_time[0])
        medianTimes = float(median_time[0])

        averageTimes = float (batchTimes / iterations)

        tupleTimes = (averageTimes, medianTimes)

        print(tupleTimes)

        Timings[gibbon_binary] = tupleTimes

        print()

print(Timings)

f = open("experiment_timings_marmoset.txt", "w")

for key, value in Timings.items():
    f.write('%s:(average:%s, median:%s)\n' % (key, value[0], value[1]))

f.close()
'''                                                                    
