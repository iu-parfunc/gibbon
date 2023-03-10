import os 
import subprocess
import re
import statistics as stat

iterations = 12

rootdir = "/local/scratch/a/singhav/Applications/src/gibbon/gibbon-compiler/examples/layout_benchmarks/blog_management/marmoset"

ut_hash_include = "/local/scratch/a/singhav/Applications/src/uthash-2.3.0/include"

#Passes = ["ContentSearch", "DeleteTag", "InsertTag", "TagSearch"]

#layouts = ["layout1", "layout2", "layout3", "layout4", "layout5", "layout6", "layout7", "layout8"]

#Passes = [ "TagSearch", "ContentSearch", "FindBlog"]
Passes = ["ContentSearch"]

layouts = ["layout4", "layout2", "layout3", "layout1" , "layout5"  , "layout7" , "layout8"]
#layouts = ["layout5"]

#Compilation phase
for myPass in Passes:
    for layout in layouts: 
        
        gibbon_file_name = layout + myPass

        print()
        print("Trying compilation for file " + gibbon_file_name + ".hs")
        print()

        filename_haskell = gibbon_file_name + ".hs" 

        haskell_cmd = "gibbon --packed --no-gc --toC " + filename_haskell

        print("The haskell command was: ")
        print()
        print(haskell_cmd)
        print()

        gibbon_cmd_haskell = subprocess.run(["gibbon", "--packed", "--no-gc", "--toC", filename_haskell]) 

        print("The exit code for the haskell command was %d" % gibbon_cmd_haskell.returncode)
        print() 

        filename_c = gibbon_file_name + ".c"

        gibbon_cmd = "gcc" + " -O3 " + "-I" + ut_hash_include + " " + filename_c + " -o " + gibbon_file_name
        
        print("The gcc command was: ")
        print()
        print(gibbon_cmd)
        print()

        gibbon_cmd_c = subprocess.run(["gcc", "-O3", "-I" + ut_hash_include , filename_c,  "-o", gibbon_file_name])
        print()

        print("The exit code for the gcc compilation was %d" % gibbon_cmd_c.returncode)



Timings = {}

#run the files and get the timings

for myPass in Passes:
    for layout in layouts: 

        gibbon_binary = layout + myPass
        
        print()
        print("Running the binary " + str(gibbon_binary))
        print()

        file_stats = gibbon_binary + ".txt"
       
        #try:
        #    cmd =  "(" + "cd " + rootdir + " && " + "(" + "./" + gibbon_binary + " --RUN " + str(iterations) + " > " + file_stats + ")" + ")"

        #    print(cmd)

        #   gibbon_binary_cmd = subprocess.call(cmd, shell=True)

        #    data = open(file_stats, 'r').read()  

        #    batch_time  = re.findall("BATCHTIME: (.*)", data) 
        #    median_time = re.findall("SELFTIMED: (.*)", data)
        
        #    print()
        #    print(batch_time)
        #    print(median_time)
       
        #    print(float(batch_time[0]))
        #    print(float(median_time[0]))
        
        #    batchTimes = float(batch_time[0])
        #    medianTimes = float(median_time[0])

        #    averageTimes = float (batchTimes / iterations)
        
        #    tupleTimes = (averageTimes, medianTimes)

        #    Timings[gibbon_binary] = tupleTimes

        #    print(tupleTimes)

        #except: 
        #    print("Failed to run " + str(gibbon_binary))
        #    print("Trying this another way....")

        try:
                run_times_except = []
                for k in range(iterations):
                    cmd =  "(" + "cd " + rootdir + " && " + "(" + "./" + gibbon_binary  + " > " + file_stats + ")" + ")"

                    print(cmd)

                    gibbon_binary_cmd = subprocess.call(cmd, shell=True)

                    data = open(file_stats, 'r').read()

                    #batch_time  = re.findall("BATCHTIME: (.*)", data)
                    self_time = re.findall("SELFTIMED: (.*)", data)

                    print()
                    #print(batch_time)
                    print(self_time)
                    run_times_except.append(float(self_time[0]))
                    
                average_except = stat.mean(run_times_except) 
                median_except  = stat.median(run_times_except)
                tupleTimes = (average_except, median_except)
                Timings[gibbon_binary] = tupleTimes 
        except:
                
                tupleTimes = (-1, -1)
                
                Timings[gibbon_binary] = tupleTimes
                
        print()

print(Timings)

f = open("eval_OOPSLA_run7_contentSearch.txt", "w")

for key, value in Timings.items():
    f.write('%s:(average:%s, median:%s)\n' % (key, value[0], value[1]))

f.close()
