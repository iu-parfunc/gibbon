import subprocess
import time
import statistics
import numpy as np
import scipy
import re
import os
import sys

iterations = 9
inf_buffer_size = 10000000000

def mean_confidence_interval(data, confidence=0.95):
    a = 1.0 * np.array(data)
    n = len(a)
    m, se = np.mean(a), scipy.stats.sem(a)
    h = se * scipy.stats.t.ppf((1 + confidence) / 2., n-1)
    return m, m-h, m+h

rootdir = "/home/vidush/workdisk/git/gibbon-main/gibbon-compiler/examples/layout_bench/ECOOP-2024-Bench/"


# Was thinking to make compile and run separate but not important right now.
#compileTrue = sys.argv[2]
#executeTrue = sys.argv[3]

executables = []

gibbonFiles = ['eval_r.hs', 'layout3FilterBlogs.hs', 'TreeExpoPre.hs', 'layout1TagSearch.hs', 'layout2ListLen.hs', 'layout8TagSearch.hs', 'layout2TagSearch.hs', 'layout8FilterBlogs.hs', 'TreeExpoIn.hs', 'layout4ContentSearch.hs', 'layout3ContentSearch.hs', 'eval_l.hs', 'TreeAddOnePre.hs', 'layout7TagSearch.hs', 'layout2ContentSearch.hs', 'TreeRightMost_l.hs', 'layout2FilterBlogs.hs', 'manyFuncs.hs', 'layout1PowerList.hs', 'TreeCopyPre.hs', 'TreeAddOneIn.hs', 'layout4TagSearch.hs', 'layout1ListLen.hs', 'TreeCopyPost.hs', 'layout8ContentSearch.hs', 'TreeRightMost_r.hs', 'TreeAddOnePost.hs', 'layout1ContentSearch.hs', 'TreeCopyIn.hs', 'layout7ContentSearch.hs', 'layout3TagSearch.hs', 'layout2PowerList.hs', 'layout5ContentSearch.hs', 'layout4FilterBlogs.hs', 'layout5FilterBlogs.hs', 'layout5TagSearch.hs', 'layout7FilterBlogs.hs', 'layout1FilterBlogs.hs', 'TreeExpoPost.hs']

# Compile all Gibbon binaries.
for subdir, dirs, files in os.walk(rootdir):
    
    print("subdir: " + str(subdir))
    print("dirs: " + str(dirs))
    print("files: " + str(files))
    
    
    for file in files: 
        
        if ".hs" in file and file in gibbonFiles:
        
            file_path = subdir + file
            
            file_without_haskell_extension = file_path.replace(".hs", '')
            print("Compile " + file + "...")
            gibbon_cmd = ["gibbon", "--no-gc", "--to-exe", "--packed", file_path]
        
            c = subprocess.Popen(gibbon_cmd)
            c.wait()
            output, error = c.communicate()
        
            if error is None: 
                print("Compiled " + file + " succesfully!")
                
            executables.append(file_without_haskell_extension + ".exe")
            
            print()
        
        
marmosetFiles = ['eval_r.hs', 'TreeExpoPre.hs', 'layout2ListLen.hs', 'layout8TagSearch.hs', 'layout8FilterBlogs.hs', 'TreeAddOnePre.hs', 'layout8ContentSearch.hs', 'TreeRightMost_l.hs', 'manyFuncs.hs', 'TreeCopyPre.hs', 'layout1PowerList.hs']


# Compile all Gibbon binaries.
for subdir, dirs, files in os.walk(rootdir):
    
    print("subdir: " + str(subdir))
    print("dirs: " + str(dirs))
    print("files: " + str(files))
    
    
    for file in files: 
        
        if ".hs" in file and file in marmosetFiles:
        
            file_path = subdir + file
            
            file_without_haskell_extension = file_path.replace(".hs", '')
            print(file_without_haskell_extension)
            
            solver_binary_name = file_without_haskell_extension + "Solver"
            greedy_binary_name = file_without_haskell_extension + "Greedy"
            
            executables.append(solver_binary_name)
            executables.append(greedy_binary_name)
        
            print("Compile " + file + " with solver optimization..." )
            solver_cmd = ["gibbon", "--no-gc", "--to-exe", "--packed", "--opt-layout-global", "--opt-layout-use-solver", file_path, "-o", solver_binary_name]
        
            c = subprocess.Popen(solver_cmd)
            c.wait()
            output, error = c.communicate()
        
            if error is None: 
                print("Compiled " + file + " succesfully!")
                
            print()
            
            print("Compile " + file + " with greedy optimization..." )
            greedy_cmd = ["gibbon", "--no-gc", "--to-exe", "--packed", "--opt-layout-global", file_path, "-o", greedy_binary_name]
        
            c = subprocess.Popen(greedy_cmd)
            c.wait()
            output, error = c.communicate()
        
            if error is None: 
                print("Compiled " + file + " succesfully!")
                
                
            print()


for file in executables:
    
    #print(file)
    runtimeFile = file + ".txt"
    
    cmd =  [file , "--inf-buffer-size", str(inf_buffer_size), "--iterate", "9"]
    
    writeFileHandle = open(runtimeFile, "w")
    
    try:
        c = subprocess.Popen(cmd, stdout=writeFileHandle, stderr=subprocess.PIPE, universal_newlines=True)
        c.wait()
        output, error = c.communicate()
        if error is not None:
            cmd =  [file , "--iterate", "9"]
            c = subprocess.Popen(cmd, stdout=writeFileHandle, stderr=subprocess.PIPE, universal_newlines=True)
            c.wait()
    except:
        print("Error could not run file!")

    writeFileHandle.close()
    readFileHandle = open(runtimeFile, "r")
    fileLines = readFileHandle.readlines()
    
    iterTimes = []
    mean = 0.0
    median = 0.0
    for lines in fileLines:
        search = re.match("itertime: (-?\ *[0-9]+\.?[0-9]*(?:[Ee]\ *-?\ *[0-9]+)?)", lines)
        #print(search)
        if search is not None:
            iterTimes.append(float(search.groups()[0]))
    #print(iterTimes)
    mean = np.mean(iterTimes)
    median = np.median(iterTimes)
    a, l, u = mean_confidence_interval(iterTimes)
    print(str(file) + "(mean:{0}, median:{1}, lower:{2}, upper:{3})".format(str(mean), str(median), str(l), str(u)))
    readFileHandle.close()
