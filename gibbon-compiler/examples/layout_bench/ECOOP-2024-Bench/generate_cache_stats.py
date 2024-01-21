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
papi_dir = "/home/vidush/workdisk/git/gibbon-main/gibbon-compiler/examples/layout_bench/ECOOP-2024-Bench/papi_hl_output/"



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


# Compile all Marmoset binaries.
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
            
            
papi_tot_ins = 0
papi_tot_cyc = 0
papi_l2_dcm = 0            

for file in executables:
    
    cmd2 = ["rm", "-rf", papi_dir]
    c1 = subprocess.Popen(cmd2, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    c1.wait()
    
    
    cmd =  [file , "--inf-buffer-size", str(inf_buffer_size), "--iterate", "9"]
    
    try:
        c = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, universal_newlines=True)
        c.wait()
        output, error = c.communicate()
    except:
        print("Error!")
        
    if not os.path.exists(papi_dir):
        cmd =  [file, "--iterate", "9"]
        c = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, universal_newlines=True)
        c.wait()


    file_name = os.listdir(papi_dir)[0]
    file_path = os.path.join(papi_dir, file_name)
    fl = open(file_path)
    
    data = json.load(fl)
    
    stats = data['threads']['0']['regions']
    
    if len(stats) == iterations:
        for i in range(0, iterations):
            
            iter_stats = stats[str(i)]
            papi_tot_ins += int(iter_stats['PAPI_TOT_INS'])
            papi_tot_cyc += int(iter_stats['PAPI_TOT_CYC'])
            papi_l2_dcm += int(iter_stats['PAPI_L2_DCM'])

    tot_ins_avg = papi_tot_ins / iterations
    papi_tot_cyc = papi_tot_cyc / iterations
    papi_l2_dcm_avg = papi_l2_dcm / iterations

    print(f + " : " + "ins : {}, cyc : {}, l2 dcm : {}".format(tot_ins_avg, papi_tot_cyc, papi_l2_dcm_avg))

    papi_tot_ins = 0
    papi_tot_cyc = 0
    papi_l2_dcm = 0
    
    fl.close()
    
    
    
    
