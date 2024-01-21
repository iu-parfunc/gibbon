import os
import subprocess
import re
import numpy as np
import scipy.stats
import json


rootdir="/local/scratch/a/singhav/gibbon/gibbon-compiler/PAPI_BINARIES/"
papi_dir = "/local/scratch/a/singhav/gibbon/gibbon-compiler/PAPI_BINARIES/papi_hl_output/"

iterations = 9

for subdir, dirs, files in os.walk(rootdir):

    # subdirectories where the c++ compilation occurs have dirs == []
    #if dirs == []:

        # Opening JSON file

        papi_tot_ins = 0
        papi_tot_cyc = 0
        papi_l3 = 0
        papi_l2_dcm = 0
        papi_l3_accesses = 0
        for f in files:
            if "." not in f:

                #print(subdir)
                #print(f)

                cmd2 = ["rm", "-rf", papi_dir]
                c1 = subprocess.Popen(cmd2, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
                c1.wait()


                #if "Solver" in f and "Filter" not in f:
                #    cmd =  ["./" + f , "--inf-buffer-size", "10000000000", "--biginf-buffer-size", "1000000000", "--iterate", "9"]
                #else:
                #    cmd = ["./" + f , "--iterate", "9"]

                #c = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
                #c.wait()
                cmd =  ["./" + f , "--inf-buffer-size", "10000000000", "--iterate", "9"]

                #print(cmd)
                #writeFileHandle = open(runtimeFile, "w")

                try:
                    c = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, universal_newlines=True)
                    c.wait()
                    output, error = c.communicate()
                    #if error is not None:
                    #    cmd =  ["./" + f , "--iterate", "9"]
                    #    c = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, universal_newlines=True)
                    #    c.wait()
                except:
                    print("ERROR!")
                    print(e.message)
                    #cmd =  ["./" + f , "--iterate", "9"]
                    #c = subprocess.Popen(cmd, stdout=writeFileHandle, stderr=subprocess.PIPE, universal_newlines=True)
                    #c.wait()

                #if error is not None:
                #    cmd =  ["./" + f , "--iterate", "9"]
                #    c = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, universal_newlines=True)
                #    c.wait()

                if not os.path.exists(papi_dir):
                    cmd =  ["./" + f , "--iterate", "9"]
                    c = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, universal_newlines=True)
                    c.wait()


                file_name = os.listdir(papi_dir)[0]
                file_path = os.path.join(papi_dir, file_name)
                fl = open(file_path)

                # returns JSON object as 
                # a dictionary
                data = json.load(fl)

                # Iterating through the json
                # list

                #for key in data: 
                #    print(key)
                #    print(data[key])
                #    print()

                #print(data['threads'])

                stats = data['threads']['0']['regions']
                #print(stats)
                if len(stats) == iterations:
                    for i in range(0, iterations):
                        #print(i)
                        #print(stats[str(i)])
                        #print()

                        iter_stats = stats[str(i)]
                        #print(iter_stats['PAPI_TOT_INS'])
                        #print(iter_stats['PAPI_TOT_CYC'])
                        #print(iter_stats['PAPI_L2_DCM'])
                        #print()

                        papi_tot_ins += int(iter_stats['PAPI_TOT_INS'])
                        papi_tot_cyc += int(iter_stats['PAPI_TOT_CYC'])
                        #papi_l3 += int(iter_stats['PAPI_L3_TCM'])
                        #papi_l3_accesses += int(iter_stats['PAPI_L3_DCA'])
                        papi_l2_dcm += int(iter_stats['PAPI_L2_DCM'])

                tot_ins_avg = papi_tot_ins / iterations
                papi_tot_cyc = papi_tot_cyc / iterations
                #papi_l3_avg = papi_l3 / iterations
                #papi_l3_accesses_avg = papi_l3_accesses / iterations
                papi_l2_dcm_avg = papi_l2_dcm / iterations

                #print(f + " : " + "ins : {}, cyc : {}, l3 misses : {}, l3 accesses : {}, miss rate : {}".format(tot_ins_avg, papi_tot_cyc, papi_l3_avg, papi_l3_accesses_avg, float(papi_l3_avg/papi_l3_accesses_avg)))
                print(f + " : " + "ins : {}, cyc : {}, l2 dcm : {}".format(tot_ins_avg, papi_tot_cyc, papi_l2_dcm_avg))

                #print(tot_ins_avg)
                #print(papi_tot_cyc)
                #print(papi_l2_avg)

                papi_tot_ins = 0
                papi_tot_cyc = 0
                papi_l3 = 0
                papi_l3_accesses = 0
                papi_l2_dcm = 0


                # Closing file
                fl.close()

