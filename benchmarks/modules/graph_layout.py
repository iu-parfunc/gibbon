import matplotlib.pyplot as plt
import numpy as np
import re

ids = []
depths = []
times = []

with open("run.log", "r") as f:
    line = f.readline()
    i = -1
    while (line):
        if (re.search(r"---", line)):
            i = i + 1
            ids.append(0)
            depths.append(0)
            times.append(0)
        else:
            match = re.search(r"ID: \[(?P<num>-*\d+)\]", line)
            if(match):
                ids[i] = int(match.groups("num")[0])
            else:
                match = re.search(r"TIMES: \[(?P<num>[\d\.]+)\]", line)
                if(match):
                    times[i] = float(match.groups("num")[0])
                else:
                    match = re.search(r"DEPTH: \[(?P<num>\d+)\]", line)
                    if(match):
                        depths[i] = int(match.groups("num")[0])
        
        line = f.readline()

#print("ids: ")
#print(ids)
#print("\ndepths:")
#print(depths)
#print("\ntimes:")
#print(times)

fig = plt.figure()
ax = fig.add_subplot(projection="3d")
ax.scatter(ids[::727], depths[::727], times[::727])
#for i in range(0, len(ids), 11):
    

ax.set_xlabel("index")
ax.set_ylabel("depth")
ax.set_zlabel("time to access")

plt.show()