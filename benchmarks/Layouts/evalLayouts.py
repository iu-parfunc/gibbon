import re
import argparse
import subprocess
import numpy as np
import matplotlib.pyplot as plt


# Get Arguements
#parser = argparse.ArgumentParser()
#parser.add_argument("size")
#args = parser.parse_args()

files = ["Left", "Right", "LeftAndRight", "RightAndLeft"]
sizes = range(5,10)

# Data Point Class
class Point:

    def __init__(self, dir, size, times):
        self.dir = files[dir]
        self.dir_idx = dir
        self.size = size
        self.avg = np.mean(times)
        self.var = np.var(times)



data = []

code = ""
for size in sizes:
    for i in range(len(files)):
        filename = files[i]
        with open(f"{filename}.hs", "r") as f:
            code = f.read()
            code = re.sub("<SIZE>", f"{pow(2, size)}", code)
            f.close()
            with open(f"_{filename}.hs", "w") as f:
                f.write(code)
                f.close()
        
        proc = subprocess.run(["gibbon", "--to-exe", f"_{filename}.hs"])
        with open(f"{filename}.out", "w") as f:
            proc = subprocess.call([f"./_{filename}.exe"], stdout=f)
            f.close()

        with open(f"{filename}.out", "r") as f:
            times = []
            for line in f:
                match = re.search(r"TIMES: \[(?P<num>[\d\.\d]+)\]", line)
                if match:
                    times.append(float(match.groups("num")[0]))
            data.append(
                Point(i, size, times)
            )

fig = plt.figure()
ax0 = fig.add_subplot(121, projection="3d")
ax0.set_title("Average Time")
ax0.bar3d([p.dir_idx for p in data], [p.size for p in data], 0, 1, 1, [p.avg for p in data], shade=True)
ax0.set_xticks(range(len(files)))
ax0.set_xticklabels(files)

ax1 = fig.add_subplot(122, projection="3d")
ax1.set_title("Variance")
ax1.bar3d([p.dir_idx for p in data], [p.size for p in data], 0, 1, 1, [p.var for p in data], shade=True)
ax1.set_xticks(range(len(files)))
ax1.set_xticklabels(files)

plt.show()