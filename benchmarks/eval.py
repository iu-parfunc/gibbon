import subprocess as sp
import time
import numpy as np

TEST = "./modules/GibbonPureSet.exe"
VERSIONS = [TEST, "./modules/GHCPureSet", "./modules/GHCStdSet"]

def runproc(filename: str):
    start = time.time()
    p = sp.call([filename], stdout=sp.DEVNULL, stderr=sp.STDOUT)
    #p.wait()
    return time.time() - start


results = {}
avgs = {}

for version in VERSIONS:
    results[version] = []

for i in range(100):
    for version in VERSIONS:
        results[version].append(runproc(version))

print("\n-- Results ------\n")
for version in VERSIONS:
    avgs[version] = np.mean(results[version])
    print(f"{version}: {avgs[version]}ms")
print("\n-----------------\n")

for version in VERSIONS:
    if(version != TEST):
        ratio = avgs[version]/avgs[TEST]
        msg = "[X]"
        if(ratio > 1): msg = "[🟢]"

        print(f"{msg} Gibbon offers a {ratio}x improvement over {version}")
