from itertools import permutations


# Layout Parameters
NUM_STRUCTS_RANGE = range(1, 8)
SIZE_RANGE = range(2, 20) #bytes
SIZE_STEP = 1
SIZE_FUNCT = "exp2" # "linear" "quadratic" "exp2"
META_ACCESS_PATTERNS = ["forward", "backward", "random"]


test_cases = []

for num in NUM_STRUCTS_RANGE:
    for access_pattern in permutations(range(num)):
        test_cases_of_n = []
        for size in range(len(SIZE_RANGE)):
            for meta_access_pattern in range(len(META_ACCESS_PATTERNS))
                test_cases_of_n.append(TestCase(access_pattern))

        for i in range(num):
            for size in range(len(SIZE_RANGE)):
                for meta_access_pattern in range(len(META_ACCESS_PATTERNS))
                    test_cases_of_n[
                        meta_access_pattern + size*len(META_ACCESS_PATTERNS)
                    ].append(
                        SIZE_RANGE[size], 
                        META_ACCESS_PATTERNS[meta_access_pattern]
                )
        test_cases = test_cases + test_cases_of_n



class TestCase:
    def __init__(self, access_pattern):
        self.access_pattern = access_pattern
        self.structs = []

    def append(self, size, meta_access_pattern):
        self.structs.append({
            "size": size,
            "access_pattern": meta_access_pattern
        })

    def write(self):
        with open("Layout.hs") as f:
            f.write("module Layout where\n")
            for s in range(len(structs)):
                fwrite(f"data Field_{s} = ")
            f.write("data Layout = ")