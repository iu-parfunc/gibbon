

all:

LOG=subst_all_expanded_1.2.log

scan_for_errors:
	grep -v "Benchmark:" $(LOG)  | grep -v  "==========" | grep -v "Done ingesting" | grep -v "cpu time" | grep -v "^$$" | tee wc 
