#!/bin/bash

# Allow failures so we can keep going:
set +e

failures=0
for f in `find ../../cleaned_racket -name "*.sexp" `; do
    echo "Processing file " $f
    ./check.exe $f 1 
    if [ $? -eq 0 ];then
      ((passed+=1))
      echo "SUCCESS!!!!"
    else
      ((failed+=1))
    fi
done

echo ""
echo "PASSED : "$passed
echo "FAILED : "$failed
