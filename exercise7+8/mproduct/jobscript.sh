#!/bin/bash

rm mproduct.txt
rm results.txt
rm matrices.dat
rm mproduct.err
rm matrices.err
rm mproduct.out
rm matrices.out

echo "Method    CPU time [s]" >> mproduct.txt
echo "Method    First element   Trace" >> results.txt
for s in $(seq 1 1 3) 
do
./mproduct > mproduct.out 2> mproduct.err <<EOF
1000
$s
EOF

echo -e "$s \t$(awk '/Elapsed/ {print $4}' mproduct.out)" >> mproduct.txt
echo -e "$s \t$(awk '/The/ {print $9}' mproduct.out) \t$(awk '/The/ {print $15}' mproduct.out)" >> results.txt
rm mproduct.out
done

echo "Method    CPU time [s]   Memory [MB]" >> matrices.dat
for n in $(seq 1000 1000 10000)
do
./mproduct > matrices.out 2> matrices.err <<EOF
$n
4
EOF
echo -e "$n \t$(awk '/Elapsed/ {print $4}' matrices.out) \t$(awk '/Elapsed/ {print $8}' matrices.out)" >> matrices.dat
rm matrices.out
done