#!/bin/bash

day=$1
files=($2)
compiles=($3)

cd ..
cd python
sed 's/X/'"$day"'/g' read_input.py > temp.py

python3 temp.py

rm -rf temp.py

cd ..
cd fortran

for f in "${files[@]}"
do
    gfortran -c $f
done

gfortran -o $day "${compiles[@]}"

./$day