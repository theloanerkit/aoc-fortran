DAY = "X"

f = open(f"/home/kit/Documents/aoc-2023-fortran/inputs/{DAY}.txt","r")
data = f.readlines()
f.close()

file_length = len(data)

f = open(f"/home/kit/Documents/aoc-2023-fortran/fortran/{DAY}f.txt","w")
f.write(str(file_length))
f.write("\n")
for line in data:
    f.write(line)
f.close()