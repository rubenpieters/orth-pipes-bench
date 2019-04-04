import csv
import sys

# cmd argument 1: folder to process
folder = sys.argv[1]

data = {}
names = []
preamble = "Lib"

with open(folder + "/bench.csv", "rt", encoding="utf8") as benchfile:
    reader = csv.reader(benchfile)

    line_nr = 0
    for line in reader:
        if line_nr == 0:
            line_nr += 1
            continue
        [bench_type, bench_name, bench_n] = line[0].split("/", 2)
        bench_mean = line[1]
        bench_mean_lb = line[2]
        bench_mean_ub = line[3]
        data.setdefault(bench_type, {}).setdefault(bench_name, (bench_mean,bench_mean_lb,bench_mean_ub))

        if bench_name not in names:
            names.append(bench_name)
            preamble += " " + bench_name + " min max"
        line_nr += 1

# print("names - " + str(names))
# print(preamble)

file = open(folder + "/results.tmp", "w")
file.write(preamble + "\n")

for bench_type in data.keys():
    # print("type - " + bench_type)

    line = bench_type
    for bench_name in data[bench_type].keys():
        (bench_mean, bench_mean_lb, bench_mean_ub) = data[bench_type][bench_name]
        line += " " + bench_mean + " " + bench_mean_lb + " " + bench_mean_ub
    
    # print("line - " + line)
    file.write(line + "\n")

file.close()
