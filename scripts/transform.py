import csv
import sys

# cmd argument 1: folder to process
folder = sys.argv[1]

data = {}

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
        data.setdefault(bench_name, {}).setdefault(bench_type, {}).setdefault(bench_n, (bench_mean,bench_mean_lb,bench_mean_ub))

        line_nr += 1

for bench_name in data.keys():
    # print("benchmark - " + bench_name)
    for bench_type in data[bench_name].keys():
        # print ("type - " + bench_type)
        filename = bench_name + "_" + bench_type + ".tmp"
        # print ("filling: " + filename)
        file = open(folder + "/" + filename, "w")

        for bench_n in data[bench_name][bench_type].keys():
            (bench_mean, bench_mean_lb, bench_mean_ub) = data[bench_name][bench_type][bench_n]
            line = bench_n + " " + bench_mean + " " + bench_mean_lb + " " + bench_mean_ub
            # print ("line: " + line)
            file.write(line + "\n")

        file.close()
