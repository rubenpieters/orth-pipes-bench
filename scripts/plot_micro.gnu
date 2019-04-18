set terminal eps enhanced font 'Verdana,10'
set output 'benchmark_micro.eps'

set key left top
set style data histogram
set style histogram cluster gap 1 errorbars
set style fill solid border rgb "black"
set auto x
set yrange [0:*]


plot for [i=2:14:3] 'results-micro/results.tmp' using i:(column(i+1)):(column(i+2)):xtic(1) title col(i)