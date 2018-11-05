plot 'plot/deep-seq_conduit.tmp' using 1:2:3:4 with errorlines title "conduit" pt 6 dt 1 lc rgb '#3e9651', 'plot/deep-seq_pipes.tmp' using 1:2:3:4 with errorlines title "pipes" pt 4 dt 2 lc rgb '#cc2529', 'plot/deep-seq_proxyrep.tmp' using 1:2:3:4 with errorlines title "proxyrep" pt 2 dt 3 lc rgb '#396ab1', 'plot/deep-seq_contpipe.tmp' using 1:2:3:4 with errorlines title "contpipe" pt 8 dt 4 lc rgb '#6b439a'

set autoscale y

MAX=GPVAL_X_MAX

set xrange [1:MAX+MAX*0.05]

set logscale x

set terminal eps enhanced font 'Verdana,10'
set output 'benchmark_seq.eps'

set border linewidth 1.5
set pointintervalbox 3
set pointsize 0.5

set key left top
set xlabel "number of deep-seq calls [log] (n)"
set ylabel "time (seconds)"
set grid ytics

plot 'plot/deep-seq_conduit.tmp' using 1:2:3:4 with errorlines title "conduit" pt 6 dt 1 lc rgb '#3e9651', 'plot/deep-seq_pipes.tmp' using 1:2:3:4 with errorlines title "pipes" pt 4 dt 2 lc rgb '#cc2529', 'plot/deep-seq_proxyrep.tmp' using 1:2:3:4 with errorlines title "proxyrep" pt 2 dt 3 lc rgb '#396ab1', 'plot/deep-seq_contpipe.tmp' using 1:2:3:4 with errorlines title "contpipe" pt 8 dt 4 lc rgb '#6b439a'
