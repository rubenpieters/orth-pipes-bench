plot 'results-yahoo/adevents_conduit.tmp' using 1:2:3:4 with errorlines title "conduit" pt 6 dt 1 lw 3 lc rgb '#3e9651', 'results-yahoo/adevents_pipes.tmp' using 1:2:3:4 with errorlines title "pipes" pt 4 dt 2 lw 3 lc rgb '#cc2529', 'results-yahoo/adevents_streamly.tmp' using 1:2:3:4 with errorlines title "streamly" pt 9 dt 5 lw 3 lc rgb '#948b3d', 'results-yahoo/adevents_proxyrep.tmp' using 1:2:3:4 with errorlines title "proxyrep" pt 2 dt 3 lw 3 lc rgb '#396ab1'


set autoscale x
set autoscale y

MAX=GPVAL_X_MAX
MIN=GPVAL_X_MIN

unset autoscale x
set xrange [MIN:MAX+(MAX-MIN)*0.05]

set terminal eps enhanced font 'Verdana,10'
set output 'benchmark_yahoo.eps'

set border linewidth 1.5
set pointintervalbox 3
set pointsize 0.5

set key left top
set xlabel "number of stream elements (n)"
set ylabel "time (seconds)"
set grid ytics

plot 'results-yahoo/adevents_conduit.tmp' using 1:2:3:4 with errorlines title "conduit" pt 6 dt 1 lw 3 lc rgb '#3e9651', 'results-yahoo/adevents_pipes.tmp' using 1:2:3:4 with errorlines title "pipes" pt 4 dt 2 lw 3 lc rgb '#cc2529', 'results-yahoo/adevents_streamly.tmp' using 1:2:3:4 with errorlines title "streamly" pt 12 dt 5 lw 3 lc rgb '#948b3d', 'results-yahoo/adevents_proxyrep.tmp' using 1:2:3:4 with errorlines title "proxyrep" pt 2 dt 3 lw 3 lc rgb '#396ab1'
