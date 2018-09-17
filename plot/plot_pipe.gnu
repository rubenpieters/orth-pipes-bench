plot 'plot/deep-pipe_conduit.tmp' using 1:2:3:4 with errorlines title "conduit" pt 7 lc rgb '#3e9651', 'plot/deep-pipe_pipes.tmp' using 1:2:3:4 with errorlines title "pipes" pt 7 lc rgb '#cc2529', 'plot/deep-pipe_proxyrep.tmp' using 1:2:3:4 with errorlines title "proxyrep" pt 7 lc rgb '#396ab1', 'plot/deep-pipe_contpipe.tmp' using 1:2:3:4 with errorlines title "contpipe" pt 7 lc rgb '#6b439a'

set autoscale y

MAX=GPVAL_X_MAX

set xrange [1:MAX+MAX*0.05]

set logscale x

set terminal eps enhanced font 'Verdana,10'
set output 'benchmark_pipe.eps'

set border linewidth 1.5
set pointintervalbox 3
set pointsize 0.1

set key left top
set xlabel "amount of deep-pipe [log] (n)"
set ylabel "seconds (s)"
set grid ytics

plot 'plot/deep-pipe_conduit.tmp' using 1:2:3:4 with errorlines title "conduit" pt 7 lc rgb '#3e9651', 'plot/deep-pipe_pipes.tmp' using 1:2:3:4 with errorlines title "pipes" pt 7 lc rgb '#cc2529', 'plot/deep-pipe_proxyrep.tmp' using 1:2:3:4 with errorlines title "proxyrep" pt 7 lc rgb '#396ab1', 'plot/deep-pipe_contpipe.tmp' using 1:2:3:4 with errorlines title "contpipe" pt 7 lc rgb '#6b439a'
