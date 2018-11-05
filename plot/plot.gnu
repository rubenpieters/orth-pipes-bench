plot 'plot/primes_conduit.tmp' using 1:2:3:4 with errorlines title "conduit" pt 6 dt 1 lc rgb '#3e9651', 'plot/primes_pipes.tmp' using 1:2:3:4 with errorlines title "pipes" pt 4 dt 2 lc rgb '#cc2529', 'plot/primes_proxyrep.tmp' using 1:2:3:4 with errorlines title "proxyrep" pt 2 dt 3 lc rgb '#396ab1', 'plot/primes_contpipe.tmp' using 1:2:3:4 with errorlines title "contpipe" pt 8 dt 4 lc rgb '#6b439a'


set autoscale x
set autoscale y

MAX=GPVAL_X_MAX
MIN=GPVAL_X_MIN

unset autoscale x
set xrange [MIN:MAX+(MAX-MIN)*0.05]

set terminal eps enhanced font 'Verdana,10'
set output 'benchmark.eps'

set border linewidth 1.5
set pointintervalbox 3
set pointsize 0.5

set key left top
set xlabel "number of primes (n)"
set ylabel "time (seconds)"
set grid ytics

plot 'plot/primes_conduit.tmp' using 1:2:3:4 with errorlines title "conduit" pt 6 dt 1 lc rgb '#3e9651', 'plot/primes_pipes.tmp' using 1:2:3:4 with errorlines title "pipes" pt 4 dt 2 lc rgb '#cc2529', 'plot/primes_proxyrep.tmp' using 1:2:3:4 with errorlines title "proxyrep" pt 2 dt 3 lc rgb '#396ab1', 'plot/primes_contpipe.tmp' using 1:2:3:4 with errorlines title "contpipe" pt 8 dt 4 lc rgb '#6b439a'
