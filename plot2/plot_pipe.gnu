plot 'plot/deep-pipe_orth-pipes-recursive-merge.tmp' using 1:2:3:4 with errorlines title "rec-orth-pipes" pt 7 lc rgb '#3e9651', 'plot/deep-pipe_orth-pipes-no-unsafe-coerce.tmp' using 1:2:3:4 with errorlines title "safe-orth-pipes" pt 7 lc rgb '#da7c30', 'plot/deep-pipe_pipes.tmp' using 1:2:3:4 with errorlines title "pipes" pt 7 lc rgb '#cc2529', 'plot/deep-pipe_orth-pipes.tmp' using 1:2:3:4 with errorlines title "orth-pipes" pt 7 lc rgb '#396ab1', 'plot/deep-pipe_contpipe.tmp' using 1:2:3:4 with errorlines title "contpipe" pt 7 lc rgb '#6b439a'


set autoscale x
set autoscale y

MAX=GPVAL_X_MAX
MIN=GPVAL_X_MIN

unset autoscale x
set xrange [MIN:MAX+(MAX-MIN)*0.05]

set terminal eps enhanced font 'Verdana,10'
set output 'benchmark_pipe.eps'

set border linewidth 1.5
set pointintervalbox 3
set pointsize 0.1

set key left top
set xlabel "amount of deep-pipe (n)"
set ylabel "seconds (s)"
set grid ytics

plot 'plot/deep-pipe_orth-pipes-recursive-merge.tmp' using 1:2:3:4 with errorlines title "rec-orth-pipes" pt 7 lc rgb '#3e9651', 'plot/deep-pipe_orth-pipes-no-unsafe-coerce.tmp' using 1:2:3:4 with errorlines title "safe-orth-pipes" pt 7 lc rgb '#da7c30', 'plot/deep-pipe_pipes.tmp' using 1:2:3:4 with errorlines title "pipes" pt 7 lc rgb '#cc2529', 'plot/deep-pipe_orth-pipes.tmp' using 1:2:3:4 with errorlines title "orth-pipes" pt 7 lc rgb '#396ab1', 'plot/deep-pipe_contpipe.tmp' using 1:2:3:4 with errorlines title "contpipe" pt 7 lc rgb '#6b439a'
