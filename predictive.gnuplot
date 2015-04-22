# file = 'tmp/stats.log'
#set title "n_first"
#set output "out.png"

set autoscale                        # scale axes automatically
unset log                              # remove any log-scaling
unset label                            # remove any previous labels
set xtic .1                          # set xtics automatically
set ytic auto                          # set ytics automatically
set tics nomirror
#set term pngcairo size 1000,1000 enhanced font 'Verdana,10'

#set terminal svg enhanced fname 'Verdana, Helvetica, Arial, sans-serif' fsize 10
#set terminal epslatex size 10cm,10cm color colortext
set terminal postscript eps size 30cm,9cm enhanced color font 'Helvetica,20' linewidth 2
set xlabel font 'Helvetica,28'
set ylabel font 'Helvetica,28'
# set xtics font 'Helvetica,25'
# set ytics font 'Helvetica,25'

set size 1,1
unset title

set style line 11 lc rgb '#808080' lt 1
set border 3 back ls 11

set style line 12 lc rgb '#808080' lt 0 lw 1
set grid back ls 12


set multiplot layout 1, 3;
set xlabel 'Probability of Jumping'
set xr [0:1]

set ylabel 'Percentage'
set yr [0:100]
plot \
     file using 1:14 title 'Testing Budget'  pt 8 lw 2 with linespoints, \
     file using 1:3  title 'Prediction Rate' pt 4 lw 2 with linespoints, \
     file using 1:13 title 'Skipped Points'  pt 6 lw 2 with linespoints

set ylabel 'Error (meters)'
set autoscale y
#set key center right
set key out horizontal center top
#set yr [0:4000]

set style line 1 pt 6 ps 2 lw 2 lc rgb '#E1701F'
set style line 2 pt 1 ps 1 lw 2 lc rgb 'black'
set style line 3 pt 4 ps 2 lw 2 lc rgb '#E1701F'
set style line 4 pt 3 ps 1 lw 2 lc rgb 'black'
plot \
     file using 1:4  ls 1 title 'Predictive avg'  with linespoints, \
     file using 1:5  ls 2 title 'Independent avg' with linespoints, \
     file using 1:9  ls 3 title 'Predictive 90%'  with linespoints, \
     file using 1:10 ls 4 title 'Independent 90%' with linespoints

set ylabel 'Budget Consumption Rate (%)'
set yr [0:10]
set ytic 1                          # set ytics automatically
plot \
     file using 1:11 ls 1 title 'Predictive'  with linespoints, \
     file using 1:12 ls 2 title 'Independent' with linespoints

unset multiplot

#pause -1