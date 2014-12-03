req = 'requirement.dat'

set autoscale                        # scale axes automatically
unset log                              # remove any log-scaling
unset label                            # remove any previous labels
#set xtic 1                          # set xtics automatically
#set ytic 50                          # set ytics automatically
set tics nomirror
#set term pngcairo size 1000,1000 enhanced font 'Verdana,10'

#set terminal svg enhanced fname 'Verdana, Helvetica, Arial, sans-serif' fsize 10
#set terminal epslatex size 10cm,10cm color colortext
set terminal postscript eps size 20cm,10cm enhanced color font 'Helvetica,20' linewidth 2
set xlabel font 'Helvetica,28'
set ylabel font 'Helvetica,28'
# set xtics font 'Helvetica,25'
# set ytics font 'Helvetica,25'

set key top left

set size 1,1

set style line 11 lc rgb '#808080' lt 1
set border 3 back ls 11

set style line 12 lc rgb '#808080' lt 0 lw 1
set grid back ls 12


set xlabel 'Radius'
set ylabel 'Privacy Points'

set xr [0:2]
set yr [0:20]

#req
set style line 1 pt 4 ps 2 lw 2 lc rgb 'black'
#mappa
set style line 2 pt 6 ps 2 lw 2 lc rgb '#E1701F'
#lapla-w
set style line 3 pt 1 ps 1 lw 2 lc rgb 'red'
#lapla-s
set style line 5 pt 1 ps 1 lw 2 lc rgb 'blue'


plot \
     req    using 1:2 ls 1 title 'requirement' with linespoints, \
     mappa  using 1:2 ls 2 title 'EM'          with linespoints, \
     laplaw using 1:2 ls 3 title 'PLh'         with linespoints, \
     laplas using 1:2 ls 5 title 'PLl'         with linespoints


#pause -1