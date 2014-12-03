set autoscale                        # scale axes automatically
unset log                              # remove any log-scaling
unset label                            # remove any previous labels
#set xtic .1                          # set xtics automatically
#set ytic auto                          # set ytics automatically
set tics nomirror
#set term pngcairo size 1000,1000 enhanced font 'Verdana,10'

#set terminal svg enhanced fname 'Verdana, Helvetica, Arial, sans-serif' fsize 10
#set terminal epslatex size 10cm,10cm color colortext
set terminal postscript eps size 15cm,10cm enhanced color font 'Helvetica,20' linewidth 2
set xlabel font 'Helvetica,28'
set ylabel font 'Helvetica,28'
# set xtics font 'Helvetica,25'
# set ytics font 'Helvetica,25'

#set xlabel 'Mechanism'
set ylabel 'Privacy Points'
set title 'Country         City'
set output 'boxes-pp.ps'

# set xr [0:30]
# set yr [0:5000]

#req
set style line 1 lt 1 lc rgb 'black'
#mappa
set style line 2 lt 1 lc rgb '#E1701F'
#lapla-w
set style line 3 lt 1 lc rgb 'red'
#lapla-s
set style line 4 lt 1 lc rgb 'blue'

set style boxplot nooutliers separation 0
set boxwidth -2
set style data boxplot
unset key
set xtics ("EM" 1, "PLl" 2, "PLh" 3, "EM" 4, "PLl" 5, "PLh" 6)
set xtics nomirror
set ytics nomirror

plot \
     1 lt 3 lc rgb 'black' with lines  title 'PLh', \
'weak-mappa-pp-ln2.dat'    using (1):2 ls 2, \
'weak-laplas-pp-ln2.dat'   using (2):2 ls 4, \
'weak-laplaw-pp-ln2.dat'   using (3):2 ls 3, \
'strong-mappa-pp-ln2.dat'  using (4):2 ls 2, \
'strong-laplas-pp-ln2.dat' using (5):2 ls 4, \
'strong-laplaw-pp-ln2.dat' using (6):2 ls 3

