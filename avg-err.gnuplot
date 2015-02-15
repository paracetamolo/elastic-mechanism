set autoscale                        # scale axes automatically
unset log                              # remove any log-scaling
unset label                            # remove any previous labels
#set xtic .1                          # set xtics automatically
#set ytic auto                          # set ytics automatically
set tics nomirror
#set term pngcairo size 1000,1000 enhanced font 'Verdana,10'

#set terminal svg enhanced fname 'Verdana, Helvetica, Arial, sans-serif' fsize 10
#set terminal epslatex size 10cm,10cm color colortext
set terminal postscript eps size 10cm,10cm enhanced color font 'Helvetica,20' linewidth 2
set xlabel font 'Helvetica,28'
set ylabel font 'Helvetica,28'
# set xtics font 'Helvetica,25'
# set ytics font 'Helvetica,25'

#set xlabel 'Mechanism'
#set ylabel 'Aversarial Error (m)'

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
set style fill solid 0.35
#set style fill pattern

set style boxplot nooutliers pointtype 19
set style data boxplot
#unset key
set xtics ("EM city" 1, "EM country" 2)
set xtics nomirror
set ytics nomirror

plot \
     elastic_city    using (1):2 ls 2 title '', \
     elastic_country using (2):2 ls 2 title '', \
     lapla ls 4 title 'PL'

#pause -1
