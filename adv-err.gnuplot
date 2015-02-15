set autoscale                        # scale axes automatically
unset log                              # remove any log-scaling
unset label                            # remove any previous labels
#set xtic .1                          # set xtics automatically
#set ytic auto                          # set ytics automatically
set tics nomirror
#set term pngcairo size 1000,1000 enhanced font 'Verdana,10'

#set terminal svg enhanced fname 'Verdana, Helvetica, Arial, sans-serif' fsize 10
#set terminal epslatex size 10cm,10cm color colortext
set terminal postscript eps size 20cm,10cm enhanced color font 'Helvetica,20' linewidth 2
set xlabel font 'Helvetica,28'
set ylabel font 'Helvetica,28'
# set xtics font 'Helvetica,25'
# set ytics font 'Helvetica,25'

#set xlabel 'Mechanism'
#set ylabel 'Aversarial Error (m)'

# set xr [0:30]
 set yr [0.65:1]

#req
set style line 1 lt 1 lc rgb 'black'
#elastic
set style line 2 lt 1 lc rgb '#E1701F'
#lapla-w
set style line 3 lt 1 lc rgb 'red'
#lapla-s
set style line 4 lt 1 lc rgb 'blue'
set style fill solid 0.35
#set style fill pattern


set style boxplot nooutliers pointtype 19
set style data boxplot
set xtics nomirror
set ytics nomirror

# set multiplot layout 1,2 title global_title

# unset key
# unset title
# set xtics ("EM city" 1, "PL city" 2)
# plot \
#      elastic_city    using (1):1 ls 2 title '', \
#      lapla_city      using (2):1 ls 3 title ''


# unset key
# unset title
# set xtics ("EM suburb" 1, "PL suburb" 2)
# plot \
#      elastic_country using (1):1 ls 2 title '', \
#      lapla_country   using (2):1 ls 3 title ''

#set title global_title
set xtics ("EM city" 1, "PL city" 2, "EM suburb" 3, "PL suburb" 4)
plot \
     elastic_city    using (1):1 ls 2 title '', \
     lapla_city      using (2):1 ls 4 title '', \
     elastic_country using (3):1 ls 2 title '', \
     lapla_country   using (4):1 ls 4 title ''
