#!/bin/bash

NAME=evaluation


function run {
    OCAMLRUNPARAM='b' /usr/bin/time -v -o time.temp ./$NAME.native "$1" 1>"$1/note"
    cat time.temp >> "$1/note"
    rm time.temp
}

function profile_time {
    OCAMLRUNPARAM='b' /usr/bin/time -v -o time.temp perf record -g ./$NAME.p.native "$1" 1>"$1/note"
    cat time.temp >> "$1/note"
    rm time.temp
    perf report -g
}

function clean {
         ocamlbuild -clean
}

function deploy {
    AWS=
    VERSION=$(git log -1 --pretty=%h)
    rsync -PaL *.ml Makefile do _tags schema.py *.gnuplot $AWS:src-$VERSION
}

function graphs {
    gnuplot -e "set title 'Park'; set output 'park.ps'; mappa='mappa-weak.dat'; laplaw='lapla-weak-weak.dat'; laplas='lapla-strong-weak.dat';" ../png.gnuplot    
    gnuplot -e "set title 'Mall'; set output 'mall.ps'; mappa='mappa-strong.dat'; laplaw='lapla-weak-strong.dat'; laplas='lapla-strong-strong.dat';" ../png.gnuplot    
    gnuplot ../box-pp.gnuplot    
    laplaw=$(cat laplaw-err.dat)
    laplas=$(cat laplas-err.dat)
    echo $laplaw $laplas
    gnuplot -e "set title 'Average Error'; set output 'boxes-err.ps'; mappa_strong='strong-mappa-err.dat'; mappa_weak='weak-mappa-err.dat'; laplaw=$laplaw; laplas=$laplas;" ../box-err.gnuplot    
    for i in $(ls *.ps); do ps2pdf -dEPSCrop $i; done
}

case "$1" in    
    run)
        make clean
        make native
        run "$2"
	;;
    graphs)
        cd "$2"
        graphs
        ;;
    profile)
	make clean
        make profile
        profile_time "$2"
        ;;
    query)
        query
        ;;
    deploy)
        deploy
        ;;
    *)
        echo "Usage:
./do run <dir>
./do graphs <dir>
./do deploy
./do profile <dir>
./do query
"
        ;;
esac
