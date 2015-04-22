# Install Instructions #
Tested on Debian 8 Jessie.

## Build dependencies:
```bash
~$ sudo aptitude install libproj-dev libocamlgraph-ocaml-dev libcalendar-ocaml-dev libxml-light-ocaml-dev libpostgresql-ocaml-dev git make libocamlgsl-ocaml-dev ocaml-batteries-included gnuplot
```
A couple of libraries from sources:
```bash
~$ git clone https://github.com/hcarty/proj4ml
~$ cd proj4ml && make
~$ sudo make install
~$ git clone https://github.com/rdicosmo/parmap/
~$ cd parmap
~$ ./configure && make
~$ sudo make install
```

## Database:
```bash
~$ sudo aptitude install postgresql postgresql-client postgis postgresql-9.4-postgis-2.1 imposm
```

In `/etc/postgresql/9.4/main/pg_hba.conf` change the `local` connections from `peer` to `trust` and restart.
```bash
sudo /etc/init.d/postgresql restart
```

Set a password for the new user `postgres`, I just used `postgres` again:
```bash
sudo passwd postgres
```
and in the database:
```bash
~$ su - postgres
postgres~$ psql
ALTER USER Postgres WITH PASSWORD 'postgres';
CRTL-D
```

Now we can create the database:

```bash
postgres~$ createdb -E UTF8 -O postgres imposm
postgres~$ psql -d imposm -f /usr/share/postgresql/9.4/contrib/postgis-2.1/postgis.sql
postgres~$ psql -d imposm -f /usr/share/postgresql/9.4/contrib/postgis-2.1/spatial_ref_sys.sql
```

At [geofabrik](http://download.geofabrik.de/) there are daily regional extracts of the OSM database, in this example I'm using ile-de-france in pbf format. The schema used to import the data is defined in the file `schema.py`.
```bash
postgres~$ wget http://download.geofabrik.de/europe/france/ile-de-france-latest.osm.pbf
postgres~$ imposm -U postgres -d imposm --merge-cache --read --write --optimize --deploy-production-tables --proj EPSG:4326 --mapping-file /path/to/elastic/source/schema.py  ile-de-france-latest.osm.pbf
```
This import takes around 40 minutes and for larger areas imposm seems to hang so I'd suggest using the smallest possible area.

# Usage #
The evaluation of the elastic mechanism in contained in the file `evaluation.ml` that can be built with `make`. Several files are created so the program expects a working directory.

In terms of time consumption the evaluation can be split in 3 phases:
1. grid contruction, which implies querying postgres
2. metric construction, which runs the main algorithm to generate the graph
3. computation of statistics, comparison with polar laplace mechanism and dump of geojson

The fist two steps are cached to file so it is not necessary to re-run them if something changes later in the process.
In particular, if the program finds in the working directory a file called `merge.dump`, step 1 is skipped and the grid read from the file.
If the files `mappa-boxes.dump`, `mappa-edges.dump`, `mappa-nodes.dump` are present, then step 2 will be skipped and the elastic metric will be build from these files.

```bash
~$ make
~$ mkdir 750-paris
~$ ./do run 750-paris
```
When the run is finished in the file `note` there is a report of the run.
With the other files created it is possible to generate the graphs:
```bash
~$ ./do graphs 750-paris
```

Additionally the geojson files with extension `.json` can be inspected with the leaflet based viewer found in `viewer/index.html`.

# Documentation
Some OCaml documentation can be built with `make doc` and found in `elastic.docdir/index.html`.


# Predictive
Download [Geolife](http://research.microsoft.com/en-us/downloads/b16d359d-d164-469e-9fd4-daa38f2b2e13/)

```bash
~$ make
~$ mkdir test
~$ unzip path/to/Geolife\ Trajectories\ 1.3.zip -d test
  (first try it with just a few users, like 000 and 001)
~$ ./do-predictive test
convert geolife .plt files to .gpx
filter
sample every 30 minutes
sample every 60 minutes
sanitize sample 30m
sanitize sample 60m
generate graphs
```

The generated graphs will be in `test/gpx-filtered-sampled-1-{3.6}0-5-sanitized/*.pdf`.
