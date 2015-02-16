'use strict';

// create map
var map = new L.map('map',{
    center : [51.505, -0.09],
    zoom : 13,
    attributionControl : false
});
L.control.scale({'imperial': false}).addTo(map);
var controlLayers = L.control.layers().addTo(map);

// add osm tile layer
// var osm = new L.tileLayer('http://{s}.tiles.mapbox.com/v3/MapID/{z}/{x}/{y}.png', {
//     attribution: 'Map data &copy; <a href="http://openstreetmap.org">OpenStreetMap</a> contributors, <a href="http://creativecommons.org/licenses/by-sa/2.0/">CC-BY-SA</a>, Imagery © <a href="http://mapbox.com">Mapbox</a>',
//     maxZoom: 18
// }).addTo(map);

var osm = new L.tileLayer('http://{s}.tile.osm.org/{z}/{x}/{y}.png', {
    attribution: '&copy; <a href="http://osm.org/copyright">OpenStreetMap</a> contributors'
}).addTo(map);
controlLayers.addBaseLayer(osm, "OpenStreetMap");



function makeColor(list){
    var cnt = 0;
    return function color(){cnt+=1; var col= list[cnt]; console.log(cnt +" " +col); return col};
}
//             blue        red        green       yellow   light blue  purple        black
var colors = ["#0000ff", "#ff0000", "#00ff00", "#ffff00", "#00ffff", "#ff00ff", "#000000"];

var colorIter = makeColor(colors);



// returns options for geoJson layer
function geoOptions(trackColor) {
    return {
        // draws popups
        onEachFeature: function (feature, layer) {
            if (feature.properties) {
                var popupString = '<div class="popup">';
                for (var k in feature.properties) {
                    var v = feature.properties[k];
                    popupString += k + ': ' + v + '<br />';
                }
                popupString += '</div>';
                layer.bindPopup(popupString, {
                    maxHeight: 200
                });
            }
        },
        // draws markers
        pointToLayer: function (feature, latlng) {
            var opa = 0.8;
            if (feature.properties.hard) {opa = 1};
            return L.circleMarker(latlng, {
                radius: 8,
                fillColor: trackColor,
                color: "#000",
                weight: 1,
                opacity: opa,
                fillOpacity: opa
            });
        },
        style: function(feature) {
            var colors = ["#FFFFFF", //white
                          "#FFFF00", //yellow
                          "#FFCC00", //
                          "#FF9900", //orange
                          "#FF6600",
                          "#FF3300", 
                          "#FF0000", //red
                          "#CC0000",
                          "#990000", //darkred
                          "#660000",
                          "#330000", //almost black
                          "#000000"  //black
                         ];
            var color_id = -1;
            if (feature.properties.pp != null) {
                color_id = feature.properties.pp * colors.length;
                console.log("pp case");
            }
            else if (feature.properties.pdf != null) {
                color_id = feature.properties.pdf_n * colors.length;
                console.log("pdf case");
            }
            else if (feature.properties.err != null) {
                color_id = feature.properties.err_n * colors.length;
                console.log("err case");
            }
            else {
                color_id = feature.properties.weight_n * colors.length;
                console.log("weight case");
            }
            var trackColor = colors[Math.floor(color_id)];

            var opacity = 0.7;
            if (color_id == 0) {
                opacity = 0
            }
            return {color: trackColor, 
                    fillColor: trackColor,
                    fillOpacity: opacity,
                    opacity: 0.7,
                    weight : 1};
        }

       // filter could be usefull to show a subset of points
    }
}



// This global variable takes track of all trackLayers
var trackLayerList = [];
function addTrackLayerToMap (tl) {
    tl.addTo(map);
    trackLayerList.push(tl);
}
function maxBounds () {
    var bb = new L.LatLngBounds();
    for (var i=0; i < trackLayerList.length; i++) {
        var tl = trackLayerList[i];
        bb.extend(tl.getBounds());
    }
    return bb;
}

function handleFileSelect(evt) {
    var files = evt.target.files; // FileList object

    var reader = null;
    var trackLayer = null;

    for (var i = 0; i<files.length; i++) {

        var f = files[i];
        reader = new FileReader();

        reader.onload = (function(file){
            return function(e){
                // var col = colorIter(); 
                // console.log(file.name +" color "+col);

                // This is the part you should check out
                trackLayer = new L.GeoJSON(null, geoOptions());
                trackLayer.addData(JSON.parse(e.target.result));
                addTrackLayerToMap(trackLayer);
                map.fitBounds(maxBounds());
                controlLayers.addOverlay(trackLayer, file.name);
            }
        })(f);                  // closure trick to remember f

        reader.readAsText(f);

    }
}
document.getElementById('files').addEventListener('change', handleFileSelect, false);
