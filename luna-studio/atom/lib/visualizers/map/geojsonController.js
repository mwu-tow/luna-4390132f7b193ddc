(function () {
    var map               = null;
    var hoverPopupOptions = null;
    var popupOptions      = null;
    var hoverPopupOptionsSmall = {closeButton: false, autoPan: false, maxWidth: 200, maxHeight: 150};
    var popupOptionsSmall      = {autoPan: false, maxWidth: 200, maxHeight: 150};
    var hoverPopupOptionsBig   = {closeButton: false, autoPan: false, maxWidth: 400, maxHeight: 400};
    var popupOptionsBig        = {autoPan: false, maxWidth: 400, maxHeight: 400};
    var mapOptions = { center: [0,0], zoom: 0.2, minZoom: 0.2, closePopupOnClick: false }; //  maxBounds: [[-90, -Infinity], [180, Infinity]],  maxBoundsViscosity: 1.0};

    window.addEventListener("resize", function (e) {
        map.invalidateSize();
        width = map.getSize().x;
        height = map.getSize().y
        map.setMinZoom(Math.max(width, height)/1024);
    });

    window.addEventListener("load", function (e) {
        map = L.map('chart-container', mapOptions);
        L.tileLayer('https://api.tiles.mapbox.com/v4/{id}/{z}/{x}/{y}.png?access_token=pk.eyJ1IjoibWFwYm94IiwiYSI6ImNpejY4NXVycTA2emYycXBndHRqcmZ3N3gifQ.rJcFIG214AriISLbB6B5aw', {
            maxZoom: 18,
            attribution: 'Map data &copy; <a href="http://openstreetmap.org">OpenStreetMap</a> contributors, ' +
                '<a href="http://creativecommons.org/licenses/by-sa/2.0/">CC-BY-SA</a>, ' +
                'Imagery © <a href="http://mapbox.com">Mapbox</a>',
            id: 'mapbox.streets'
        }).addTo(map);
    });

    window.addEventListener("message", function (evt) {
        var data = JSON.parse(evt.data.data);
        if (evt.data.event == "data") {
            L.geoJSON(data, {style: function (f) { console.log(f); return f.properties.style; }}).addTo(map);
        }
    });
}());
