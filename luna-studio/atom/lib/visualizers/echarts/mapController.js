(function () {
    var map               = null;
    var markers           = null;
    var hoverPopupOptions = null;
    var popupOptions      = null;
    var hoverPopupOptionsSmall = {closeButton: false, autoPan: false, maxWidth: 200, maxHeight: 150};
    var popupOptionsSmall      = {autoPan: false, maxWidth: 200, maxHeight: 150};
    var hoverPopupOptionsBig   = {closeButton: false, autoPan: false, maxWidth: 400, maxHeight: 400};
    var popupOptionsBig        = {autoPan: false, maxWidth: 400, maxHeight: 400};
    var mapOptions = { center: [0,0], zoom: 0.2, minZoom: 0.2, closePopupOnClick: false, maxBounds: [[-90, -180], [90.0, 180]],  maxBoundsViscosity: 1.0};

    window.addEventListener("resize", function () {
        if (document.body.clientHeight < 500 && document.body.clientWidth < 500) {
            hoverPopupOptions = hoverPopupOptionsSmall;
            popupOptions = popupOptionsSmall;
        } else {
            hoverPopupOptions = hoverPopupOptionsBig;
            popupOptions = popupOptionsBig;
        }
        map.closePopup();
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
        markers = L.featureGroup().addTo(map);
        if (document.body.clientHeight < 500 && document.body.clientWidth < 500) {
            hoverPopupOptions = hoverPopupOptionsSmall;
            popupOptions = popupOptionsSmall;
        } else {
            hoverPopupOptions = hoverPopupOptionsBig;
            popupOptions = popupOptionsBig;
        }
    });

    window.addEventListener("message", function (evt) {
        var data = JSON.parse(evt.data.data);
        if (evt.data.event == "data") {
            markers.clearLayers();
            if (data[0] && Array.isArray(data[0]) && Array.isArray(data[0][0])) {
                data.forEach(function (entry) {
                    var marker = L.marker(entry[0]);
                        clicked = false;
                        processingClick = false;
                    marker.on('mouseover', function () {
                        if (!clicked) {
                            marker.unbindPopup();
                            marker.bindPopup(entry[1], hoverPopupOptions);
                            this.openPopup();
                        }
                    });
                    marker.on('mouseout', function () { if (!clicked) this.closePopup(); });
                    marker.on('click', function () {
                        processingClick = true;
                        marker.unbindPopup();
                        marker.bindPopup(entry[1], popupOptions);
                        this.openPopup();
                        clicked = true;
                        processingClick = false;
                    });
                    marker.on('popupclose', function () { clicked = false; });
                    marker.on('popupopen', function(e) {
                        if (processingClick) {
                            var px = map.project(e.popup._latlng);
                            px.y -= e.popup._container.clientHeight/2;
                            map.panTo(map.unproject(px),{animate: true});
                        }
                    });
                    marker.addTo(markers);
                });
            } else {
                data.forEach(function (entry) {
                    L.marker(entry).addTo(markers);
                });
            }
        }
        map.fitBounds(markers.getBounds());
    });
}());
