"use strict";
var Mixpanel = require("mixpanel");

var mixpanel = Mixpanel.init("0d906436719b047c86b7fee8ae550601", {
    protocol: 'https'
});

var devMode = process.env.LUNA_STUDIO_DEVELOP !== null;

var filters = [];

function test(title) {
    for (var i = 0, len = filters.length; i < len; i++)
        if(filters[i].test(title))
            return true;
    return false;
}

module.exports = {
    setFilters: function(strings) {
        filters = [];
        for (var i = 0, len = strings.length; i < len; i++)
            filters.push(new RegExp(strings[i]));
    },
    track: function (title, data) {
        if (atom.config.get('luna-studio.analyticsEnabled') && test(title)) {
            if(devMode) {
                console.log("track.accept: " + title, data);
            } else {
                mixpanel.track(title, data);
            }
        } else {
            if(devMode) {
                console.log("track.discard: " + title, data);
            }
        }
    }
};
