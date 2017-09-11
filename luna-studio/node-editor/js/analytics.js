Mixpanel = require("mixpanel")

mixpanel = Mixpanel.init("0d906436719b047c86b7fee8ae550601", {
    protocol: 'https'
});

filters = []

function test(title) {
    for (var i = 0, len = filters.length; i < len; i++)
        if(filters[i].test(title))
            return true;
    return false;
}

module.exports = {
    setFilters: function(strings) {
        fliters = [];
        for (var i = 0, len = strings.length; i < len; i++)
            filters.push(new RegExp(strings[i]))
    },
    track: function (title, data) {
        if (atom.config.get('luna-studio.analyticsEnabled') && test(title))
            console.log("track: " + title, data);
            // mixpanel.track(title, data);
        else
            console.log("discard: " + title, data);
    }
}
