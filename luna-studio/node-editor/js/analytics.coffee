Mixpanel = require 'mixpanel'

mixpanel = Mixpanel.init "0d906436719b047c86b7fee8ae550601",
    protocol: 'https'

devMode = process.env.LUNA_STUDIO_DEVELOP?

filters = []

test = (title) =>
    for filter in filters
        if filter.test title
            return true
    return false

module.exports =
    setFilters: (strings) =>
        filters = []
        for str in strings
            filters.push new RegExp str

    setUserInfo: (@userInfo) =>

    track: (title, data) =>
        data ?= {}
        data.user_info = @userInfo
        if atom.config.get('luna-studio.analyticsEnabled') && test(title)
            if devMode
                console.log ("track.accept: " + title), data
            else
                mixpanel.track title, data
        else
            if devMode
                console.log ("track.discard: " + title), data
