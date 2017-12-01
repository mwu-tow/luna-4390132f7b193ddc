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

filtersSet = false
userInfoSet = false
isConfigured = => filtersSet and userInfoSet

cachedEvents = []


cacheEvent = (title, data) =>
    cachedEvents.push
        title: title
        data: data

module.exports =

    setFilters: (strings) ->
        filters = []
        for str in strings
            filters.push new RegExp str
        filtersSet = true
        @sendCached()

    setUserInfo: (@userInfo) ->
        userInfoSet = true
        @sendCached()

    sendCached: ->
        if isConfigured()
            for event in cachedEvents
                @track event.title, event.data
            cachedEvents = []

    track: (title, data) ->
        data ?= {}
        data.user_info = @userInfo
        if isConfigured()
            if atom.config.get('luna-studio.analyticsEnabled') && test(title)
                if devMode
                    console.log ("track.accept: " + title), data
                else
                    mixpanel.track title, data
            else
                if devMode
                    console.log ("track.discard: " + title), data
        else
            cacheEvent title, data
