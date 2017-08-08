{View} = require 'atom-space-pen-views'


module.exports =
class LunaWelcomeTab extends View

    constructor: (@codeEditor) ->
        super

    @content: ->
        @div =>
            @h1 "Welcome to Luna Studio"

    getTitle:     -> 'Welcome'
