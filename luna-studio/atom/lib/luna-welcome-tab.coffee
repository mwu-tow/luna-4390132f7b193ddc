{View} = require 'atom-space-pen-views'


module.exports =
class LunaWelcomeTab extends View

    constructor: (@codeEditor) ->
        super

    @content: ->
        @div =>
            @div class: 'block', =>
                @div class: 'inline-block', 'Luna Studio'
                @div class: 'inline-block-tight', =>
                    @a
                        class: 'btn'
                        href: "http://luna-lang.org"
                        'forum'
                @div class: 'inline-block-tight', =>
                    @a
                        class: 'btn'
                        href: "http://luna-lang.org"
                        'chat'
            @div class: 'block', =>
                @input
                    class: 'input-search'
                    type: 'search'
                    placeholder: 'Search'
            @div class: 'block', =>
                @h1 'Tutorials'
                @h1 'Private'
                @h1 'Community'

    getTitle:     -> 'Welcome'
