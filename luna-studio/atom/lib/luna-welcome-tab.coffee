{View} = require 'atom-space-pen-views'
etch = require 'etch'
ProjectItem = require './project-item'


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
                    class: 'input-search native-key-bindings'
                    type: 'search'
                    placeholder: 'Search'
                    outlet: 'searchInput'
            @div class: 'block', =>
                @ul class: 'list-group', =>
                    @li
                        class: 'list-item'
                        outlet: 'searchResultsSection'
                        =>
                            @span class: 'icon icon-search', 'Search results'
                    @li
                        class: 'list-item'
                        outlet: 'tutorialsSection'
                        =>
                            @span class: 'icon icon-book', 'Tutorials'
                            @div class: 'block', outlet: 'tutorialsContainer', =>

                    @li
                        class: 'list-item',
                        outlet: 'privateSection'
                        =>
                            @span class: 'icon icon-person', 'Private'
                            @div class: 'block', outlet: 'privateContainer', =>
                    @li
                        class: 'list-item'
                        outlet: 'communitySection'
                        =>
                            @span class: 'icon icon-organization',  'Community'
                            @div class: 'block', outlet: 'communityContainer', =>

    initialize: ->
        @hideSearchResults()
        @searchInput.on 'search', @search
        @searchInput.on 'keyup', @search
        @displayItems()

    search: =>
        if @searchInput[0].value == ""
            @hideSearchResults()
        else
            @showSearchResults()

    showSearchResults: =>
        @communitySection.hide()
        @privateSection.hide()
        @tutorialsSection.hide()
        @searchResultsSection.show()

    hideSearchResults: =>
        @searchResultsSection.hide()
        @communitySection.show()
        @privateSection.show()
        @tutorialsSection.show()

    displayItems: =>
        privates = [ new ProjectItem('new project', null)]
        publics = []
        tutorials =
            [ new ProjectItem('test', 'luna-studio/atom/test.luna')
            , new ProjectItem('test2', 'luna-studio/atom/test2.luna')
            , new ProjectItem('cryptocurrencies', 'luna-studio/atom/test-cryptocurrencies.luna')
            , new ProjectItem('list', 'luna-studio/atom/test-list.luna')
            ]
        for item in privates
            @privateContainer.append(item.element)
        for item in tutorials
            @tutorialsContainer.append(item.element)
        for item in publics
            @communityContainer.append(item.element)

    getTitle: -> 'Welcome'
