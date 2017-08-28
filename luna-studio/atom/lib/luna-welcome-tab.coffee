{View} = require 'atom-space-pen-views'
etch = require 'etch'
fuzzyFilter = null # defer until used
ProjectItem = require './project-item'
projects = require './projects'

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
                            @div class: 'block', outlet: 'searchResultsContainer', =>
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

    initialize: =>
        @privateItems = []
        @tutorialItems = []
        @searchInput.on 'search', @search
        @searchInput.on 'keyup', @search
        projects.recent.load (recentProjectPath) =>
            item = new ProjectItem(recentProjectPath)
            @privateItems.push(item)
            @privateContainer.append(item.element)
        projects.tutorial.list (tutorials) =>
            for tutorial in tutorials
                item = new ProjectItem(tutorial, tutorial, => projects.tutorial.open(tutorial))
                @tutorialItems.push(item)
                @tutorialsContainer.append(item.element)
        @hideSearchResults()

    getFilterKey: ->
        return 'name'

    search: =>
        filterQuery = @searchInput[0].value
        if filterQuery == ""
            @hideSearchResults()
        else
            fuzzyFilter ?= require('fuzzaldrin').filter
            filteredItems = fuzzyFilter(@privateItems, filterQuery, key: @getFilterKey())
            @showSearchResults filteredItems


    showSearchResults: (searchResults) =>
        @searchResultsContainer.empty()
        for item in searchResults
            @searchResultsContainer.append item.element

        @communitySection.hide()
        @privateSection.hide()
        @tutorialsSection.hide()
        @searchResultsSection.show()

    hideSearchResults: =>
        @privateContainer.empty()
        for privateItem in @privateItems
            @privateContainer.append(privateItem.element)

        @tutorialsContainer.empty()
        for tutorialItem in @tutorialItems
            @tutorialsContainer.append(tutorialItem.element)

        @searchResultsSection.hide()
        @communitySection.show()
        @privateSection.show()
        @tutorialsSection.show()

    getTitle: -> 'Welcome'
