{View} = require 'atom-space-pen-views'
etch = require 'etch'
fuzzyFilter = null # defer until used
ProjectItem = require './project-item'
projects = require './projects'

projectClasses = "inline-block btn luna-project "
tutorialClasses = projectClasses + "luna-project-tutorial"
recentClasses = projectClasses + "luna-project-recent"
privateNewClasses = projectClasses + 'luna-project-private-new'
comunnityNewClasses = projectClasses + 'luna-project-community-new'

module.exports =
class LunaWelcomeTab extends View
    constructor: (@codeEditor) ->
        super

    @content: ->
        @div class: 'luna-welcome', =>
            @div class: 'luna-header block', =>
                @div class: 'luna-title inline-block', 'Luna Studio'
                @div class: 'luna-link luna-link-forum inline-block-tight', =>
                    @a
                        class: 'btn'
                        href: "http://luna-lang.org"
                        'forum'
                @div class: 'luna-link luna-link-chat inline-block-tight', =>
                    @a
                        class: 'btn'
                        href: "http://luna-lang.org"
                        'chat'
            @div class: 'luna-body', =>
                @div class: 'luna-search-input block', =>
                    @input
                        class: 'input-search native-key-bindings'
                        type: 'search'
                        placeholder: 'Search'
                        outlet: 'searchInput'
                @div class: 'luna-projects block', =>
                    @ul class: 'list-group', =>
                        @li
                            class: 'luna-search-results list-item'
                            outlet: 'searchResultsSection'
                            =>
                                @span class: 'luna-search-results-title icon icon-search', 'Search results'
                                @div class: 'luna-search-results-container block', outlet: 'searchResultsContainer', =>
                        @li
                            class: 'luna-tutorials-section list-item'
                            outlet: 'tutorialsSection'
                            =>
                                @span class: 'luna-tutorials-section-title icon icon-book', 'Tutorials'
                                @div class: 'luna-tutorials-section-container block', outlet: 'tutorialsContainer', =>

                        @li
                            class: 'luna-private-section list-item',
                            outlet: 'privateSection'
                            =>
                                @span class: 'luna-private-section-title icon icon-person', 'Private'
                                @div class: 'luna-private-section-container block', outlet: 'privateContainer', =>
                        @li
                            class: 'luna-community-section list-item'
                            outlet: 'communitySection'
                            =>
                                @span class: 'luna-community-section-title icon icon-organization',  'Community'
                                @div class: 'luna-community-section-container block', outlet: 'communityContainer', =>

    initialize: =>
        @tutorialItems = []
        @privateItems = []
        @privateNew = new ProjectItem({name: 'new project', uri: null}, privateNewClasses, (progress, finalize) =>
            finalize()
            atom.pickFolder (paths) => if paths? then atom.project.setPaths paths
            @detach())
        @communityItems = []
        @comunnityNew = new ProjectItem({name: 'new project', uri: null}, comunnityNewClasses, (progress, finalize) =>
            finalize()
            atom.confirm
                message: "Not supported yet"
                detailedMessage: "Community projects are not supported yet."
                buttons:
                    Ok: -> )
        @searchInput.on 'search', @search
        @searchInput.on 'keyup', @search

        @hideSearchResults()
        projects.recent.load (recentProjectPath) =>
            item = new ProjectItem {name: recentProjectPath}, recentClasses, (progress, finalize) =>
                progress 0.5
                atom.project.setPaths [recentProjectPath]
                finalize()
                @detach()
            @privateItems.push(item)
            @privateContainer.append(item.element)
        projects.tutorial.list (tutorials) =>
            for tutorial in tutorials
                item = new ProjectItem(tutorial, tutorialClasses, (progress, finalize) =>
                    projects.tutorial.open(tutorial, progress, =>
                        finalize()
                        @detach()))
                @tutorialItems.push(item)
                @tutorialsContainer.append(item.element)

    getFilterKey: ->
        return 'name'

    attach: (@mode) ->
        console.log 'attach'
        @panel ?= atom.workspace.addModalPanel({item: this, visible: false})
        console.log @panel
        @previouslyFocusedElement = document.activeElement
        @panel.show()
        console.log @panel
        @panel.show()

    detach: ->
        return unless @panel.isVisible()
        @panel.hide()
        @previouslyFocusedElement?.focus()

    search: =>
        filterQuery = @searchInput[0].value
        if filterQuery == ""
            @hideSearchResults()
        else
            fuzzyFilter ?= require('fuzzaldrin').filter
            allItems = @tutorialItems.concat(@privateItems)
            filteredItems = fuzzyFilter(allItems, filterQuery, key: @getFilterKey())
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
        @privateContainer.append(@privateNew.element)
        for privateItem in @privateItems
            @privateContainer.append(privateItem.element)

        @tutorialsContainer.empty()
        for tutorialItem in @tutorialItems
            @tutorialsContainer.append(tutorialItem.element)

        @communityContainer.empty()
        @communityContainer.append(@comunnityNew.element)
        for communityItem in @communityItems
            @communityContainer.append(communityItem.element)


        @searchResultsSection.hide()
        @communitySection.show()
        @privateSection.show()
        @tutorialsSection.show()

    getTitle: -> 'Welcome'
