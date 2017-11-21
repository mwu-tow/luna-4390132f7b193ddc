{View} = require 'atom-space-pen-views'

module.exports =
class LunaToolbar extends View
    constructor: (@codeEditor) ->
        super

    @content: ->
        @div
            class: 'luna-toolbar'
            =>
                @div
                    class: 'luna-toolbar__container'
                    outlet: 'logoContainer'
                    =>
                        @img
                            class: 'luna-toolbar__button luna-toolbar__logo'
                            src: 'atom://luna-studio/rsc/logo.svg'
                            outlet: 'buttonLogo'
                @div
                    class: 'luna-toolbar__container'
                    outlet: 'buttonContainer'
                    =>
                        @div
                            class: 'luna-toolbar__button icon-plus'
                            outlet: 'buttonNew'
                        @div
                            class: 'luna-toolbar__button icon-search'
                            outlet: 'buttonSearch'
                        @div
                            class: 'luna-toolbar__button icon-gear'
                            outlet: 'buttonSettings'
    initialize: =>
        target = atom.views.getView atom.workspace
        connectButton = (btn, action) =>
            btn.on 'click', ->
                atom.commands.dispatch target, action

        connectButton @buttonLogo    , 'luna-studio:welcome'
        connectButton @buttonNew     , 'application:new-file'
        connectButton @buttonSearch  , 'fuzzy-finder:toggle-file-finder'
        connectButton @buttonSettings, 'application:show-settings'


    attach: =>
        @panel ?= atom.workspace.addLeftPanel({item: this, visible: false})
        @previouslyFocusedElement = document.activeElement
        @panel.show()

    detach: =>
        if @panel and @panel.isVisible()
            @panel.hide()
            @previouslyFocusedElement?.focus()

    getTitle: -> 'Luna toolbar'
