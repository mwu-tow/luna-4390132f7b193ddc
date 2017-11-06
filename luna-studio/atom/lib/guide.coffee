{View} = require 'atom-space-pen-views'

tmpSteps = [
        title: 'Step 1'
        description: 'Use search bar to search for projects'
        target:
            className: 'luna-welcome-search'
            action: 'click'

        after: (storage) ->
            storage.lastItem = 'test'
            console.log 'after fired!'
    ,
        title: 'Step 2'
        description: 'Type some keywords'
        target:
            className: 'luna-welcome-search'
            action: 'keydown'
    ,
        title: 'Step 3'
        description: 'Click on forum button'
        target:
            className: 'luna-welcome-link--forum'
            action: 'click'
]

focusClass = 'luna-guide-focus'

module.exports =
    class VisualGuide extends View
        constructor: ->
            super

        @content: ->
            @div =>
            # @div class: 'luna-guide-background', =>
                @div class: 'luna-guide-message', outlet: 'messageBox', =>
                    @div
                        class: 'luna-guide-title'
                        outlet: 'guideTitle'
                    @div
                        class: 'luna-guide-description'
                        outlet: 'guideDescription'
                    @button
                        outlet: 'buttonHide'
                        class: 'luna-guide-hide'
                        'Hide'
                    @button
                        outlet: 'buttonDisable'
                        class: 'luna-guide-disable'
                        'Do not show again'

        initialize: =>
            @steps = tmpSteps
            @buttonHide.on 'click', => @detach()
            @buttonDisable.on 'click', => @disable()

            @storage = {}

        nextStep: =>
            if @currentStep? and @currentStep.after?
                @currentStep.after @storage

            @currentStepNo ?= 0
            @currentStep = @steps[@currentStepNo]
            @currentStepNo++

            unless @currentStep?
                @detach()
                return

            @guideTitle[0].innerText = @currentStep.title
            @guideDescription[0].innerText = @currentStep.description
            target = @currentStep.target
            focus = null
            if target.className
                focus = document.getElementsByClassName(target.className)[0]
            else if target.id
                focus = document.getElementById(target.id)[0]
            else if target.custom
                focus = target.custom @storage

            if focus?
                focus.classList.add focusClass
                focusRect = focus.getBoundingClientRect()
                @messageBox[0].style.top = focusRect.top + 'px'
                @messageBox[0].style.left = (focusRect.left - 100) + 'px'

                target.action ?= 'click'
                if target.action is 'click'
                    oldHandlers = focus.onclick
                    focus.onclick = =>
                        focus.onclick = oldHandlers
                        @nextStep()
                else if target.action is 'keydown'
                    oldHandlers = focus.onkeydown
                    focus.onkeydown = =>
                        focus.onkeydown = oldHandlers
                        @nextStep()

        attach: =>
            @panel ?= atom.workspace.addHeaderPanel({item: this, visible: false})
            @panel.show()
            @nextStep()

        detach: =>
            if @panel.isVisible()
                @panel.hide()

        disable: =>
            atom.config.set('luna-studio.showWelcomeGuide', false)
            @detach()
