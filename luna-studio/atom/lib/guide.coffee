{View} = require 'atom-space-pen-views'

tmpSteps = [
        title: 'Step1'
        description: 'Step1 description'
        target: { className: 'luna-welcome-search'}
    ,
        title: 'Step2'
        description: 'Step2 description'
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

        nextStep: =>
            @currentStepNo ?= 0
            @currentStep = @steps[@currentStepNo]

            @guideTitle[0].innerText = @currentStep.title
            @guideDescription[0].innerText = @currentStep.description
            target = @currentStep.target
            focus = null
            if target.className
                focus = document.getElementsByClassName(target.className)[0]
            if focus?
                focus.classList.add focusClass
                focusRect = focus.getBoundingClientRect()
                @messageBox[0].style.top = focusRect.top + 'px'
                @messageBox[0].style.left = (focusRect.left - 100) + 'px'
            @currentStepNo++


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
