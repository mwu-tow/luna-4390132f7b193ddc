{View} = require 'atom-space-pen-views'

tmpSteps = [
        title: 'Step1'
        description: 'Step1 description'
    ,
        title: 'Step2'
        description: 'Step2 description'
]

module.exports =
    class VisualGuide extends View
        constructor: ->
            super

        @content: ->
            @div class: 'luna-guide-background', =>
                @div class: 'luna-guide-message', =>
                    @div
                        class: 'luna-guide-title'
                        outlet: 'guideTitle'
                    @div
                        class: 'luna-guide-description'
                        outlet: 'guideDescription'
                    @button
                        class: 'luna-guide-dismiss'
                        'Dismiss'


        initialize: =>
            @steps = tmpSteps

        nextStep: =>
            @currentStepNo ?= 0
            @currentStep = @steps[@currentStepNo]

            @guideTitle[0].innerText = @currentStep.title
            @guideDescription[0].innerText = @currentStep.description
            console.log @currentStep
            @currentStepNo++


        attach: =>
            @panel ?= atom.workspace.addHeaderPanel({item: this, visible: false})
            @panel.show()
            @nextStep()

        detach: =>
            if @panel.isVisible()
                @panel.hide()
