{View} = require 'atom-space-pen-views'
fs     = require 'fs-plus'
path   = require 'path'
yaml   = require 'js-yaml'


tmpGuide =
    steps: [
        title: 'Step 0'
        description: 'Hello guide'
    ,
        title: 'Step 1'
        description: 'Use search bar to search for projects'
        target:
            className: 'luna-welcome-search'
            action: 'onclick'

        after: (storage) ->
            storage.lastItem = 'test'
            console.log 'after fired!'
    ,
        title: 'Step 2'
        description: 'Type "Crypto"'
        target:
            className: 'luna-welcome-search'
            action: 'value'
            value: 'Crypto'
    ,
        title: 'Step 3'
        description: 'Click on forum button'
        target:
            className: 'luna-welcome-link--forum'
            action: 'onclick'
    ,
        title: 'Step 4'
        description: 'Guide finished'
]

encoding = 'utf8'
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
                        outlet: 'buttonContinue'
                        class: 'luna-guide-continue'
                        'Continue'
                    @button
                        outlet: 'buttonHide'
                        class: 'luna-guide-hide'
                        'Hide'
                    @button
                        outlet: 'buttonDisable'
                        class: 'luna-guide-disable'
                        'Do not show again'

        initialize: =>
            @buttonHide.on 'click', => @detach()
            @buttonDisable.on 'click', => @disable()
            @buttonContinue.on 'click', =>
                @nextStep()
                @buttonContinue.hide()
            @buttonContinue.hide()
            @storage = {}

        nextStep: =>
            if @currentStep? and @currentStep.after?
                @currentStep.after @storage

            @currentStep = @guide.steps[@currentStepNo]
            @currentStepNo++

            unless @currentStep?
                @detach()
                return

            @guideTitle[0].innerText = @currentStep.title
            @guideDescription[0].innerText = @currentStep.description
            target = @currentStep.target
            target ?= {}
            target.action ?= 'proceed'

            focus = null
            if target.className
                focus = document.getElementsByClassName(target.className)[0]
            else if target.id
                focus = document.getElementById(target.id)[0]
            else if target.custom
                focus = target.custom @storage

            if target.action is 'proceed'
                @buttonContinue.show()
            else if target.action is 'value'
                oldHandlers = focus.onkeyup
                focus.onkeyup = =>
                    if focus.value is target.value
                        focus.onkeyup = oldHandlers
                        @nextStep()
            else if focus?
                oldHandlers = focus[target.action]
                focus[target.action] = =>
                    focus[target.action] = oldHandlers
                    @nextStep()

            msgBoxWidth = 200
            msgBoxHeight = 50
            windowRect = document.body.getBoundingClientRect()
            msgBoxLeft = (windowRect.width - msgBoxWidth)/2
            msgBoxTop  = (windowRect.height - msgBoxHeight)/2

            if focus?
                focus.classList.add focusClass
                focusRect = focus.getBoundingClientRect()
                if focusRect.width != 0 and focusRect.height != 0
                    if focusRect.left > msgBoxWidth
                        msgBoxLeft = focusRect.left - msgBoxWidth
                        msgBoxTop = focusRect.top
                    else if focusRect.right + msgBoxWidth < windowRect.width
                        msgBoxLeft = focusRect.right
                        msgBoxTop = focusRect.top
                    else if focusRect.top > msgBoxHeight
                        msgBoxTop = focusRect.top - msgBoxHeight
                    else if focusRect.bottom + msgBoxHeight < windowRect.height
                        top = focusRect.bottom

            @messageBox[0].style.width = msgBoxWidth + 'px'
            @messageBox[0].style.height = msgBoxHeight + 'px'
            @messageBox[0].style.top = msgBoxTop + 'px'
            @messageBox[0].style.left = msgBoxLeft + 'px'


        attach: =>
            @panel ?= atom.workspace.addHeaderPanel({item: this, visible: false})
            @panel.show()

        detach: =>
            if @panel.isVisible()
                @panel.hide()

        disable: =>
            @disableGuide()
            @detach()

        startProject: =>
            projectPath = atom.project.getPaths()[0]
            if projectPath?
                guidePath = path.join projectPath, 'guide.yml'
                fs.readFile guidePath, encoding, (err, data) =>
                    unless err
                        parsed = yaml.load data
                        if parsed? && not parsed.disabled?
                            @start parsed, guidePath

        disableGuide: =>
            if @guidePath?
                @guide.disabled = null
                data = yaml.dump(@guide)
                fs.writeFile @guidePath, data, encoding, (err) =>
                if err?
                    console.error err
            else
                atom.config.set('luna-studio.showWelcomeGuide', false)


        start: (@guide, @guidePath) =>
            @guide ?= tmpGuide
            @currentStepNo = 0
            @attach()
            @nextStep()
