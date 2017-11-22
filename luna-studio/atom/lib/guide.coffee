{View} = require 'atom-space-pen-views'
fs     = require 'fs-plus'
path   = require 'path'
yaml   = require 'js-yaml'
{VM}   = require 'vm2'

vm     = new VM
            timeout: 1000
            sandbox:
                window: window
                storage: {}

welcomeGuide = steps: []

encoding = 'utf8'
highlightClass = 'luna-guide-highlight'

module.exports =
    class VisualGuide extends View
        constructor: ->
            super

        @content: ->
            @div =>
                @div class: 'luna-guide-message', outlet: 'messageBox', =>
                    @div
                        class: 'luna-guide-title'
                        outlet: 'guideTitle'
                    @div
                        class: 'luna-guide-description'
                        outlet: 'guideDescription'
                    @button
                        outlet: 'buttonContinue'
                        class: 'luna-guide-button luna-guide-continue'
                        'Continue'
                    @button
                        outlet: 'buttonHide'
                        class: 'luna-guide-button luna-guide-hide'
                        'Hide'
                    @button
                        outlet: 'buttonDisable'
                        class: 'luna-guide-button luna-guide-disable'
                        'Do not show again'

        initialize: =>
            @buttonHide.on 'click', => @detach()
            @buttonDisable.on 'click', => @disable()
            @buttonContinue.on 'click', =>
                @nextStep()
                @buttonContinue.hide()
            @buttonContinue.hide()

        nextStep: =>
            if @currentStep? and @currentStep.after?
                try
                    vm.run @currentStep.after
                catch error
                    console.error error
            if @highlightedElem?
                @highlightedElem.classList.remove highlightClass

            @currentStep = @guide.steps[@currentStepNo]
            @currentStepNo++

            if @currentStep?
                @display()
            else
                @detach()

        display: =>
            target = @currentStep.target
            target ?= {}
            target.action ?= 'proceed'

            @highlightedElem = null
            if target.className
                if typeof target.className == 'string'
                    @highlightedElem = document.getElementsByClassName(target.className)[0]
                else
                    for t in target.className
                        if @highlightedElem?
                            @highlightedElem = @highlightedElem.getElementsByClassName(t)[0]
                        else
                            @highlightedElem = document.getElementsByClassName(t)[0]
                            unless @highlightedElem?
                                break
            else if target.id
                @highlightedElem = document.getElementById(target.id)
            else if target.custom
                @highlightedElem = vm.run target.custom

            msgBoxWidth = 200
            msgBoxHeight = 50
            windowRect = document.body.getBoundingClientRect()
            msgBoxLeft = (windowRect.width - msgBoxWidth)/2
            msgBoxTop  = (windowRect.height - msgBoxHeight)/2

            if target.action is 'proceed'
                @buttonContinue.show()
            else if not @highlightedElem?
                @guideTitle[0].innerText = @currentStep.title
                @guideDescription[0].innerText = 'Please wait...'
                @messageBox[0].style.width = msgBoxWidth + 'px'
                @messageBox[0].style.height = msgBoxHeight + 'px'
                @messageBox[0].style.top = msgBoxTop + 'px'
                @messageBox[0].style.left = msgBoxLeft + 'px'

                setTimeout(@display, 300)
                return

            @guideTitle[0].innerText = @currentStep.title
            @guideDescription[0].innerText = @currentStep.description

            if @highlightedElem?
                @highlightedElem.classList.add highlightClass
                highlightedRect = @highlightedElem.getBoundingClientRect()
                if highlightedRect.width != 0 and highlightedRect.height != 0

                    if target.action is 'value'
                        oldHandlers = @highlightedElem.onkeyup
                        @highlightedElem.onkeyup = =>
                            if @highlightedElem.value is target.value
                                @highlightedElem.onkeyup = oldHandlers
                                @nextStep()
                    else if target.action.includes ':'
                        handler = {}
                        handler[target.action] = =>
                            @disposable.dispose()
                            @nextStep()
                        @disposable = atom.commands.add @highlightedElem, handler
                    else if @highlightedElem?
                        oldHandlers = @highlightedElem[target.action]
                        @highlightedElem[target.action] = =>
                            @highlightedElem[target.action] = oldHandlers
                            @nextStep()

                    if highlightedRect.left > msgBoxWidth
                        msgBoxLeft = highlightedRect.left - msgBoxWidth
                        msgBoxTop = highlightedRect.top
                    else if highlightedRect.right + msgBoxWidth < windowRect.width
                        msgBoxLeft = highlightedRect.right
                        msgBoxTop = highlightedRect.top
                    else if highlightedRect.top > msgBoxHeight
                        msgBoxTop = highlightedRect.top - msgBoxHeight
                    else if highlightedRect.bottom + msgBoxHeight < windowRect.height
                        msgBoxTop = highlightedRect.bottom


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
                        parsed = yaml.safeLoad data
                        if parsed? && not parsed.disabled
                            @start parsed, guidePath

        disableGuide: =>
            if @guidePath?
                @guide.disabled = true
                data = yaml.safeDump(@guide)
                fs.writeFile @guidePath, data, encoding, (err) =>
                if err?
                    console.error err
            else
                atom.config.set('luna-studio.showWelcomeGuide', false)


        start: (@guide, @guidePath) =>
            @guide ?= welcomeGuide
            @currentStepNo = 0
            @attach()
            @nextStep()
