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
highlightClass = 'luna-guide__highlight'
pointerWindowClass = 'luna-guide__pointer--window'

module.exports =
    class VisualGuide extends View
        constructor: ->
            super

        @content: ->
            @div =>
                @div class: 'luna-guide__pointer', outlet: 'pointer'
                @div class: 'luna-guide__message', outlet: 'messageBox', =>
                    @div
                        class: 'luna-guide__title'
                        outlet: 'guideTitle'
                    @div
                        class: 'luna-guide__description'
                        outlet: 'guideDescription'
                    @div class: 'luna-guide__buttons', =>
                        @button
                            outlet: 'buttonContinue'
                            class: 'luna-guide__button luna-guide__button--continue'
                            'Continue'
                        @button
                            outlet: 'buttonDoIt'
                            class: 'luna-guide__button luna-guide__button--doit'
                            'Next'
                        @button
                            outlet: 'buttonHide'
                            class: 'luna-guide__button luna-guide__button--hide luna-guide__button--link'
                            'Hide'
                        @button
                            outlet: 'buttonDisable'
                            class: 'luna-guide__button luna-guide__button--disable luna-guide__button--link'
                            'Do not show again'

        initialize: =>
            @buttonHide.on 'click', @detach
            @buttonDisable.on 'click', @disable
            @buttonContinue.on 'click', =>
                @nextStep()
                @buttonContinue.hide()
            @buttonContinue.hide()
            @buttonDoIt.on 'click', =>
                @buttonDoIt.hide()
                @doIt()
            @buttonDoIt.hide()

        nextStep: =>
            if @currentStep? and @currentStep.after?
                try
                    vm.run @currentStep.after
                catch error
                    console.error error

            @unsetHighlightedElem()

            @currentStep = @guide.steps[@currentStepNo]
            @currentStepNo++

            if @currentStep?
                @target = @currentStep.target
                @target ?= {}
                @target.action ?= 'proceed'
                @displayStep()
            else
                @detach()


        findTargetedElem: =>
            targetedElem = null
            if @target.className
                if typeof @target.className == 'string'
                    targetedElem = document.getElementsByClassName(@target.className)[0]
                else
                    for t in @target.className
                        if targetedElem?
                            targetedElem = targetedElem.getElementsByClassName(t)[0]
                        else
                            targetedElem = document.getElementsByClassName(t)[0]
                            unless targetedElem?
                                break
            else if @target.id
                targetedElem = document.getElementById(@target.id)
            else if @target.custom
                targetedElem = vm.run @target.custom
            return targetedElem

        setHighlightedElem: =>
            @highlightedElem = @findTargetedElem()
            if @highlightedElem?
                @highlightedElem.classList.add highlightClass
            @updatePointer()

        unsetHighlightedElem: =>
            if @highlightedElem?
                @highlightedElem.classList.remove highlightClass
                @highlightedElem = null
            @updatePointer()

        installHandlers: =>
            if @highlightedElem?
                highlightedRect = @highlightedElem.getBoundingClientRect()
                if highlightedRect.width != 0 and highlightedRect.height != 0
                    if @target.action is 'value'
                        @buttonDoIt.show()
                        oldHandlers = @highlightedElem.onkeyup
                        @highlightedElem.onkeyup = =>
                            if @highlightedElem.value is @target.value
                                @highlightedElem.onkeyup = oldHandlers
                                @nextStep()
                    else if @target.action.includes ':'
                        @buttonDoIt.show()
                        handler = {}
                        handler[@target.action] = =>
                            @disposable.dispose()
                            @nextStep()
                        @disposable = atom.commands.add @highlightedElem, handler
                    else if @highlightedElem?
                        @buttonDoIt.show()
                        oldHandlers = @highlightedElem[@target.action]
                        @highlightedElem[@target.action] = =>
                            @highlightedElem[@target.action] = oldHandlers
                            @nextStep()

        doIt: =>
            if @highlightedElem?
                if @target.action is 'value'
                    @highlightedElem.value = @target.value
                else if @target.action.includes ':'
                    view = atom.views.getView @highlightedElem
                    atom.commands.dispatch view, @target.action
                else if @highlightedElem?
                    if @target.action.startsWith 'on'
                        action = @target.action.slice 2
                    else
                        action = @target.action
                    event = new Event action,
                                        view: window
                                        bubbles: true
                                        cancelable: true
                    @highlightedElem.dispatchEvent(event)

        displayStep: (retry = false) =>
            @setHighlightedElem()

            msgBoxWidth = 292
            msgBoxHeight = 50
            msgBoxOffset = 10
            windowRect = document.body.getBoundingClientRect()
            msgBoxLeft = (windowRect.width - msgBoxWidth)/2
            msgBoxTop  = (windowRect.height - msgBoxHeight)/2

            if @target.action is 'proceed'
                @buttonContinue.show()
            else if not @highlightedElem?
                @buttonDoIt.hide()
                unless retry
                    @guideTitle[0].innerText = @currentStep.title
                    @guideDescription[0].innerText = 'Please wait...'
                    @messageBox[0].style.width = msgBoxWidth + 'px'
                    @messageBox[0].style.height = msgBoxHeight + 'px'
                    @messageBox[0].style.top = msgBoxTop + 'px'
                    @messageBox[0].style.left = msgBoxLeft + 'px'

                setTimeout (=> @displayStep(true)), 300
                return

            @installHandlers()

            if @highlightedElem?
                highlightedRect = @highlightedElem.getBoundingClientRect()
                if highlightedRect.width != 0 and highlightedRect.height != 0
                    if highlightedRect.left > msgBoxWidth + msgBoxOffset
                        msgBoxLeft = highlightedRect.left - msgBoxWidth - msgBoxOffset
                        msgBoxTop = highlightedRect.top + highlightedRect.height/2 - msgBoxHeight/2
                    else if highlightedRect.right + msgBoxWidth + msgBoxOffset < windowRect.width
                        msgBoxLeft = highlightedRect.right + msgBoxOffset
                        msgBoxTop = highlightedRect.top + highlightedRect.height/2 - msgBoxHeight/2
                    else if highlightedRect.top > msgBoxHeight + msgBoxOffset
                        msgBoxTop = highlightedRect.top - msgBoxHeight - msgBoxOffset
                    else if highlightedRect.bottom + msgBoxHeight + msgBoxOffset < windowRect.height
                        msgBoxTop = highlightedRect.bottom + msgBoxOffset

            @guideTitle[0].innerText = @currentStep.title
            @guideDescription[0].innerText = @currentStep.description
            @messageBox[0].style.width = msgBoxWidth + 'px'
            @messageBox[0].style.height = msgBoxHeight + 'px'
            @messageBox[0].style.top = msgBoxTop + 'px'
            @messageBox[0].style.left = msgBoxLeft + 'px'

            if @highlightedElem?
                @retryWhenHighlightedElementIsGone()

        retryWhenHighlightedElementIsGone: =>
            targetedElem = @findTargetedElem()
            if targetedElem? and targetedElem.classList.contains highlightClass
                setTimeout @retryWhenHighlightedElementIsGone, 100
            else
                @displayStep(retry: true)


        updatePointer: =>
            if @highlightedElem?
                if (@highlightedElem.classList.contains 'luna-studio-window') or (@highlightedElem.classList.contains 'luna-studio-mount')
                    @pointer[0].classList.add pointerWindowClass
                else
                    @pointer[0].classList.remove pointerWindowClass
                highlightedRect = @highlightedElem.getBoundingClientRect()
                unless highlightedRect.width is 0 or highlightedRect.height is 0
                    @pointer.show()
                    @pointer[0].style.width  = highlightedRect.width + 'px'
                    @pointer[0].style.height = highlightedRect.height + 'px'
                    @pointer[0].style.top  = highlightedRect.top + 'px'
                    @pointer[0].style.left = highlightedRect.left + 'px'
                    window.requestAnimationFrame @updatePointer
                else
                    @pointer.hide()
            else
                @pointer.hide()

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
