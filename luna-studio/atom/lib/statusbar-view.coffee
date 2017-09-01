{View} = require 'atom-space-pen-views'

module.exports =
class Statusbar extends View
    constructor: (@codeEditor) ->
        super

    @content: ->
        @div class: 'luna-statusbar inline-block', =>
            @div class: 'btn-group inline-block luna-interpreter-buttons', =>
                @div
                    class: 'btn icon icon-playback-play luna-interpreter-startstop'
                    outlet: 'startStopButton'
                @div
                    class: 'btn icon icon-sync luna-interpreter-reload'
                    outlet: 'buttonSync'
                @div
                    class: 'luna-interpreter-status inline-block'
                    outlet: status
                    'Interpreter status'

    initialize: =>

    getTitle: -> 'Status bar'
