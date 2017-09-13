collectFPS = require 'collect-fps'
analytics = require './gen/analytics'

module.exports =
    collect: =>
        endFps = collectFPS()
        discardInit = true
        first = true

        gatherFps = =>
            fps = endFps()
            if discardInit
                discardInit = false
            else if first
                analytics.track("Performance.FPS.First", fps)
                first = false
            else
                analytics.track("Performance.FPS", fps)

            endFps = collectFPS()
            setTimeout(gatherFps, 30000)

        gatherFps()
