analytics = require './gen/analytics'
collectFPS = require 'collect-fps'
fs = require 'fs'
yaml = require 'js-yaml'

timeStart = new Date()
runtimeReport = {}
dataPath = if process.env.LUNA_STUDIO_DATA_PATH? then process.env.LUNA_STUDIO_DATA_PATH + '/analytics-data.yml' else './analytics-data.yml'

encoding = 'utf8'

module.exports =
    collect: =>
        endFps = collectFPS()
        discardInit = true
        first = true

        timeLoaded = new Date()
        runtimeReport.loadingTime = (timeLoaded - timeStart)/1000.0
        timeStart = timeLoaded

        gatherFps = =>
            fps = endFps()
            if discardInit
                discardInit = false
            else if first
                analytics.track('Performance.FPS.First', fps)
                first = false
            else
                analytics.track('Performance.FPS', fps)
                runtimeReport.fps = fps

            endFps = collectFPS()
            setTimeout(gatherFps, 30000)

        gatherFps()
        fs.readFile dataPath, encoding, (err, data) =>
            if err
                analytics.track 'Stats.FirstRun'
            else
                parsed = yaml.safeLoad(data)
                if parsed?
                    analytics.track 'Stats.Runtime', parsed, =>
                        fs.writeFileSync dataPath, "", {encoding:encoding}



    finalize: =>
        timeEnd = new Date()
        runtimeReport.totalTime = (timeEnd - timeStart)/1000.0
        data = yaml.safeDump(runtimeReport)
        fs.writeFileSync dataPath, data, {encoding:encoding}
