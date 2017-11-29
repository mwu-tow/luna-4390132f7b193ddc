analytics = require './gen/analytics'
fs = require 'fs'
yaml = require 'js-yaml'

timeStart = new Date()
runtimeReport = {}
dataPath = if process.env.LUNA_STUDIO_DATA_PATH? then process.env.LUNA_STUDIO_DATA_PATH + '/analytics-data.yml' else './analytics-data.yml'

encoding = 'utf8'

frames = []
gatherActive = false
analyseActive = false

gather = =>
    frames.push new Date()
    if gatherActive
        requestAnimationFrame gather

analyse = (callback) =>
    snapshot = frames
    frames = []

    if snapshot.length < 3
        return

    prev = undefined
    min = undefined
    max = undefined
    sum = 0

    for curr in snapshot
        if prev?
            delta = curr - prev
            sum += delta
            if not min? or delta < min
                min = delta
            if not max? or delta > max
                max = delta
        prev = curr

    avg = sum/(snapshot.length - 1)
    avgFps = 1000 / avg
    minFps = 1000 / max
    maxFps = 1000 / min

    callback
        min: minFps
        max: maxFps
        avg: avgFps

startGather = =>
    gatherActive = true
    requestAnimationFrame gather

stopGather = =>
    gatherActive = false

startAnalyse = (interval, callback) =>
    analyseActive = true
    unless gatherActive
        startGather()
    run = =>
        analyse callback
        if analyseActive
            setTimeout run, interval
    run()

stopAnalyse = =>
    analyseActive = false
    stopGather()

isActive = false

document.body.onmouseover = => isActive = true
document.body.onscroll    = => isActive = true
document.body.onkeydown   = => isActive = true

whenActive = (callback) =>
    if isActive
        callback()
        isActive = false

module.exports =
    collect: =>
        discardInit = true
        first = true

        timeLoaded = new Date()
        runtimeReport.loadingTime = (timeLoaded - timeStart)/1000.0
        timeStart = timeLoaded

        startAnalyse 60000, (fps) =>
            if discardInit
                discardInit = false
            else if first
                analytics.track('Performance.FPS.First', fps)
                first = false
            else
                whenActive => analytics.track('Performance.FPS', fps)
                runtimeReport.fps = fps

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
