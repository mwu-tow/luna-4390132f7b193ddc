fs   = require 'fs'
yaml = require 'js-yaml'

recentProjectsPath = if process.env.LUNA_STUDIO_CONFIG? then process.env.LUNA_STUDIO_CONFIG + '/recent-projects.yml' else './recent-projects.yml'
tutorialsPath   = process.env.LUNA_STUDIO_CONFIG + '/tutorials.yml'
encoding = 'utf8'

loadRecentNoCheck = (fun) =>
    fs.readFile recentProjectsPath, encoding, (err, data) =>
        recentProjectsPaths = []
        if err
            console.log err
        else
            parsed = yaml.safeLoad(data)
            if parsed?
                recentProjectsPaths = parsed
        fun recentProjectsPaths

module.exports =
    recent:
        load: (fun) =>
            loadRecentNoCheck (recentProjectsPaths) =>
                recentProjectsPaths.forEach (recentProjectPath) =>
                    fs.access recentProjectPath, (err) =>
                        if not err
                            fun recentProjectPath

        add: (recentProjectPath) =>
            loadRecentNoCheck (recentProjectsPaths) =>
                pos = recentProjectsPaths.indexOf(recentProjectPath);
                if pos != -1
                    recentProjectsPaths.splice(pos, 1)
                recentProjectsPaths.unshift(recentProjectPath)
                data = yaml.safeDump(recentProjectsPaths)
                fs.writeFile recentProjectsPath, data, encoding, (err) =>
                    if err?
                        console.log err
    tutorial:
        list: (fun) =>
            fs.readFile tutorialsPath, (err, data) =>
                tutorials = []
                if err
                    console.log err
                else
                    parsed = yaml.safeLoad(data)
                    if parsed?
                        tutorials = parsed
                fun tutorials

        open: (tutorial) ->
            console.log ("Opening " + tutorial)
