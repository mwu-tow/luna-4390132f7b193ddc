fs   = require 'fs'
yaml = require 'js-yaml'
recentProjectsPath = if process.env.LUNA_STUDIO_CONFIG? then process.env.LUNA_STUDIO_CONFIG + '/recent-projects.yml' else './recent-projects.yml'
encoding = 'utf8'

loadNoCheck = (fun) =>
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


    load: (fun) =>
        loadNoCheck (recentProjectsPaths) ->
            recentProjectsPaths.forEach (recentProjectPath) =>
                console.log recentProjectPath
                fs.access recentProjectPath, (err) =>
                    if not err
                        console.log recentProjectPath
                        fun recentProjectPath


            console.log recentProjectsPaths


    add: (recentProjectPath) =>
        loadNoCheck (recentProjectsPaths) ->
            pos = recentProjectsPaths.indexOf(recentProjectPath);
            recentProjectsPaths.splice(pos, 1)
            recentProjectsPaths.unshift(recentProjectPath)
            console.log recentProjectsPaths
            data = yaml.safeDump(recentProjectsPaths)
            console.log data
            fs.writeFile(recentProjectsPath, data, encoding, console.log);
