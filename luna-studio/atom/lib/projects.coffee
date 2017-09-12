fs      = require 'fs'
Git     = require 'nodegit'
request = require 'request'
yaml    = require 'js-yaml'

recentProjectsPath = if process.env.LUNA_STUDIO_CONFIG? then process.env.LUNA_STUDIO_CONFIG + '/recent-projects.yml' else './recent-projects.yml'
tutorialsPath   = process.env.LUNA_STUDIO_CONFIG + '/tutorials.yml'
tutorialsDownloadPath = if process.env.LUNA_STUDIO_TUTORIALS? then  process.env.LUNA_STUDIO_TUTORIALS else '/tmp'
encoding = 'utf8'

tutorialRequestOpts =
    url: 'https://api.github.com/users/luna-packages/repos'
    headers:
        'User-Agent': 'luna-studio'

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
            try
                request.get tutorialRequestOpts, (err, response, body) =>
                    parsed = yaml.safeLoad(body)
                    repos = []
                    if parsed?
                        for repo in parsed
                            repos.push
                                name: repo.name
                                description: repo.description
                                uri: repo.html_url
                                thumb: ('https://raw.githubusercontent.com/luna-packages/' + repo.name + '/master/thumb.png')
                    fun repos
            catch error
                atom.confirm
                    message: "Error while getting tutorials"
                    detailedMessage: error.message
                    buttons:
                        Ok: ->

        open: (tutorial) ->
            dstPath = tutorialsDownloadPath + '/' + tutorial.name
            cloneOpts =
                fetchOpts:
                    callbacks:
                        certificateCheck: => 1
                        credentials: (url, userName, bla) =>
                            Git.Cred.sshKeyFromAgent(userName)
            fs.access dstPath, (err) =>
                if err
                    Git.Clone(tutorial.uri, dstPath, cloneOpts)
                        .then((repo) => atom.project.setPaths [dstPath])
                        .catch((error) => atom.confirm
                            message: "Error while cloning tutorial"
                            detailedMessage: error.message
                            buttons:
                                Ok: -> )
                else
                    atom.project.setPaths [dstPath]
