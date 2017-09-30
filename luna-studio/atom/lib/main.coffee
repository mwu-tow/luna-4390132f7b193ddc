path     = require 'path'
request  = require 'request'
yaml     = require 'js-yaml'

stats = require './stats'
analytics = require './gen/analytics'
LunaEditorTab  = require './luna-editor-tab'
LunaStudioTab  = require './luna-studio-tab'
LunaWelcomeTab = require './luna-welcome-tab'
LunaSemanticGrammar = require './luna-grammar'
projects  = require './projects'
Statusbar = require './statusbar-view'
(require './luna-visualizers')()
codeEditor = (require './gen/text-editor-ghcjs.js')()
nodeEditor = (require './gen/node-editor-ghcjs.js')()


LUNA_STUDIO_URI  = 'atom://luna/studio'

analyticsConfigRequest =
    url: 'https://raw.githubusercontent.com/luna/luna-studio-config/master/analytics.yml'
    headers:
        'User-Agent': 'luna-studio'

module.exports = LunaStudio =
    activate: (state) ->
        @loadAnalyticsConfig()
        atom.grammars.addGrammar(new LunaSemanticGrammar(atom.grammars, codeEditor.lex))
        atom.workspace.addOpener (uri) => @lunaOpener(uri)
        codeEditor.connect(nodeEditor.connector)
        @welcome = new LunaWelcomeTab(codeEditor)

        actStatus = (act, uri, status) ->
            if act == 'Init'
                rootPath = atom.project.getPaths().shift()
                if rootPath? and rootPath != ""
                    projects.recent.add rootPath
                    codeEditor.pushInternalEvent(tag: "SetProject", _path: rootPath)
                atom.workspace.open(LUNA_STUDIO_URI, {split: atom.config.get('luna-studio.preferredNodeEditorPosition')})
            if act == 'FileOpened'
                codeEditor.pushInternalEvent(tag: "GetBuffer", _path: uri)
            if act == 'ProjectMove'
                for pane in atom.workspace.getPaneItems()
                    if pane instanceof LunaStudioTab
                        panePath = pane.uri
                        if panePath.startsWith(projects.temporaryProject.path)
                            pane.uri = uri + panePath.slice(projects.temporaryProject.path.length)
                            nodeEditor.pushEvent(tag: "SetFile", path: pane.uri)
                    else if pane instanceof LunaEditorTab
                        panePath = pane.uri
                        if panePath.startsWith(projects.temporaryProject.path)
                            pane.setUri(uri + panePath.slice(projects.temporaryProject.path.length))
                atom.project.setPaths [uri]

        codeEditor.statusListener actStatus
        atom.workspace.onDidChangeActivePaneItem (item) => @handleItemChange(item)
        atom.workspace.onDidDestroyPaneItem (event) => @handleItemDestroy(event)
        atom.workspace.observeTextEditors (editor) => @handleSaveAsLuna(editor)
        atom.workspace.onDidAddPaneItem (pane)   => @handleItemChange(pane.item)
        atom.project.onDidChangePaths (projectPaths) => @handleProjectPathsChange(projectPaths)
        atom.packages.onDidActivateInitialPackages =>
            if atom.config.get('luna-studio.showWelcomeScreen') and atom.project.getPaths().length == 0
                @welcome.attach()
            if atom.config.get('luna-studio.resetProjects') and atom.project.getPaths().length == 0
                projects.temporaryProject.open (err) =>
                    if err then throw err
        atom.commands.add 'body',
            'luna-studio:welcome': => @welcome.attach()
            'core:cancel': => @welcome.detach()
        codeEditor.start()

    loadAnalyticsConfig: ->
        try
            request.get analyticsConfigRequest, (err, response, body) =>
                filters = yaml.safeLoad(body)
                analytics.setFilters filters
                stats.collect()
        catch error
            console.error error

    consumeStatusBar: (statusBar) ->
        myElement = new Statusbar(codeEditor)
        @statusBarTile = statusBar.addLeftTile(item: myElement, priority: -1)

    deserializeLunaEditorTab: ({uri}) ->
        actStatus = (status) ->
            if status == 'Init'
                atom.workspace.open(uri, {split: atom.config.get('luna-studio.preferredCodeEditorPosition')})

        codeEditor.statusListener actStatus

    lunaOpener: (uri) ->
        if uri is LUNA_STUDIO_URI
              new LunaStudioTab(null, nodeEditor, codeEditor)
        else if path.extname(uri) is '.luna'
              new LunaEditorTab(uri, codeEditor)

    deactivate: ->
        stats.finalize()
        @statusBarTile?.destroy()
        @statusBarTile = null

    handleItemChange: (item) =>
        if item instanceof LunaEditorTab
            for i in atom.workspace.getPaneItems()
                i.uri = item.uri if i instanceof LunaStudioTab
            nodeEditor.pushEvent(tag: "SetFile", path: item.uri)

    handleItemDestroy: (event) =>
        if (event.item instanceof LunaEditorTab)
            urisOf = (instance) ->
                pane.uri for pane in atom.workspace.getPaneItems().filter((a) -> a instanceof instance)
            codeUris  = urisOf LunaEditorTab
            graphUris = urisOf LunaStudioTab
            if event.item.uri not in codeUris #last opened file
                if event.item.uri in graphUris
                    nodeEditor.pushEvent(tag: "UnsetFile")
                return codeEditor.pushInternalEvent(tag: "CloseFile", _path: event.item.uri)

    handleSaveAsLuna: (editor) ->
        editor.onDidSave (e) =>
            if path.extname(e.path) is ".luna"
                atom.workspace.destroyActivePaneItem()
                atom.workspace.open(e.path)

    handleProjectPathsChange: (projectPaths) ->
        projectPath = projectPaths[0]
        if projectPath?
            projects.recent.add projectPath
            codeEditor.pushInternalEvent(tag: "SetProject", _path: projectPath)

    config:
        showWelcomeScreen:
            title: 'Welcome screen'
            description: 'Show welcome screen on start up'
            type: 'boolean'
            default: true
        preferredNodeEditorPosition:
            title: 'Preferred pane for node editor'
            type: 'string'
            default: 'right'
            enum: [
                { value: 'left' , description: 'Left pane' }
                { value: 'right', description: 'Right pane' }
                { value: 'up'   , description: 'Upper pane' }
                { value: 'down' , description: 'Lower pane' }
            ]
        preferredCodeEditorPosition:
            title: 'Preferred pane for code editor'
            type: 'string'
            default: 'left'
            enum: [
                { value: 'left' , description: 'Left pane' }
                { value: 'right', description: 'Right pane' }
                { value: 'up'   , description: 'Upper pane' }
                { value: 'down' , description: 'Lower pane' }
            ]
        typecolors_l:
            title: 'Set L for LCH type colouring'
            type: 'number'
            default: 30

        typecolors_c:
            title: 'Set C for LCH type colouring'
            type: 'number'
            default: 45

        typecolors_h:
            title: 'Set initial H for LCH type colouring'
            type: 'number'
            default: 100.7

        analyticsEnabled:
            title: 'Send anonymous data to improve the application'
            type: 'boolean'
            default: true

        resetProjects:
            title: 'Open empty project on start up'
            type: 'boolean'
            default: true
