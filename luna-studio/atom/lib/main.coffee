path     = require 'path'
SubAtom  = require 'sub-atom'

LunaEditorTab  = require './luna-editor-tab'
LunaStudioTab  = require './luna-studio-tab'
LunaWelcomeTab = require './luna-welcome-tab'
LunaSemanticGrammar = require './luna-grammar'

(require './luna-visualizers')()
codeEditor = (require './gen/text-editor-ghcjs.js')()
nodeEditor = (require './gen/node-editor-ghcjs.js')()

LUNA_STUDIO_URI  = 'atom://luna/studio'
LUNA_WELCOME_URI = 'atom://luna/welcome'

module.exports = LunaStudio =

    config:
        showWelcomeScreen:
            title: 'Welcome screen'
            description: 'Show welcome screen when on start up.'
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


    activate: (state) ->
        atom.grammars.addGrammar(new LunaSemanticGrammar(atom.grammars, codeEditor.lex))
        atom.workspace.addOpener (uri) => @lunaOpener(uri)
        codeEditor.connect(nodeEditor.connector)
        codeEditor.start()
        if atom.config.get('luna-studio.showWelcomeScreen')
            atom.workspace.open(LUNA_WELCOME_URI, {split: "left"})
        actStatus = (act, uri, status) ->
            if act == 'Init'
                rootPath = atom.project.getPaths().shift()
                if rootPath? and rootPath != ""
                    codeEditor.pushInternalEvent(tag: "SetProject", _path: rootPath)
                atom.workspace.open(LUNA_STUDIO_URI, {split: atom.config.get('luna-studio.preferredNodeEditorPosition')})
            if act == 'FileOpened'
                codeEditor.pushInternalEvent(tag: "GetBuffer", _path: uri)
        codeEditor.statusListener actStatus
        @subscribe = new SubAtom()
        @subscribe.add atom.workspace.onDidChangeActivePaneItem (item) => @handleItemChange(item)
        @subscribe.add atom.workspace.onDidDestroyPaneItem (event) => @handleItemDestroy(event)
        @subscribe.add atom.workspace.observeTextEditors (editor) => @handleSaveAsLuna(editor)
        @subscribe.add atom.workspace.onDidAddPaneItem (pane)   => @handleItemChange(pane.item)

    deserializeLunaEditorTab: ({uri}) ->
        actStatus = (status) ->
            if status == 'Init'
                atom.workspace.open(uri, {split: atom.config.get('luna-studio.preferredCodeEditorPosition')})

        codeEditor.statusListener actStatus

    lunaOpener: (uri) ->
        if uri is LUNA_STUDIO_URI
              new LunaStudioTab(null, nodeEditor, codeEditor)
        else if uri is LUNA_WELCOME_URI
              new LunaWelcomeTab(codeEditor)
        else if path.extname(uri) is '.luna'
              new LunaEditorTab(uri, codeEditor)

    deactivate: ->
        @subscribe.dispose()

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
