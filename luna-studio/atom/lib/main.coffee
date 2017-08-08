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

    deserializeLunaEditorTab: ({uri}) ->
        actStatus = (status) ->
            if status == 'Init'
                atom.workspace.open(uri, {split: "left"})

        codeEditor.statusListener actStatus

    activate: (state) ->
        atom.grammars.addGrammar(new LunaSemanticGrammar(atom.grammars, codeEditor.lex))
        atom.workspace.addOpener (uri) => @lunaOpener(uri)
        codeEditor.connect(nodeEditor.connector)
        codeEditor.start()
        atom.workspace.open(LUNA_WELCOME_URI, {split: "left"})
        actStatus = (act, uri, status) ->
            if act == 'Init'
                rootPath = atom.project.getPaths().shift()
                if rootPath? and rootPath != ""
                    codeEditor.pushInternalEvent(tag: "SetProject", _path: rootPath)
                atom.workspace.open(LUNA_STUDIO_URI, {split: "right"})
            if act == 'FileOpened'
                codeEditor.pushInternalEvent(tag: "GetBuffer", _path: uri)
        codeEditor.statusListener actStatus
        @subscribe = new SubAtom()
        @subscribe.add atom.workspace.onDidChangeActivePaneItem (item) => @handleItemChange(item)
        @subscribe.add atom.workspace.onDidDestroyPaneItem (event) => @handleItemDestroy(event)
        @subscribe.add atom.workspace.observeTextEditors (editor) => @handleSaveAsLuna(editor)
        @subscribe.add atom.workspace.onDidAddPaneItem (pane)   => @handleItemChange(pane.item)

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
