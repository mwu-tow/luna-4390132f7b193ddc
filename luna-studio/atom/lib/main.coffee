LunaEditorTab = require './luna-editor-tab'
LunaStudioTab = require './luna-studio-tab'
SubAtom       = require 'sub-atom'

(require './luna-visualizers')()
codeEditor = (require './gen/text-editor-ghcjs.js')()
nodeEditor = (require './gen/node-editor-ghcjs.js')()
path = require 'path'
LunaSemanticGrammar = require './luna-grammar'

lunaStudioFakePath = 'luna-studio'

module.exports = LunaStudio =

    deserializeLunaEditorTab: ({uri}) ->
        actStatus = (status) ->
            if status == 'Init'
                atom.workspace.open(uri, {split: "left"})

        codeEditor.statusListener actStatus

    activate: (state) ->
        atom.grammars.addGrammar(new LunaSemanticGrammar(atom.grammars, codeEditor.lex))
        codeEditor.connect(nodeEditor.connector)
        codeEditor.start()
        actStatus = (act, uri, status) ->
            if act == 'Init'
                rootPath = atom.project.getPaths().shift()
                if rootPath? and rootPath != ""
                    codeEditor.pushInternalEvent(tag: "SetProject", _path: rootPath)
                    atom.workspace.open(lunaStudioFakePath, {split: "right"})
            if act == 'FileOpened'
                codeEditor.pushInternalEvent(tag: "GetBuffer", _path: uri)
        codeEditor.statusListener actStatus

        atom.workspace.addOpener (uri) => @lunaOpener(uri)

        @subscribe = new SubAtom()
        @subscribe.add atom.workspace.onDidChangeActivePaneItem (item) => @handleItemChange(item)
        @subscribe.add atom.workspace.onDidDestroyPaneItem (event) => @handleItemDestroy(event)
        @subscribe.add atom.workspace.observeTextEditors (editor) => @handleSaveAsLuna(editor)
        @subscribe.add atom.workspace.onDidAddPaneItem (pane)   => @handleItemChange(pane.item)

    lunaOpener: (uri) ->
        if path.basename(uri) is lunaStudioFakePath
              new LunaStudioTab(null, nodeEditor, codeEditor)
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
