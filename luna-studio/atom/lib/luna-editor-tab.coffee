{TextEditor, TextBuffer} = require 'atom'
{TextEditorView, View} = require 'atom-space-pen-views'
{CompositeDisposable} = require 'event-kit'
path = require 'path'
SubAtom = require 'sub-atom'

TextBuffer::subscribeToFileOverride = (codeEditor) ->
    @fileSubscriptions?.dispose()
    @fileSubscriptions = new CompositeDisposable

    @fileSubscriptions.add @file.onDidChange =>
        @conflict = true if @isModified()
        console.log "FileChanged"
        codeEditor.pushInternalEvent(tag: "FileChanged", _path: @getPath())
      #   previousContents = @cachedDiskContents
      #
      #   # Synchrounously update the disk contents because the {File} has already cached them. If the
      #   # contents updated asynchrounously multiple `conlict` events could trigger for the same disk
      #   # contents.
      #   @updateCachedDiskContentsSync()
      #   return if previousContents == @cachedDiskContents
      #
      #   if @conflict
      #     @emitter.emit 'did-conflict'
      #   else
      #     @reload()

    @fileSubscriptions.add @file.onDidDelete =>
        modified = @getText() != @cachedDiskContents
        @wasModifiedBeforeRemove = modified
        @emitter.emit 'did-delete'
        if modified
            @updateCachedDiskContents()
        else
            @destroy()

    @fileSubscriptions.add @file.onDidRename =>
        @emitter.emit 'did-change-path', @getPath()

    @fileSubscriptions.add @file.onWillThrowWatchError (errorObject) =>
        @emitter.emit 'will-throw-watch-error', errorObject

subscribe = null

module.exports =
  class LunaEditorTab extends TextEditor

    constructor: (@uri, @codeEditor) ->
        super
        @diffToOmit = new Set()
        @getBuffer().setPath(@uri)
        @getBuffer().subscribeToFileOverride(@codeEditor)
        @codeEditor.pushInternalEvent(tag: "OpenFile", _path: @uri)

        @codeEditor.onSetBuffer @setBuffer
        @codeEditor.onSetClipboard @setClipboard
        @codeEditor.onInsertCode @insertCode

        @handleEvents()

        @subscribe = new SubAtom
        @subscribe.add @getBuffer().onDidStopChanging (event) =>
            for change in event.changes
                if @diffToOmit.has(change.newText)
                    @diffToOmit.delete(change.newText)
                else
                    diff =
                        # uri: @uri
                        start: change.start
                        end:   change.start.translate(change.oldExtent)
                        text:  change.newText
                        cursor: @.getCursorBufferPosition()
                      #   cursor: (@getBuffer().characterIndexForPosition(x) for x in @.getCursorBufferPositions()) #for multiple cursors
                    @codeEditor.pushDiff(diff)

    serialize: -> { deserializer: 'LunaEditorTab', uri: @uri }

    getTitle: -> path.basename(@uri)

    deactivate: -> @subscribe.dispose()

    handleEvents: =>
        atom.commands.add @element,
            'core:copy':  (e) => @handleCopy(e)
            'core:cut':   (e) => @handleCopy(e)
            'core:paste': (e) => @handlePaste(e)
            'core:save':  (e) => @handleSave(e)

    spans: =>
        buffer = @getBuffer()
        [buffer.characterIndexForPosition(s.marker.oldHeadBufferPosition),
         buffer.characterIndexForPosition(s.marker.oldTailBufferPosition)].sort() for s in @getSelections()

    handleCopy: (e) =>
        e.preventDefault()
        e.stopImmediatePropagation()
        @codeEditor.pushInternalEvent(tag: "Copy", _path: @uri, _selections: @spans())

    handlePaste: (e) =>
        cbd = atom.clipboard.readWithMetadata()
        cbdData = []
        if cbd.metadata.selections?
            for x in cbd.metadata.selections
                cbdData.push(x.text)
        else
            cbdData[0] = cbd.text
        e.preventDefault()
        e.stopImmediatePropagation()
        @codeEditor.pushInternalEvent(tag: "Paste", _path: @uri, _selections: @spans(), _content: cbdData)

    handleSave: (e) =>
        e.preventDefault()
        e.stopImmediatePropagation()
        @codeEditor.pushInternalEvent(tag: "SaveFile", _path: atom.workspace.getActivePaneItem().uri)

    insertCode: (uri_send, start_send, end_send, text) =>
        if @uri == uri_send
            @omitDiff(text)
            @getBuffer().setText(text)

    setClipboard: (uri_send, text) =>
        if @uri == uri_send
            atom.clipboard.write(text)

    setBuffer: (uri_send, text) =>
        console.log(uri_send, @uri)
        if @uri == uri_send
            @omitDiff(text)
            @getBuffer().setText(text)
            console.log "setBuffer"

    omitDiff: (text) =>
        @diffToOmit.add(text)
