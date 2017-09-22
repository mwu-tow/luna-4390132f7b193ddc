$$              = require('./common')
config          = require('./config')
atomCallbackTextEditor    = require('./atom-callback-text-editor')



start = ->
  $(document).ready ->
    if window.already_initialized
      console.error 'app already started'
    else
      window.already_initialized = true
      require('env')().start()

module.exports =
  start: start

window.processedEvents = []
