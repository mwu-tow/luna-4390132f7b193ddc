"use strict";

removeFromArray = (array, elt) =>
    index = array.indexOf elt
    array.splice index, 1

listeners =
    onEvent: []

globalRegistry = null
eventFilters = { blockedEvents: []
               , allowedEvents: []
               };

module.exports =
    connector: (otherGlobal) => globalRegistry = otherGlobal
    setActiveLocation: (location) =>  globalRegistry.activeLocation = location
    pushNotification: (lvl, msg) =>
        unless atom?
            switch lvl
                when 0, 1
                    console.error msg
                    break
                when 2
                    console.warn msg
                    break
                else
                    console.log msg
        else
            switch lvl
                when 0
                    notification = atom.notifications.addFatalError "Fatal Error",
                        dismissable: true
                        description: msg
                        buttons: [
                            text: 'Copy to clipboard'
                            onDidClick: =>
                                atom.clipboard.write msg
                                return notification.dismiss()
                        ]
                when 1
                    notification = atom.notifications.addError "Error",
                        dismissable: true
                        description: msg
                        buttons: [
                            text: 'Copy to clipboard'
                            onDidClick: =>
                                atom.clipboard.write msg
                                return notification.dismiss()
                        ]
                else
                    notification = atom.notifications.addWarning "Warning",
                        dismissable: true
                        description: msg
                        buttons: [
                            text: 'Copy to clipboard'
                            onDidClick: =>
                                atom.clipboard.write msg
                                return notification.dismiss()
                        ]
    onEvent: (listener) => listeners.onEvent.push listener
    unOnEvent: (listener) => removeFromArray listeners.onEvent, listener
    pushEvent: (data) =>
        listeners.onEvent.forEach (listener) => listener(data)
    setEventFilter: (blocked, allowed) => 
        eventFilters = { blockedEvents: blocked, allowedEvents: allowed }
    acceptEvent: (event) =>
        event    = JSON.parse(event);
        nodeInfo = event.eventInfo.nodeInfo
        nodeName = if nodeInfo then nodeInfo.nodeName else null
        portId   = if nodeInfo && nodeInfo.portInfo then nodeInfo.portInfo.portId else null
        if eventFilters.blockedEvents.length == 0 && eventFilters.allowedEvents.length == 0
            return true
        for p in eventFilters.allowedEvents
            if (p.regexp.test event.name) and ((not p.nodeName) or (p.nodeName == nodeName)) and ((not p.portId) or (p.portId == portId))
                return true
        if eventFilters.blockedEvents.length == 0
            return false
        for p in eventFilters.blockedEvents
            if (p.regexp.test event.name) and ((not p.nodeName) or (p.nodeName == nodeName)) and ((not p.portId) or (p.portId == portId))
                return false
        return true