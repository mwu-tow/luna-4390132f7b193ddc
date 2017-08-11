"use strict";

var removeFromArray = function (array, elt) {
    var index = array.indexOf(elt);
    array.splice(index, 1);
};

module.exports = function () {

    var listeners = {
        bufferListener: [],
        codeListener: [],
        eventListenerInternal: [],
        diffListener: [],
        statusListener: [],
        lexer: null,
    };

    return {
        codeListener: function (listener) {
            listeners.codeListener.push(listener);
        },
        pushCode: function(uri_send, start_send, end_send, text) {
            listeners.codeListener.forEach(function(listener) {
                listener(uri_send, start_send, end_send, text);
            });
        },

        bufferListener: function (listener) {
            listeners.bufferListener.push(listener);
        },
        pushBuffer: function(data1, data2) {
            listeners.bufferListener.forEach(function(listener) {
                listener(data1, data2);
            });
        },

        statusListener: function (listener) {
            listeners.statusListener.push(listener);
        },
        pushStatus: function(data1, data2, data3) {
            listeners.statusListener.forEach(function(listener) {
                listener(data1, data2, data3);
            });
        },

        subscribeEventListenerInternal: function(listener) {
            listeners.eventListenerInternal.push(listener);
        },
        unsubscribeEventListenerInternal: function(listener) {
            removeFromArray(listeners.eventListenerInternal, listener);
        },
        pushInternalEvent: function(data) {
            listeners.eventListenerInternal.forEach(function(listener) {
                listener(data);
            });
        },

        subscribeDiff: function(listener) {
            listeners.diffListener.push(listener);
        },
        unsubscribeDiff: function(listener) {
            removeFromArray(listeners.diffListener, listener);
        },
        pushDiff: function(data) {
            listeners.diffListener.forEach(function(listener) {
                listener(data);
            });
        },
        setLexer: function(fun) {
            listeners.lexer = fun;
        },
        unsetLexer: function() {
            listeners.lexer = null;
        },
        lex: function(stack, data) {
            return listeners.lexer(stack, data);
        }
    };
};
