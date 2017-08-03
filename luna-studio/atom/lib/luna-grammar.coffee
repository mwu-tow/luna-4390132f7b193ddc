{Grammar} = require "first-mate"
{lunaClass} = require "./gen/lexer-classes"

module.exports =
class LunaSemanticGrammar extends Grammar
    constructor: (registry, lex) ->
        super(registry, { name: "Luna"
                        , fileTypes: ["luna"]
                        , scopeName: "source.luna"
                        })
        @lex = lex

    tokenizeLine: (line, ruleStack, firstLine = false) =>
        ruleStack = 0 unless ruleStack?
        lexerLine = @lex(line)
        buffer = line
        tags = []
        tokens = []
        addToken = (text, lexerTags) =>
            scopes = @scopeName
            for lexerTag in lexerTags
                cls = lunaClass(lexerTag)
                if cls?
                    scopes += "." + cls
            tags.push @registry.startIdForScope(scopes)
            tags.push text.length
            tags.push @registry.endIdForScope(scopes)
            tokens.push { value: text, scopes: [scopes] }

        while buffer.length != 0
            if lexerLine.length > 0
                tokenInfo = lexerLine.shift()
                token = buffer.substr(0, tokenInfo.length)
                buffer = buffer.substr(tokenInfo.length, buffer.length)
                addToken(token, tokenInfo.tags)
            else
                addToken(buffer, [])
                buffer = ""

        return { line: line, tags: tags, tokens: tokens, ruleStack: ruleStack + 1 }
