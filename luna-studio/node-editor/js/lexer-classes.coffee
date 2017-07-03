# https://github.com/atom/template-syntax/blob/master/stylesheets/base.less
lunaClasses = {

        # Layout      :
        # BOF         :
        # EOF         :
        # EOL         :
        # Terminator  :
        # BlockStart  :
        # Block       :
        # Group       :
        Marker      : 'marker'

        # Ident       :
        Var         : 'variable'
        Cons        : 'constant'
        Wildcard    : 'variable'

        Keyword     : 'keyword'
        KwAll       : 'keyword.control'
        KwCase      : 'keyword.control'
        KwClass     : 'meta.class'
        KwDef       : 'meta.class'
        KwImport    : 'meta.import'
        KwOf        : 'keyword.control'

        Operator    : 'keyword.operator'
        Modifier    : 'keyword.other.special-method'
        Accessor    : 'keyword.other.special-method'
        # Assignment  :
        # TypeApp     :
        # Merge       :
        # Range       :
        # Anything    :

        Literal     : 'constant'
        Number      : 'constant.numeric'
        # Quote       :
        Str         : 'string'
        StrEsc      : 'constant.character.escape'
        List        : 'storage'
        StrWrongEsc : 'invalid.illegal'

        # Control     :
        Disabled    : 'disabled'

        Comment     : 'comment'
        # Doc         :

        # Unknown     :
    }

module.exports =
    lunaClass: (tag) -> lunaClasses[tag]
