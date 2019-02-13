# https://github.com/atom/template-syntax/blob/master/stylesheets/base.less
lunaClasses = {
        # --- Entities ---
        BlockStart  : 'entity.lambda'
        Group       : 'entity.group'

        # --- Variables ---
        Ident       : 'variable'
        Var         : 'variable.regular'
        Cons        : 'variable.constructor'
        Wildcard    : 'variable.wildcard'

        # --- Keywords ---
        Keyword     : 'keyword'
        KwCase      : 'keyword.control.case'
        KwOf        : 'keyword.control.of'
        KwClass     : 'keyword.definition.class'
        KwDef       : 'keyword.definition.function'
        KwImport    : 'keyword.definition.import'
        Operator    : 'keyword.operator'
        Modifier    : 'keyword.operator.modifier'
        Accessor    : 'keyword.operator.accessor'
        Assignment  : 'keyword.operator.assignment'
        Range       : 'keyword.operator.range'
        Anything    : 'keyword.operator.anything'

        # --- Literals ---
        Literal     : 'constant'
        Number      : 'constant.numeric'
        List        : 'constant.list'

        # --- Strings ---
        Str         : 'string'
        Block       : 'string.escape'
        Quote       : 'string.quote'
        StrEsc      : 'constant.character.escape'

        # --- Invalid ---
        Unknown     : 'invalid.unknown'
        StrWrongEsc : 'invalid.constant.character.escape'

        # --- Comments ---
        Comment     : 'comment'
        Disable     : 'comment.disabled'
        Doc         : 'comment.doc'

        # --- Helpers ---
        Marker      : 'helper.marker'
    }

module.exports =
    lunaClass: (tag) -> lunaClasses[tag]
