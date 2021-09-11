" Vim syntax file " Language: Li

if !exists("g:main_syntax")
  if version < 600
    syntax clear
  elseif exists("b:current_syntax")
    finish
  endif
  let g:main_syntax = 'li'
  syntax region liFold start="{" end="}" transparent fold
endif

" Ensure long multiline strings are highlighted.
syntax sync fromstart

" keyword definitions
syntax keyword liConditional    if else switch
syntax keyword liRepeat         do while for
"syntax keyword liBoolean        true false
"syntax keyword liConstant       null
"syntax keyword liTypedef        this super class typedef
"syntax keyword liOperator       new is as in factory
"syntax match   liOperator       "+=\=\|-=\=\|*=\=\|/=\=\|%=\=\|\~/=\=\|<<=\=\|>>=\=\|[<>]=\=\|===\=\|\!==\=\|&=\=\|\^=\=\||=\=\|||\|&&\|\[\]=\=\|=>\|!\|\~\|?\|:"
syntax keyword liType           void u8 u16
syntax keyword liStatement      return
"syntax keyword liStorageClass   static abstract final const
"syntax keyword liExceptions     throw rethrow try on catch finally
"syntax keyword liAssert         assert
"syntax keyword liClassDecl      extends with implements
syntax keyword liBranch         break continue nextgroup=liUserLabelRef skipwhite
syntax keyword liKeyword        fn var asm include as const
syntax match   liLabelUse       display "\'\%([^[:cntrl:][:space:][:punct:][:digit:]]\|_\)\%([^[:cntrl:][:punct:][:space:]]\|_\)*"
syntax match   liLabelDecl      display "\'\%([^[:cntrl:][:space:][:punct:][:digit:]]\|_\)\%([^[:cntrl:][:punct:][:space:]]\|_\)*:"

"syntax match liLibrary         "^\(import\|part of\|part\|export\|library\|show\|hide\)\s"

" Comments
syntax keyword liTodo          contained TODO FIXME XXX
syntax region  liComment       start="/\*"  end="\*/" contains=liTodo,liDocLink,@Spell
syntax match   liLineComment   "//.*" contains=liTodo,@Spell

" Strings
syntax match	liCharacter	"L\='[^\\]'"
syntax match	liCharacter	"L'[^']*'" contains=liSpecialChar
"syntax match	cSpecial	display contained "\\\(x\x\+\|\o\{1,3}\|.\|$\)"
syntax match	liSpecialChar	display contained "\\\(x\x\+\|\o\{1,3}\|.\|$\)"
syntax region  liMultilineString     start=+\z(["`]\)+ skip=+\\\\\|\\\z1+ end=+\z1+ contains=liGbAsm,liGbReg,liAsmLabel,liAsmComment
"syntax region  liString        start=+\z(["']\)+ end=+\z1+ contains=@Spell,liInterpolation,liSpecialChar
"syntax region  liRawString     start=+r\z(["']\)+ end=+\z1+ contains=@Spell
"syntax region  liMultilineString     start=+\z("\{3\}\|'\{3\}\)+ end=+\z1+ contains=@Spell,liInterpolation,liSpecialChar
"syntax region  liRawMultilineString     start=+r\z("\{3\}\|'\{3\}\)+ end=+\z1+ contains=@Spell
"syntax match   liInterpolation contained "\$\(\w\+\|{[^}]\+}\)"
"syntax match   liSpecialChar   contained "\\\(u\x\{4\}\|u{\x\+}\|x\x\x\|x{\x\+}\|.\)"
syntax keyword liGbAsm contained ld xor and or add sub sbc rr rl db ds call di ei halt nop jp jr ret reti cp inc dec push pop swap
syntax keyword liGbReg contained a b c d e h l f af bc de hl sp
syntax match liAsmLabel contained /\I\i*:/
syn match liAsmComment contained /;.*/

" Numbers
syntax match liDecNumber   display "\<[0-9][0-9_]*\%([iu]\%(size\|8\|16\)\)\="
syntax match liHexNumber   display "\<0x[a-fA-F0-9_]\+\%([iu]\%(size\|8\|16\)\)\="

" The default highlighting.
highlight default link liBranch          Conditional
highlight default link liUserLabelRef    liUserLabel
highlight default link liLabelDecl       Label
highlight default link liLabelUse        Special
highlight default link liUserLabel       Label
highlight default link liConditional     Conditional
highlight default link liRepeat          Repeat
highlight default link liExceptions      Exception
highlight default link liAssert          Statement
highlight default link liStorageClass    StorageClass
highlight default link liClassDecl       liStorageClass
highlight default link liBoolean         Boolean
highlight default link liString          String
highlight default link liRawString       String
highlight default link liMultilineString String
highlight default link liCharacter       Number
highlight default link liRawMultilineString String
highlight default link liDecNumber       Number
highlight default link liHexNumber       Number
highlight default link liStatement       Statement
highlight default link liOperator        Operator
highlight default link liComment         Comment
highlight default link liLineComment     Comment
highlight default link liConstant        Constant
highlight default link liTypedef         Typedef
highlight default link liTodo            Todo
highlight default link liKeyword         Keyword
highlight default link liGbAsm           Macro
highlight default link liGbReg           Type
highlight default link liAsmComment      Comment
highlight default link liAsmLabel        Label
highlight default link liType            Type
highlight default link liInterpolation   PreProc
highlight default link liSpecialChar     SpecialChar
highlight default link liLibrary         Include

let b:current_syntax = "li"
let b:spell_options = "contained"

if g:main_syntax is# 'li'
  unlet g:main_syntax
endif
