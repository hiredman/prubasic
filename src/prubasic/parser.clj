(ns prubasic.parser
  (:require [instaparse.core :as insta]))

(def basic-g
  "
<program> = [endln] program | command-line endln program | command-line [endln]
command-line = digits ws command
<command> = let | for | next | if | goto | end | read | write
let = <'LET '> variable-name <' = '> expression
for = <'FOR '> variable-name <' = '> value <' TO '> value
next = <'NEXT '> variable-name
if = <'IF '> comparison-expression  <' THEN '> command
goto = <'GOTO '> digits
end = <'END'>
read = <'READ '> variable-name <' '> expression
write = <'WRITE '> variable-name <' '> expression

expression = value | variable-name | expression ws operator ws expression
value = hex-number
hex-number = '0x' hex-digits
hex-digits = #'[a-fA-F0-9]+'
digits = #'[0-9]+'
variable-name = #'[a-z][a-zA-Z0-9]*'
<endln> = <'\n'>
<ws> = <#'\\s+'>
operator = '+'
comparison = '=' | '>' | '<'
comparison-expression = expression ws comparison ws expression
")


(def parse (insta/parser basic-g))

(def basic-g2
  "
<program> = [endln] program | command-line endln program | command-line [endln]
command-line = <[ws]> command
<command> = let | for | next | if | goto | end | read | write | label
let = <'LET '> variable-name <' = '> expression
for = <'FOR '> variable-name <' = '> value <' TO '> value
next = <'NEXT '> variable-name
if = <'IF '> comparison-expression  <' THEN '> command
goto = <'GOTO '> label-name
end = <'END'>
read = <'READ '> variable-name <' '> expression
write = <'WRITE '> variable-name <' '> expression
label = label-name ':'

label-name = #'[A-Z][A-Z0-9]+'
expression = value | variable-name | expression ws operator ws expression
value = hex-number
hex-number = '0x' hex-digits
hex-digits = #'[a-fA-F0-9]+'
digits = #'[0-9]+'
variable-name = #'[a-z][a-zA-Z0-9]*'
<endln> = <'\n'>
<ws> = <#'\\s+'>
operator = '+'
comparison = '=' | '>' | '<'
comparison-expression = expression ws comparison ws expression
")

(def parse2 (insta/parser basic-g2))
