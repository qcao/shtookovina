# How to Translate User Interface?

Copy `en.lisp` file renaming it according to
[ISO 639-2](http://www.loc.gov/standards/iso639-2/php/code_list.php)
standard. Name of the new files should consist of two-letter code of
new language and `.lisp` suffix.

`en.lisp` is default English translation of the user interface. Other
translations can be easily written by replacing of corresponding English
strings. To apply some style (see documentation of
[`cl-ansi-term`](https://mrkkrp.github.io/cl-ansi-term/) library) enclose
text in brackets, and immediately after closing bracket `]`, put name of
desired style in parenthesis. For example: `[my text](cmd)`. To get full
list of default styles, see `src/default-style.lisp'.

First of all, edit commentary on the top of the file mentioning name of
language of user interface. Now just edit arguments of `set-ui-language`
function translating them into your language. Don't edit keywords («headers»
of every string, they begin with colon `:`).

When you finished, run Шτookωвiнα and test your translation of user
interface. Try to track down strange translations and other nasty
things. When you are sure that your translation is good, open pull request.
