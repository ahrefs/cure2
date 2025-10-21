# Cure2

Cure2 is a combinator frontend for Re2. It allows you to use Re2 without having
to deal with the syntax.

Instead of `[+-]?(\d+(\.\d*)?|\.\d+)([eE][+-]?\d+)?` you can write

```ocaml
  opt (chars "+-")
+ ((rep1 digit + opt (char '.' + rep digit)) || (char '.' + rep1 digit))
+ opt (chars "eE" + opt (chars "+-") + rep1 digit)
```

Instead of `"https?:\\/\\/(?:www\\.)?[-a-zA-Z0-9@:%._\\+~#=]{1,256}\\.[a-zA-Z0-9()]{1,6}\\b(?:[-a-zA-Z0-9()@:%_\\+.~#?&//=]*)"`,
you can write:
```ocaml
let second_level_char = charset Charset.[Ascii.alnum; chars "-@:%._\\+~#="] in
let top_level_chars = charset Charset.[Ascii.alnum; chars "()"] in
let path_chars = charset Charset.[Ascii.alnum; chars "()@:%_\\+.~#?&/="] in
str "http" + !?(char 's') + str "://"
+ !?(str "www.") + rep ~min:1 ~max:256 second_level_char
+ char '.' + rep ~min:1 ~max:6 top_level_chars
+ bow + rep path_chars
```

This is more verbose but also way more readable. And contrary to regexp you just
hover on anything you are not familiar with and documentation will show up.

The interface is inspired by the `Re` library, but there are a few difference in
cases where I thought it could be improved or where the semantics had to be
different because the backends are not the same.

The main reason to use this over `Re` is the Unicode support. There are also
performance differences, but according to my testing they are minimal.