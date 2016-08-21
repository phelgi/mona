# mona
A monadic json parser written in scheme

Based on the paper
"Monadic Parser Combinators"
by  Graham Hutton and Erik Meijer

Compliant to R4RS (because it was written for an old scheme interpreter)

Usage:

> (define obj (caar (object (read-json "my-file.json"))))

results in a parsed representation of the json object.

To access the data:

> (get-objects obj)

> (get-value obj 'some-sub-object)

> (get-fields obj 'some-field-name)

Have fun
