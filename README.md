nestify
=======

A command-line tool that indents each entry in a log file based on how "nested" it is.

Usage
-----

Use `nestify --help` for details about arguments. All input is through `stdin`.

Currently, a nestifiable log file must have at least two columns. The last column will be indented
and the penultimate column will be used as the name of a scope. Certain keywords are detected in
the last column to determine if a new scope is being added or removed from the stack. The keywords
for adding a new scope are "Enter" and "Begin". The keywords for ending a scope are "Exit" and "End".

For example, if `test.log` contains this

```
func1|Doing something.
func2|Enter
func2|Doing something else.
func2|Exit
func1|More stuff.
func3|Begin
func4|Begin
func5|Begin
func5|Ok, we're done.
func5|End
func4|Exit
func3|End
func1|Shutdown...
```

then piping `test.log` through `nestify` produces indented output:

```
$ cat test.log | nestify -d "|"
func1|Doing something.
func2|Enter
func2| Doing something else.
func2|Exit
func1|More stuff.
func3|Begin
func4| Begin
func5|  Begin
func5|   Ok, we're done.
func5|  End
func4| Exit
func3|End
func1|Shutdown...
```


Building
--------

You must have

  * [Glasgow Haskell Compiler](http://www.haskell.org/ghc/)
  * [Cabal](http://www.haskell.org/cabal/) (which comes with the [Haskell Platform](http://www.haskell.org/platform/))

Then you can

```bash
$ cd nestify
$ cabal install
```


License
-------

Copyright &copy; Covenant Eyes 2014

This package is licensed under the [MIT license](http://opensource.org/licenses/mit-license.php)
(see `LICENSE.txt`).
