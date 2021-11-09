# Run fix & foxi
```sh
$ cd src/
$ sml

$ use "use.sml";
$ OS.FileSys.chDir "..\\Grammars";
$ use "Grammar_CK2.sml";
```

generate sml code with python

```sh

cd Grammars
python transform.py
```

script transforms `grammar.ebnf` to `Grammar_generated.sml`