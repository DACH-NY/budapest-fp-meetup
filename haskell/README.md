# cek-scheme

- install readline lib like
```brew install readline```

-stack install readline haskell bindings (use the actual path you got)
```
LDFLAGS=-L/usr/local/opt/readline/lib \
CFLAGS=-I/usr/local/opt/readline/include \
stack install readline --extra-include-dirs=/usr/local/opt/readline/include --extra-lib-dirs=/usr/local/opt/readline/lib
```

- ```stack exec cek-scheme```
 
