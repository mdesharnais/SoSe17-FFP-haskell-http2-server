# SoSe17-ffp-haskell-http2-server

# To test

## Console 1:

```
$ stack exec http2-server-exe
Listening on 127.0.0.1:8080
Incoming connection from 127.0.0.1:33824
HTTP/2 Connection prefix received.
TSettings(0)
  MaxConcurrentStreams = 100
    InitialWindowSize = 1073741824
```

## Console 2:

curl --http2-prior-knowledge localhost:8080
