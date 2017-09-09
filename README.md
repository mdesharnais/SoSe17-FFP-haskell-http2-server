# SoSe17-ffp-haskell-http2-server

## Building

Um das Binary zu kompilieren muss

```
$ stack init
$ stack build
```
ausgeführt werden.

Ausführen der Software durch
```
$ stack exec http2-server-exe
```

## Testen

Verwenden kann man den Server mit dem Program
curl. <b>Auchtung!</b> curl muss mit Support 
für HTTP/2 kompiliert worden sein.

```
curl --http2-prior-knowledge localhost:8080
```

Für die TLS Version muss man in app/Main.hs

```
main = mainTLS
```
setzen. 
Nach erneutem stack build und stack exec
kann man die TLS Version mit
```
curl -k --tlsv1.2 --http https://localhost:8443
```
testen.
