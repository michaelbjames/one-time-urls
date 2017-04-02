# one-time-urls
Serve files with one-time URLs

## Setup
1. Use [stack](https://github.com/commercialhaskell/stack). Once you have that installed:
0. `stack install`
0. Hopefully the `otu-server` and `otu` binaries are in your `$PATH`
0. You'll need redis running on its default port: 6397. Start it with (depending on your local OS flavor): `redis-server`
0. Start the server `otu-server`. It runs on port `8000`.

## REST interface
It all runs on port `8000`.
- `GET /file/:fileHash` - This endpoint can be used once per fileHash
- The administrative endpoint: `POST /insert/:filePath/:hash` - point a keyword or hash to a file.

You'll want to use `otu` to place a file on the hash. Only expose the `GET` endpoint publicly otherwise you open yourself to a host of nasty security concerns.

## Example
```
$ otu-server&
$ otu ~/Documents/for_british_eyes_only.jpg
/file/37ceb063046c41f3edd22dfd1be894b9
$ otu ~/Documents/secret_sauce.txt --name sauce
/file/sauce
$ curl localhost:8000/file/sauce
-sugar
-butter
-fish sauce
....
$ curl localhost:8000/file/sauce
404 Not Found

```
