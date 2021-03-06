# [URL Shortener](http://url-shorten.hs.vc/)

The project as it is hosts a webserver serving a web-based frontend at
`localhost:8080`, it's just a quick no-framework wrapper around the API, which
is at `/api`. The API itself exposes `/api/shorten` (PUT) and `/api/:shortId`
(GET) as outlined in the challenge, and both endpoints can accept/produce
plaintext (with `Content-Type: text/plain;charset=utf-8`) or JSON (with
`Content-Type: application/json`).

The webserver itself is built using WAI, Warp, and Servant, and (currently)
talks to a Postgres database. The shortened URLs are based on the SHA256 hash,
so shortening the same URL twice gives the same code instead of adding a
duplicate.

## How to run/build/test

The postgres server expects a database specified by the connection string in
`pgConnStr`, tests expect the same from `pgTestConnStr`. System-level
dependencies are handled by Stack and Nix, so assuming a system with Nix,
`nix-shell --run 'stack {run/build/test}` will take care of the rest!

## What could be improved

- If it were a bigger project, the frontend should probably be redone using some
  frontend framework, perhaps React with Typescript or `purescript-halogen`.
- The URL validation on the server is fairly coarse - it probably shouldn't
  accept nonsense like `aaaaaaa:///////:///$$` or even just `://`
- The `pgConnStr` and `pgTestConnStr` files are a bit hacky; they should
  probably be moved to a "real" config file format, but that'd have to pull in
  parsers and such

## Attribution

The icon is based off of the [Twemoji chain-link
emoji](https://twemoji.twitter.com/).
