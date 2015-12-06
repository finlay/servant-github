## GitHub bindings with servant

This library defines servant types that map to (parts of) the GitHub API v3
(GH-API). It should be useful for anyone wanting to use the GH-API in their
application. It does not aim to be complete, but should be easily extendible.

There are existing projects that provide a haskell client to the GH-API:
  * [github](http://hackage.haskell.org/package/github) is fairly complete and
    based on wreq and conduit,
  * [octohat](http://hackage.haskell.org/package/octohat) only implements parts
    of the API, and is based on wreq,
  * [hgithub](http://hackage.haskell.org/package/hgithub) is very simple,
    providing access only to the repository endponts.





