# resp

[![GHC version badge](https://img.shields.io/badge/ghc-%3E%3D7.4.2-blue?logo=haskell)](https://www.haskell.org/) [![CI status badge](https://img.shields.io/github/actions/workflow/status/414owen/resp/haskell-ci.yml)](https://github.com/414owen/neresp/actions/workflows/haskell-ci.yml) [![Hackage version badge](https://img.shields.io/hackage/v/resp)](https://hackage.haskell.org/package/resp) [![license](https://img.shields.io/github/license/414owen/resp)](https://github.com/414owen/resp/blob/master/LICENSE)

This is a [RESP3](https://redis.io/docs/reference/protocol-spec/) parser,
implemented as a non-backtracking incremental scanner, using the
[`scanner`](https://hackage.haskell.org/package/scanner) library.

It aims to parse valid RESP3 messages as fast as possible, with
very little emphasis on error messages.
