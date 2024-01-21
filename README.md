# resp3

This is a [RESP3](https://redis.io/docs/reference/protocol-spec/) parser,
implemented as a non-backtracking incremental scanner, using the
[`scanner`](https://hackage.haskell.org/package/scanner) library.

It aims to parse valid RESP3 messages as fast as possible, with
very little emphasis on error messages.
