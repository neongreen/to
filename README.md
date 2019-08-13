# to

`to` contains type conversions for popular Haskell types. All provided
conversions are safe and boring.

## Why?

* No orphan instances (rules out `conversion`).

* Descriptive names (rules out `string-conversions` which lets people use `cs`).

* Does not mix conversion and rendering (rules out `bytestring-conversions`).

* Encourages lenient decoding (rules out `text` and `string-conv`).

* Requires specifying encoding.

## TODO

* Numbers
* Sets, Maps
* Vectors
* `ShortByteString` and `Data.ByteString.Builder`
