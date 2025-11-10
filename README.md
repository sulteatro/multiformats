## Multiformats & friends

This is a Scala 3 implementation of the [multiformats](https://github.com/multiformats) self-describing data representation formats, including
* `varint`: a [variable-size integer format](https://github.com/multiformats/unsigned-varint) used by other `multiformats` types
* `multicodec`: self-describing codec representations defined by [a community standard](https://github.com/multiformats/multicodec)
* `multibase`: self-describing base-N encoded data defined by [a community standard](https://github.com/multiformats/multibase)
* `multihash`: self-describing hashed data defined by [a community standard](https://github.com/multiformats/multihash)
* `cid`: self-describing content-addressed identifiers (`CIDv1`) built on other `multiformats` and defined by [a community standard](https://github.com/multiformats/cid)

This repo also introduces a friend developed with similar extensibility and future-proofing in mind: `multiencoders`, a base-N encoder model that can be constructed from any alphabet and `N`. This package includes definitions for all defined `multibase` encoders, and can be easily extended as new ones are added.

### Install

To use the latest published version of this package, add this to your `build.sbt`:

```sbt
libraryDependencies += "io.sulteatro" %% "multiformats" % MultiformatsVersion
```

### Build from source

This project uses `sbt` for building & testing. Besides the usual features, it also includes update operations for the `multiformats` tables:
* `generateMulticode` generates Scala 3 code representing the latest [`multicodec` table](https://github.com/multiformats/multicodec/blob/master/table.csv)
* `generateMultibase` generates Scala 3 code representing the latest [`multibase` table](https://github.com/multiformats/multibase/blob/master/multibase.csv)

For example, to update the package from the standard multicodecs and multibase tables, run the update command(s) you need and publish the package to your local reposistory:

```bash
sbt generateMulticodec generateMultibase publishLocal
```

And of course, make sure to submit these updates in a pull request!
