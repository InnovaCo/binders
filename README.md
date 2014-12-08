# binders

**TODO**
new description needed

**Binders** is lightweight object mapper libraries for Scala <-> Databases targeted to minimize boilerplate code to communicate with database in efficient way.

binders is too far from the being ORM. It's just a helper for binding to and from db specific object such as Statement, ResultSet and Row with the support of Scala Macros.

binders consist from **binders-core** and database specific libraries. Currently **binders-cassandra** is available and you can [find it here](https://github.com/InnovaCo/binders-cassandra)

## Usage sample

Usage example: [binders-cassandra](https://github.com/InnovaCo/binders-cassandra)

## Current version

    libraryDependencies += "eu.inn" %% "binders-core" % "0.2.0"

## Requirements

binders-core library currently tested with:

* Scala 2.10.3
* scala-reflect (macros, reflection)
* sbt 0.13

## License

Product licensed under BSD 3-clause as stated in file LICENSE