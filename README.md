# CaPickling

**CaPickling** is lightweight object mapper for Cassandra written with support of [Scala Pickling](https://github.com/scala/pickling) for serialization.

CaPickling is too far from the being ORM. It's just a helper for binding to and from Cassandra data using [DataStax Java Driver for Cassandra](https://github.com/datastax/java-driver).

While the usage of CaPickle is simple it is/should be effective because of the power of underlying libraries:

* Scala Pickling that enables compile time parsing and code generation for serialization/deserialization code instead of using runtime reflection
* Using prepared statements from DataStax Java Driver

## Usage sample

Here is the sample of Scala class that is initialized with Cassandra session and allows you to do select/insert of some user data.

	import scala.pickling._
	import com.maqdev.capickling._
	
	class Db(session: com.datastax.driver.core.Session) {
	  case class User(userId: Int, name: String)
	
	  val insertStatement = new ConnectionStatement(session,
	    "insert into users(userid, name) values (?, ?)")
	
	  def insertUser(user: User) = insertStatement.executeWith(user)
	
	  val selectAllStatement = new ConnectionStatement(session,
	    "select * from users")
	
	  def selectAllUsers = selectAllStatement.select[User]
	}
 
And this class could be used like this:

	// create instance
	val db = new Db(session)
	
	// insert new user
    db.insertUser(db.User(10, "John"))
	
	// select all users (returns an iterator)
    val users = db.selectAllUsers
## Compilation    

I haven't pushed it into the public artifactory repositaries yet. So it should be used locally. 

To compile and publish into the local repositary:
sbt publish-local

For the unit tests and sample application working local instance of Cassandra is required. Please see schema in db/dbscript.cql
    
## Requirements

Currently tested and works only with:

* Cassandra 2.0.1 (corresponding driver with prepared statements)
* Scala 2.10
* Scala Pickling 0.8
* sbt 0.13

I've tested it only with case classes. They shouldn't be defined inside methods — they aren't recognised by pickle/unpickle.