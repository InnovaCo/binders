# binders

**Binders** is lightweight object mapper libraries for Scala <-> Databases targeted to minimize boilerplate code to communicate with database in efficient way.

binders is too far from the being ORM. It's just a helper for binding to and from db specific object such as Statement, ResultSet and Row with the support of Scala Macros.

binders consist from **binders-core** and database specific libraries. Currently **binders-cassandra** is awailable and you can [find it here](/InnovaCo/binders-cassanrda)

## Usage sample

Sample is based on [binders-cassandra](/InnovaCo/binders-cassanrda)

Here is the sample of Scala class that is initialized with Cassandra session and allows you to do select/insert of some user data.

	import eu.inn.binders._
	import eu.inn.binders.cassandra._
	
	
	class Db(session: com.datastax.driver.core.Session) {
	  // class for binding input/output parameters
	  case class User(userId: Int, name: String)
	
	  lazy val insertStatement = new Query(session,
	    "insert into users(userid, name) values (?, ?)")
	
	  def insertUser(user: User) = insertStatement.execute(user)
	
	  lazy val selectAllStatement = new Query(session,
	    "select * from users")
	
	  def selectAllUsers = selectAllStatement.execute().unbindAll[User]
	
	  lazy val selectUserStatement = new Query(session,
	    "select * from users where userId = ?")
	
	  def selectUser(userId: Int) = selectUserStatement.execute(userId).unbindOne[User]
	}
 
And this class could be used like this:

	val cluster = Cluster.builder().addContactPoint("127.0.0.1").build()
    val session = cluster.connect("capickling_tests")

    val db = new Db(session)
    db.insertUser(db.User(10, "John"))

    val users = db.selectAllUsers.toList
    
    val user1 = db.selectUser(1)
    
## Compilation    

I haven't pushed it into the public artifactory repositaries yet. So it should be used locally. 

To compile and publish into the local repositary:
sbt publish-local
    
## Requirements

binders-core library currently tested and works only with:

* Scala 2.10
* scala-reflect (macros, reflection)
* sbt 0.13
