import com.datastax.driver.core.BoundStatement
import org.scalatest._
import com.maqdev.capickling.capickling._

class CassandraPicklingSpec extends FlatSpec with Matchers {

  case class TestInt(testName: Int)

  "An integer " should " be bound " in {

    val m = org.scalatest.mock.MockitoSugar.mock[BoundStatement]
    val p = pickleFormat.createBuilder(m)
    TestInt(123456).pickleInto(p)

    org.mockito.Mockito.verify(m).setInt("test",123456)
  }

  /*"A Stack" should "pop values in last-in-first-out order" in {
    val stack = new Stack[Int]
    stack.push(1)
    stack.push(2)
    stack.pop() should be (2)
    stack.pop() should be (1)
  }

  it should "throw NoSuchElementException if an empty stack is popped" in {
    val emptyStack = new Stack[Int]
    a [NoSuchElementException] should be thrownBy {
      emptyStack.pop()
    } 
  }*/
}