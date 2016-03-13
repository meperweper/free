import cats.data.{StateT, State}
import cats.free.Free
import cats._
import cats.implicits._

object GetTicketsExample extends App {

  trait DbCommand[A]

  case class Insert(a: String) extends DbCommand[Unit]

  case class Read(a: String) extends DbCommand[String]

  def insert(hello: String): Free[DbCommand, Unit] = Free.liftF(Insert(hello))

  def read(hello: String): Free[DbCommand, String] = Free.liftF(Read(hello))

  val business: Free[DbCommand, String] = for {
    _ <- insert("hi")
    _ <- insert("my")
    k <- read("123")
    _ <- insert("name")
    _ <- insert("is")
    z <- insert("ann")
  } yield k


  import cats.data.State
  import cats.syntax.all._
  import cats.free.Free.liftF
  type Cmnds = Seq[Any]
  type KVStoreState[A] = State[Cmnds, A]

  val interpreter: DbCommand ~> KVStoreState = new (DbCommand ~> KVStoreState) {
    override def apply[A](fa: DbCommand[A]): KVStoreState[A] = fa match {
      case z@Insert(str) =>
        val newState = State.modify((x: Cmnds) => {
          if (x.isEmpty)
            x :+ z
          else
            x.last match {
              case _: Read => x :+ z
              case Insert(inserted) => x.dropRight(1) :+ Insert(inserted + " " + str)
            }
        })
        newState
      case el: Read =>
        val state = State.modify((x: Cmnds) => x :+ el).map(x => "asd")
        state
    }
  }


  val map = business.foldMap(interpreter).run(Seq.empty[Any]).value
  println(map._1)
  println("End")

}