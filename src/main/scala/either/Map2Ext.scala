package either

case class Person(name: Name, age: Age)

sealed class Name(val value: String)

sealed class Age(val value: Int)

object Map2Ext {

  def mkName(name: String): Either[List[String], Name] =
    if (name == "" || name == null) Left(List("Name is empty.")) else Right(new Name(name))

  def mkAge(age: Int): Either[List[String], Age] = if (age < 0) Left(List("Age is out of range.")) else Right(new Age(age))

  def mkPerson(name: String, age: Int): Either[List[String], Person] = map2Ext(mkName(name), mkAge(age))(Person(_, _))

  def map2Ext[E, A, B, C](a: Either[List[E], A], b: Either[List[E], B])(f: (A, B) => C): Either[List[E], C] = (a, b) match {
    case (Left(e1), Left(e2)) => Left(e1 ::: e2)
    case (Left(e), _)         => Left(e)
    case (_, Left(e))         => Left(e)
    case (Right(a), Right(b)) => Right(f(a, b))
  }

  def main(args: Array[String]): Unit = {
    println(mkPerson("Joe", 22))
    println(mkPerson("Joe", -1))
    println(mkPerson("", 22))
    println(mkPerson("", -1))
  }
}
