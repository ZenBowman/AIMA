import org.zenbowman.logic.firstorder.FirstOrderLogic
import FirstOrderLogic._

val x = Variable("x")
val y = Variable("y")
val parent = Predicate("parent")
val child = Predicate("child")

val s3 = ForAll(x)(ForAll(y)(parent(x, y) <-> child(y, x)))
s3.toString

val s4 = ForAll(x)(ForAll(y)(parent(x, y) -> (x =!= y)))
s4.toString

