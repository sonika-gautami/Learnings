import coursera.functional.design.recap.FuncAndPatternMatch
import coursera.functional.design.recap.FuncAndPatternMatch.{JArr, JBool, JInt, JObj}

val list: Seq[Int] = Seq(1, 2, 3)

//Left hand side of for is named as 'generators'
//Below: 'i' is generator
for {i <- list} yield (i * i)
list.map(i => i * i)


for {
  i <- list
  if (i % 2 == 0)
} yield (i / 2)

//1st translated to
for {
  i <- list.withFilter(_ % 2 == 0)
} yield (i / 2)

//2nd translated to
list.withFilter(_ % 2 == 0).map(_ / 2)


for {
  i <- 1 until 3
  j <- 1 until i
} yield (i * j)

(1 until 3).flatMap(i => for {j <- 1 until i} yield (i * j))


//Generator can be a Pattern Match
val obj1 = JObj(Map(
  "bool" -> JBool(true),
  "int" -> JInt(1),
  "jArr" -> JArr(Seq(JInt(11), JInt(22), JInt(33))),
  "jObj" -> JObj(Map("objInt" -> JInt(1)))
))

for {
  JObj(o) <- List(obj1)
  JArr(a) = o("jArr")
  j <- a
  if j.isInstanceOf[JInt]
  n = j.asInstanceOf[JInt].i
} yield n


//case-pattern translated to:
List(obj1).withFilter {
  case JObj(o) => true
  case _ => false
}
