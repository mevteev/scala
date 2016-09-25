import forcomp.Anagrams
import forcomp.Anagrams.{Occurrences, Sentence}

val w = "eat"

(w.toLowerCase.sorted groupBy((element: Char) => element)).toList

((w.toLowerCase groupBy(e => e)) map( g => (g._1, g._2.length))).toList
  .sorted

  //.map (((l:Char, c:String)) => (l, c.length))


val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
val r = List(('r', 1))
val lad = List(('a', 1), ('d', 1), ('l', 1))

Anagrams.subtract(lard, r)


def getList(c: (Char, Int)): List[Occurrences] =
  (for (n <- 1 to c._2) yield List((c._1, n))).toList ++ List()





val abba = List(('a', 2), ('b', 2))

getList(('a', 2))
getList(('b', 2))




def subtract(x: Occurrences, y: Occurrences): Occurrences = {
  val yMap = y.toMap withDefaultValue(0)
  for (
    xw <- x
    if (xw._2 > yMap(xw._1))
  ) yield (xw._1, xw._2 - yMap(xw._1))
}


val abb = List(('a', 1), ('b', 2))

val x = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
val y = List(('r', 1))

subtract(x, y)


def sentenceAnagrams(sentence: Sentence): List[Sentence] = {

  val dict = Anagrams.dictionaryByOccurrences withDefaultValue(List())

  def getOcc(comb: List[Occurrences]): List[Sentence] = {
    for (
      c <- comb
      if (dict(c).nonEmpty)
    ) yield (dict(c))
  }

  def occurAnagrams(occ: Occurrences): List[Sentence] = {
    val comb = Anagrams.combinations(occ)


    val lst = getOcc(comb)

    for (
      w <- lst
    ) yield (occurAnagrams(subtract(Anagrams.sentenceOccurrences(sentence),
      Anagrams.wordOccurrences(w.head))))

  }

}



sentenceAnagrams(List("Linux", "rulez"))

