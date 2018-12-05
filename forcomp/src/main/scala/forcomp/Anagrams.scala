package forcomp

import forcomp.Anagrams.{Occurrences, Sentence, combinations, sentenceOccurrences}


object Anagrams {

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
   *  how often the character appears.
   *  This list is sorted alphabetically w.r.t. to the character in each pair.
   *  All characters in the occurrence list are lowercase.
   *
   *  Any list of pairs of lowercase characters and their frequency which is not sorted
   *  is **not** an occurrence list.
   *
   *  Note: If the frequency of some character is zero, then that character should not be
   *  in the list.
   */
  type Occurrences = List[(Char, Int)]

  /** The dictionary is simply a sequence of words.
   *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = loadDictionary

  /** Converts the word into its character occurrence list.
   *
   *  Note: the uppercase and lowercase version of the character are treated as the
   *  same character, and are represented as a lowercase character in the occurrence list.
   *
   *  Note: you must use `groupBy` to implement this method!
   */
  def wordOccurrences(w: Word): Occurrences = w.toLowerCase.toList.groupBy(c => c).mapValues(_.size).toList.sortBy(_._1)

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = wordOccurrences(s mkString "")

  /** The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   *  the words that have that occurrence count.
   *  This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
   *
   *  For example, the word "eat" has the following character occurrence list:
   *
   *     `List(('a', 1), ('e', 1), ('t', 1))`
   *
   *  Incidentally, so do the words "ate" and "tea".
   *
   *  This means that the `dictionaryByOccurrences` map will contain an entry:
   *
   *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] =
    dictionary
      .map(word => (wordOccurrences(word) -> word))
      .groupBy(_._1)
      .mapValues(_.map(_._2))

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences(wordOccurrences(word))

  /** Returns the list of all subsets of the occurrence list.
   *  This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
   *  is a subset of `List(('k', 1), ('o', 1))`.
   *  It also include the empty subset `List()`.
   *
   *  Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
   *
   *    List(
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2))
   *    )
   *
   *  Note that the order of the occurrence list subsets does not matter -- the subsets
   *  in the example above could have been displayed in some other order.
   */
  def combinations(occurrences: Occurrences): List[Occurrences] = {
    val allLetterCombinations = for {
      (letter, maxRepeats) <- occurrences
      repeats <- 1 to maxRepeats
    } yield (letter, repeats)

    val allCombinationsGrouped = allLetterCombinations groupBy(_._1)

    def createCombinations(currentCombinations: List[Occurrences], remainingLetters: List[Char]): List[Occurrences] =
      if (remainingLetters.isEmpty) currentCombinations
      else {
        val letter = remainingLetters.head

        val newOccurences: List[Occurrences]  = for {
          letterOccurence <- allCombinationsGrouped(letter)
          existingOccurence <- currentCombinations
        } yield existingOccurence ++ List(letterOccurence)

        val nextCurrentOccurences = newOccurences ++ currentCombinations
        createCombinations(nextCurrentOccurences, remainingLetters.tail)
      }

    createCombinations(List(List()), occurrences.map(_._1))

  }


  /** Subtracts occurrence list `y` from occurrence list `x`.
   *
   *  The precondition is that the occurrence list `y` is a subset of
   *  the occurrence list `x` -- any character appearing in `y` must
   *  appear in `x`, and its frequency in `y` must be smaller or equal
   *  than its frequency in `x`.
   *
   *  Note: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   */
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    val yGrouped = y groupBy(_._1) mapValues(_.head._2)
    x map {
      case (ch, freq) => (ch, freq - yGrouped.getOrElse(ch, 0))
    } filter(_._2 > 0)
  }

  /** Returns a list of all anagram sentences of the given sentence.
   *
   *  An anagram of a sentence is formed by taking the occurrences of all the characters of
   *  all the words in the sentence, and producing all possible combinations of words with those characters,
   *  such that the words have to be from the dictionary.
   *
   *  The number of words in the sentence and its anagrams does not have to correspond.
   *  For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
   *
   *  Also, two sentences with the same words but in a different order are considered two different anagrams.
   *  For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
   *  `List("I", "love", "you")`.
   *
   *  Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
   *
   *    List(
   *      List(en, as, my),
   *      List(en, my, as),
   *      List(man, yes),
   *      List(men, say),
   *      List(as, en, my),
   *      List(as, my, en),
   *      List(sane, my),
   *      List(Sean, my),
   *      List(my, en, as),
   *      List(my, as, en),
   *      List(my, sane),
   *      List(my, Sean),
   *      List(say, men),
   *      List(yes, man)
   *    )
   *
   *  The different sentences do not have to be output in the order shown above - any order is fine as long as
   *  all the anagrams are there. Every returned word has to exist in the dictionary.
   *
   *  Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
   *  so it has to be returned in this list.
   *
   *  Note: There is only one anagram of an empty sentence.
   */
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {

    def allPossibleWordOccurences(occurrences: Occurrences): List[Occurrences] = {
      combinations(occurrences) filter(occ => dictionaryByOccurrences.contains(occ))
    }

    def generateSentencesAsOccurences(builtSentence: List[Occurrences], remainingOccurences: Occurrences): List[List[Occurrences]] = {
      if (remainingOccurences.isEmpty) List(builtSentence)
      else for {
        word <- allPossibleWordOccurences(remainingOccurences)
        sentence <- generateSentencesAsOccurences(word :: builtSentence, subtract(remainingOccurences, word))
      } yield sentence
    }

    val sentencesAsOccurences = generateSentencesAsOccurences(List.empty, sentenceOccurrences(sentence))

    def toWordSentences(sentenceAcc: Sentence, occurences: List[Occurrences]): List[Sentence] =
      if (occurences.isEmpty) List(sentenceAcc)
      else for {
        word <- dictionaryByOccurrences(occurences.head)
        sentence <- toWordSentences(word :: sentenceAcc, occurences.tail)
      } yield sentence

    for {
      occurance <- sentencesAsOccurences
      sentence <- toWordSentences(List.empty, occurance)
    } yield sentence
  }

  def sentenceAnagramsMemo(sentence: Sentence): List[Sentence] = {

    val allCombinationsSet = combinations(sentenceOccurrences(sentence)).toSet
    val allPossibleWordDictionaryMemo = dictionaryByOccurrences.filterKeys(allCombinationsSet.contains(_))

    def allPossibleWordOccurences(occurrences: Occurrences): List[Occurrences] = {
      combinations(occurrences) filter(occ => allPossibleWordDictionaryMemo.contains(occ))
    }

    // TODO: Ty not to recompute some anagrams more than once when recursively solving the problem. Think about a concrete example and a situation where you compute the anagrams of the same subset of an occurrence list multiple times.
    val memo = Map[Occurrences, List[Occurrences]]()
    def generateSentencesAsOccurences(builtSentence: List[Occurrences], remainingOccurences: Occurrences, occurenceMemo: List[Occurrences]): List[List[Occurrences]] = {
      if (remainingOccurences.isEmpty) {
        List(builtSentence)
      } else for {
        wordOccurence <- allPossibleWordOccurences(remainingOccurences)
        newRemainingOccurences = subtract(remainingOccurences, wordOccurence)
        newOccurenceMemo = newRemainingOccurences :: occurenceMemo
        sentence <- generateSentencesAsOccurences(wordOccurence :: builtSentence, newRemainingOccurences, newOccurenceMemo)
      } yield sentence
    }

    val sentencesAsOccurences = generateSentencesAsOccurences(List.empty, sentenceOccurrences(sentence), List.empty)

    def toWordSentences(sentenceAcc: Sentence, occurences: List[Occurrences]): List[Sentence] =
      if (occurences.isEmpty) List(sentenceAcc)
      else for {
        word <- dictionaryByOccurrences(occurences.head)
        sentence <- toWordSentences(word :: sentenceAcc, occurences.tail)
      } yield sentence

    for {
      occurance <- sentencesAsOccurences
      sentence <- toWordSentences(List.empty, occurance)
    } yield sentence
  }
}
