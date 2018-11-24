package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._
import org.scalatest.Matchers._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
	}


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("weight of a larger tree2") {
    new TestTrees {
      assert(weight(t2) === 9)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times(\"hello, world\")") {
    times(string2Chars("hello, world")) should contain theSameElementsAs List(('h', 1), ('e', 1), ('l', 3), ('o', 2), (',', 1), (' ', 1), ('w', 1), ('r', 1), ('d', 1))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("combine of some leaf list2") {
    val leaflist = List(Leaf('e', 2), Leaf('t', 3), Leaf('x', 4))
    assert(combine(leaflist) === List(Leaf('x',4), Fork(Leaf('e',2),Leaf('t',3),List('e', 't'),5)))
  }

  test("create tree") {
    val t = createCodeTree(string2Chars("Hello World"))
    println(t)
  }

  test("create tree frenchCode") {
    val allFrenchChars = frenchCode match {
      case f: Fork => f.chars
      case _ => throw new Error("wrong")
    }
    val newFrenchTree = createCodeTree(allFrenchChars)
    val encoded1 = quickEncode(frenchCode)(allFrenchChars)
    val encoded2 = quickEncode(newFrenchTree)(allFrenchChars)

    assert(encoded2.size <= encoded1.size)



  }

  test("codeBits") {
    new TestTrees {
      def strToBin(str: String): List[Bit] = str.toList.map(_.asDigit)
      assert(codeBits(convert(t2))('a') == strToBin("00"))
      assert(codeBits(convert(t2))('b') == strToBin("01"))
      assert(codeBits(convert(t2))('d') == strToBin("1"))
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("quickEncode") {
    new TestTrees {
      assert(decode(t1, quickEncode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("decode secret") {
    new TestTrees {
      assert(decodedSecret === "huffmanestcool".toList)
    }
  }

  test("decode and encode longer strings using frenchcode and quickEncode") {
    assert(decode(frenchCode, quickEncode(frenchCode)("thisisatest".toList)) === "thisisatest".toList)
  }

  test("encode and decode a sample string using new custome tree") {
    val myText = "thisisatestofyourcreatecodetreemethod".toList
    val myTree = createCodeTree(myText)
    val encoded = encode(myTree)(myText)
    val decoded = decode(myTree, encoded)
      assert(decoded === myText)

  }

}
