package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
    // lies!
    val t3 = Fork(Leaf('a',7), Leaf('x',8), List('a','b'), 999)
    val t4 = Fork(Leaf('a',7), Leaf('b',8), List('a','b'), 15)

  }

  test("weight of a small tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("weight of a big tree") {
    new TestTrees {
      assert(weight(t2) === 9)
    }
  }

  test("weight of a tree with lying tree") {
    new TestTrees {
      assert(weight(t3) === 15)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("chars of a lying tree") {
    new TestTrees {
      assert(chars(t3) === List('a','b'))
    }
  }

  test("times short") {
    new TestTrees {
      assert(times(List('a', 'b', 'a')) === List(('a', 2), ('b', 1)))
    }
  }

  test("times longer") {
    new TestTrees {
      assert(times(List('a','b','c','c','a','a','a','a')) ===
        List(('a', 5), ('b', 1), ('c', 2)))
    }
  }

  test("ordered leaf list") {
    new TestTrees {
      assert(
        makeOrderedLeafList(List(('a', 2), ('b', 1))) ===
        List(Leaf('b', 1), Leaf('a', 2)
        )
      )
    }
  }

  test("singleton true") {
    new TestTrees {
      assert(
        singleton(List(t1)) === true
      )
    }
  }

  test("singleton false") {
    new TestTrees {
      assert(
        singleton(List(t1, t2)) === false
      )
    }
  }

  test("singleton empty") {
    new TestTrees {
      assert(
        singleton(List()) === false
      )
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("makeOrderedLeafList of nill") {
    assert(makeOrderedLeafList(List()) === List())
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("decode char") {
    val tree = createCodeTree(string2Chars("eeeott"))
    new TestTrees {
      assert(decodeChar(tree, List(0, 0)) === ('o', List()))
      assert(decodeChar(tree, List(0, 1)) === ('t', List()))
      assert(decodeChar(tree, List(1)) === ('e', List()))
    }
  }

  test("getChars") {
    val tree = createCodeTree(string2Chars("eeeott"))
    new TestTrees {
      assert(getChars(tree) === "ote".toList)
    }
  }

  test("decode word") {
    val tree = createCodeTree(string2Chars("eeeott"))
    new TestTrees {
      assert(decode(tree, List(0, 0, 0, 1, 1)) === "ote".toList)
    }
  }

  test("encode char") {
    val tree = createCodeTree(string2Chars("eeeott"))
    new TestTrees {
      assert(encodeChar(tree)('t') === List(0, 1))
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("decode and encode a longer text should be identity") {
    new TestTrees {
      val msg = "abaabbbaaaaabbbbbbb"
      assert(decode(t4, encode(t4)(msg.toList)) === msg.toList)
    }
  }

  test("read codetable") {
    val tab = List(('q', List(0, 1)), ('w', List(1, 0)))
    new TestTrees {
      assert(codeBits(tab)('w') === List(1,0))
    }
  }

  test("merge code tables") {
    val tab1 = List(('q', List(0, 1)), ('w', List(1, 0)))
    val tab2 = List(('a', List(0, 0)), ('b', List(1, 1)))
    new TestTrees {
      assert(mergeCodeTables(tab1, tab2).toSet === List(
          ('q', List(0, 1)), ('w', List(1, 0)),
          ('a', List(0, 0)), ('b', List(1, 1))
        ).toSet)
    }
  }

  test("decode and quick encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, quickEncode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("decode and quick encode a longer text should be identity") {
    new TestTrees {
      val msg = "abaabbbaaaaabbbbbbb"
      assert(decode(t4, quickEncode(t4)(msg.toList)) === msg.toList)
    }
  }
}
