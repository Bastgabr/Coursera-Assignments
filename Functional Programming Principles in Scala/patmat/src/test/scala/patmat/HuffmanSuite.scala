package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a', 'b', 'd'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times") {
    assert(times(List('a', 'b', 'a')).toSet === Set(('a', 2), ('b', 1)))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
  }

  test("createCodeTree") {
    assert(createCodeTree("aaaaaaaabbbcdefgh".toList) == Fork(Leaf('a', 8), Fork(Fork(Fork(Leaf('h', 1), Leaf('d', 1), List('h', 'd'), 2), Fork(Leaf('g', 1), Leaf('c', 1), List('g', 'c'), 2), List('h', 'd', 'g', 'c'), 4), Fork(Fork(Leaf('e', 1), Leaf('f', 1), List('e', 'f'), 2), Leaf('b', 3), List('e', 'f', 'b'), 5), List('h', 'd', 'g', 'c', 'e', 'f', 'b'), 9), List('a', 'h', 'd', 'g', 'c', 'e', 'f', 'b'), 17))
  }

  test("decode") {
    new TestTrees {
      assert(decode(t2, List[Bit](0, 0)) == List('a'))
      assert(decode(t2, List[Bit](0, 1)) == List('b'))
      assert(decode(t2, List[Bit](1)) == List('d'))
      assert(decode(t1, List[Bit](0)) == List('a'))
      assert(decode(t1, List[Bit](1)) == List('b'))
    }
  }

  test("decoded secret") {
    assert(decodedSecret == "huffmanestcool".toList)
  }

  test("convert") {
    new TestTrees {
      assert(convert(t1) == List(('a', List(0)), ('b', List(1))))
      assert(convert(t2) == List(('a', List(0, 0)), ('b', List(0, 1)), ('d', List(1))))

    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
      assert(decode(t2, encode(t2)("abd".toList)) === "abd".toList)
    }
  }

  test("decode and quick encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, quickEncode(t1)("ab".toList)) === "ab".toList)
      assert(decode(t2, quickEncode(t2)("abd".toList)) === "abd".toList)
    }
  }
}
