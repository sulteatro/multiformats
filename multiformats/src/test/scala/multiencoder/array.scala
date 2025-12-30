package multiencoder

import array.SizedArray
import array.SizedArray.End

class SizedArrayTests extends munit.FunSuite:

  // sizes: 0, 1, 2, 10
  // methods: size, apply (element access), map, contains, toArray
  // compilation: size mismatch

  private val zero: SizedArray[0, String] = End
  private val one: SizedArray[1, String] = "one" :: End
  private val two: SizedArray[2, String] = "one" :: "two" :: End
  private val ten: SizedArray[10, Int] = 1 :: 2 :: 1 :: 7 :: 10 :: 6 :: 8 :: 14 :: 0 :: 3 :: End

  test("SizedArray size matches its size type"):
    assertEquals(zero.size, 0)
    assertEquals(one.size, 1)
    assertEquals(two.size, 2)
    assertEquals(ten.size, 10)

  test("SizedArray.toArray produces an Array containing the same values"):
    assertEquals(zero.toArray.toSeq, Array.empty[String].toSeq)
    assertEquals(one.toArray.toSeq, Array("one").toSeq)
    assertEquals(two.toArray.toSeq, Array("one", "two").toSeq)
    assertEquals(ten.toArray.toSeq, Array(1, 2, 1, 7, 10, 6, 8, 14, 0, 3).toSeq)

  test("SizedArray.apply returns the value at that index (or throws if out of bounds)"):
    interceptMessage[ArrayIndexOutOfBoundsException]("Index exceeds array size"):
      zero(0)

    assertEquals(one(0), "one")
    assertEquals(two(1), "two")
    assertEquals(ten(5), 6)

  test("SizedArray.map transforms data values and the value type"):
    assertEquals(zero.map(v => s"$v mapped"), zero)
    assertEquals(one.map(v => s"$v mapped"), "one mapped" :: End)
    assertEquals(two.map(v => v.length), 3 :: 3 :: End)
    assertEquals(ten.map(v => v - 1), 0 :: 1 :: 0 :: 6 :: 9 :: 5 :: 7 :: 13 :: -1 :: 2 :: End)

  test("SizedArray.contains checks whether a value is present in the array"):
    assertEquals(zero.contains("one"), false)
    assertNoDiff(
      compileErrors("""zero.contains(1)"""),
      """"""
    )

    assertEquals(one.contains("two"), false)
    assertEquals(two.contains("two"), true)
    assertEquals(ten.contains(0), true)
