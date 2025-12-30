package multiencoder

import scala.compiletime.ops.int.S
import scala.reflect.ClassTag

object array:
  sealed trait SizedArray[Size <: Int, +V](using Size: ValueOf[Size]):
    val size: Size = Size.value

    def ::[T >: V: ClassTag](v: T)(using ValueOf[S[Size]]): SizedArray[S[Size], T]

    def apply(index: Int): V
    def map[W: ClassTag](fn: V => W): SizedArray[Size, W]
    def contains[T >: V: ClassTag](elem: T): Boolean

    def toArray[T >: V: ClassTag]: Array[T]
    override def toString: String
    override def equals(other: Any): Boolean

  object SizedArray:
    case class Elem[Prev <: Int, V: ClassTag] private[array] (private val data: Array[V])(using
        Size: ValueOf[S[Prev]]
    ) extends SizedArray[S[Prev], V]:
      type Size = S[Prev]

      override def ::[T >: V: ClassTag](v: T)(using ValueOf[S[Size]]): SizedArray[S[Size], T] =
        new Elem[Size, T](v +: this.data)

      override def apply(index: Int): V = data(index)
      override def map[W: ClassTag](fn: V => W): SizedArray[Size, W] = Elem[Prev, W](data.map(fn))
      override def contains[T >: V: ClassTag](elem: T): Boolean =
        elem match
          case summon[ClassTag[V]](vElem) => data.contains(vElem)
          case _                          => false

      override def toArray[T >: V: ClassTag]: Array[T] = Array.from(data)
      override def toString: String = s"Array(${data.mkString(", ")})"
      override def equals(other: Any): Boolean =
        other match
          case sa: SizedArray[?, ?] =>
            size.equals(sa.size) && (0 until size).forall(i => apply(i).equals(sa(i)))
          case _ => false

    case object End extends SizedArray[0, Nothing]:
      type Size = 0

      override def ::[V: ClassTag](v: V)(using One: ValueOf[1]): SizedArray[1, V] =
        Elem[0, V](Array(v))

      override def apply(index: Int): Nothing =
        throw new ArrayIndexOutOfBoundsException("Index exceeds array size")
      override def map[W: ClassTag](fn: Nothing => W): SizedArray[0, W] = End
      override def contains[V: ClassTag](elem: V): Boolean = false

      override def toArray[V: ClassTag]: Array[V] = Array.empty[V]
      override def toString: String = "Array()"
      override def equals(other: Any): Boolean =
        other match
          case sa: SizedArray[?, ?] => size.equals(sa.size)
          case _                    => false
