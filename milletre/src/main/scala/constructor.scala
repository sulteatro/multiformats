package milletre

object constructor:

  //
  // Generic validation error to be thrown from the `apply` method or other validators of type `To`
  //
  final case class ValidationError[T](
      private val message: String = "",
      private val cause: Throwable = None.orNull
  ) extends Exception(message, cause)

  trait EitherConversion[From, To] extends Conversion[From, Either[String, To]]

  given [F, T >: F <: F] => EitherConversion[F, T] = Right(_)

  //
  // Extension for objects defining a builder returning an `Either`
  //   to include ones for optional or value-or-throw results
  //
  type ConvertTo[Base] = [T] =>> EitherConversion[T, Base]

  trait MultiConstructor[
      Hidden,
      Shown >: Hidden <: Hidden,
      ToHidden <: ConvertTo[Hidden]
  ]:
    // Input is of core hidden type
    def isValid(source: Hidden): Boolean = true
    def validated(source: Hidden): Either[String, Shown] = Right(source)

    def ifValid(source: Hidden): Option[Shown] = validated(source).toOption

    def apply(source: Hidden): Shown =
      validated(source).fold(error => throw ValidationError[Shown](error.toString), identity)

    // Input is convertible to core hidden type
    def isValid[Source](source: Source)(using toHidden: ToHidden[Source]): Boolean =
      toHidden(source).exists(isValid)

    def validated[Source](source: Source)(using toHidden: ToHidden[Source]): Either[String, Shown] =
      toHidden(source).flatMap(validated)

    def ifValid[Source](source: Source)(using
        ToHidden[Source]
    ): Option[Shown] = validated(source).toOption

    def apply[Source](source: Source)(using
        ToHidden[Source]
    ): Shown =
      validated(source).fold(error => throw ValidationError[Shown](error.toString), identity)

  // Alias for a MultiConstructor with no ToHidden ingester
  type SimpleConstructor[H, S >: H <: H] = MultiConstructor[H, S, Nothing]

  // Alias for a MultiConstructor for which Hidden and Show are the same type
  type ClearConstructor[H, I <: ConvertTo[H]] = MultiConstructor[H, H, I]

  //
  // Functor case: opaque types with explicit type parameters determining conversion behavior
  //
  type ConvertFrom[Base, Out[_]] = [T] =>> EitherConversion[Base, Out[T]]

  trait MultiFactory[
      Hidden,
      Shown[_] >: Hidden <: Hidden,
      ToHidden <: ConvertTo[Hidden],
      FromHidden <: ConvertFrom[Hidden, Shown]
  ]:
    // Input is of core hidden type
    def isValid(source: Hidden): Boolean = true
    def validated[V](source: Hidden)(using toShown: FromHidden[V]): Either[String, Shown[V]] =
      toShown(source)

    def ifValid[V](source: Hidden)(using
        FromHidden[V]
    ): Option[Shown[V]] = validated(source).toOption

    def apply[V](source: Hidden)(using
        FromHidden[V]
    ): Shown[V] =
      validated(source).fold(error => throw ValidationError[Shown[V]](error.toString), identity)

    // Input is convertible to core hidden type
    def isValid[Source](source: Source)(using toHidden: ToHidden[Source]): Boolean =
      toHidden(source).exists(isValid)

    def validated[Source, V](source: Source)(using
        toHidden: ToHidden[Source],
        toShown: FromHidden[V]
    ): Either[String, Shown[V]] = toHidden(source).flatMap(validated)

    def ifValid[Source, V](source: Source)(using
        ToHidden[Source],
        FromHidden[V]
    ): Option[Shown[V]] = validated(source).toOption

    def apply[Source, V](source: Source)(using
        ToHidden[Source],
        FromHidden[V]
    ): Shown[V] =
      validated(source).fold(error => throw ValidationError[Shown[V]](error.toString), identity)

  // Alias for a MultiFactory with no ToHidden ingester
  type SimpleFactory[H, S[_] >: H <: H, F <: ConvertFrom[H, S]] = MultiFactory[H, S, Nothing, F]
