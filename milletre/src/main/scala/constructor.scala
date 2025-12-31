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
    // Input is convertible to core hidden type
    def isValid[Source](source: Source)(using toHidden: ToHidden[Source]): Boolean =
      toHidden(source).isRight

    def validated[Source](source: Source)(using toHidden: ToHidden[Source]): Either[String, Shown] =
      toHidden(source)

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
  type ClearConstructor[Type, ToType <: ConvertTo[Type]] = MultiConstructor[Type, Type, ToType]

  //
  // Multi-case version of MultiConstructor
  //
  type ConvertToFor[Base[_]] = [C, T] =>> EitherConversion[T, Base[C]]

  trait CaseMultiConstructor[
      Hidden[_],
      Shown >: Hidden <: Hidden,
      ToHidden <: ConvertToFor[Hidden]
  ]:
    // Input is convertible to core hidden type
    def isValid[C]: [Source] => Source => ToHidden[C, Source] ?=> Boolean =
      [Source] => (source: Source) => (toHidden: ToHidden[C, Source]) ?=> toHidden(source).isRight

    def validated[C]: [Source] => Source => ToHidden[C, Source] ?=> Either[String, Shown[C]] =
      [Source] => (source: Source) => (toHidden: ToHidden[C, Source]) ?=> toHidden(source)

    def ifValid[C]: [Source] => Source => ToHidden[C, Source] ?=> Option[Shown[C]] =
      [Source] =>
        (source: Source) =>
          (toHidden: ToHidden[C, Source]) ?=> validated[C](source).toOption

    def apply[C]: [Source] => Source => ToHidden[C, Source] ?=> Shown[C] =
      [Source] =>
        (source: Source) =>
          (toHidden: ToHidden[C, Source]) ?=>
            validated[C](source).fold(
              error => throw ValidationError[Shown[C]](error.toString),
              identity
          )

  //
  // Functor case: opaque types with explicit type parameters determining conversion behavior
  //
  type ConvertFrom[Base, Out[_]] = [T] =>> EitherConversion[Base, Out[T]]

  trait MultiFactory[
      Hidden,
      Shown[_] >: Hidden <: Hidden,
      ToHidden <: ConvertTo[Hidden],
      FromHidden <: ConvertFrom[Hidden, Shown]
  ](using ToHidden[Hidden]):
    // Input is convertible to core hidden type
    def isValid[Source](source: Source)(using toHidden: ToHidden[Source]): Boolean =
      toHidden(source).isRight

    def validated[V](using
        toShown: FromHidden[V]
    ): [Source] => Source => (ToHidden[Source]) ?=> Either[String, Shown[V]] =
      [Source] =>
        (source: Source) =>
          (toHidden: ToHidden[Source]) ?=> toHidden(source).flatMap(toShown)

    def ifValid[V](using
        toShown: FromHidden[V]
    ): [Source] => Source => (ToHidden[Source]) ?=> Option[Shown[V]] =
      [Source] => (source: Source) => (toHidden: ToHidden[Source]) ?=> validated(source).toOption

    def apply[V](using
        toShown: FromHidden[V]
    ): [Source] => Source => (ToHidden[Source]) ?=> Shown[V] =
      [Source] =>
        (source: Source) =>
          (toHidden: ToHidden[Source]) ?=>
            validated(source).fold(
              error => throw ValidationError[Shown[V]](error.toString),
              identity
          )

  // Alias for a MultiFactory with no ToHidden ingester
  type SimpleFactory[H, S[_] >: H <: H, F <: ConvertFrom[H, S]] = MultiFactory[H, S, Nothing, F]
