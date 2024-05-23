package harness.sql.typeclass

implicit def queryEncoderSingleToMany[T]: Conversion[QueryEncoderSingle[T], QueryEncoderMany[T]] = QueryEncoderMany.fromSingle(_)
implicit def queryDecoderSingleToMany[T]: Conversion[QueryDecoderSingle[T], QueryDecoderMany[T]] = QueryDecoderMany.fromSingle(_)
implicit def queryCodecSingleToMany[T]: Conversion[QueryCodecSingle[T], QueryCodecMany[T]] = QueryCodecMany.fromSingle(_)
