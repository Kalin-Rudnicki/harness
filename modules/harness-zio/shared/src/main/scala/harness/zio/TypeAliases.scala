package harness.zio

import zio.stream.*

// =====| Missing Types for ZStream |=====

// format: off
// TODO (KR) : REMOVE if ZIO adds these 
type    RStream[-R, +A] = ZStream[R,   Throwable, A]
type   URStream[-R, +A] = ZStream[R,   Nothing,   A]
type TaskStream[    +A] = ZStream[Any, Throwable, A]
// format: on
