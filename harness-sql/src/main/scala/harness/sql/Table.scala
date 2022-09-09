package harness.sql

abstract class Table
object Table {
  
  trait Companion[T[_[_]] <: Table] {
    
    final type Id = T[shapeless3.deriving.Id]
    
    implicit val tableInfo: TableInfo[T]
    
  }
  
}
