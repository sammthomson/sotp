package com.samthomson.tombinator

import java.util.Date

import com.samthomson.tombinator.TomlParser.{ArrayKeyPath, KeyPath, TableKeyPath}
import com.samthomson.tombinator.TomlValue.{DuplicateKeyException, EmptyKeyException, HeterogeneousArrayException}

// TODO: serialization?
sealed trait TomlValue[+T] {
  def value: T
}
object TomlValue {
  class TomlValueException(val message: String)
    extends RuntimeException(message)
  case class HeterogeneousArrayException(a: TomlValue[_], b: TomlValue[_])
    extends TomlValueException(
      s"Array elements must be of the same type: ${a.value}, ${b.value}"
    )
  class KeyException(override val message: String)
    extends TomlValueException(message)
  case class DuplicateKeyException(path: Seq[String])
    extends KeyException(s"Key has already been set: " + path.mkString("."))
  case class EmptyKeyException()
    extends KeyException("Path must be non-empty")
}

case class TomlString(value: String) extends TomlValue[String]
case class TomlLong(value: Long) extends TomlValue[Long]
case class TomlDouble(value: Double) extends TomlValue[Double]
case class TomlDateTime(value: Date) extends TomlValue[Date]

sealed trait TomlBool extends TomlValue[Boolean]
object True extends TomlBool { val value = true }
object False extends TomlBool { val value = false }

sealed trait TomlContainer
case class TomlArray[+T](arr: Seq[TomlValue[T]]) extends TomlValue[List[T]] with TomlContainer {
  // TODO: would be nicer to enforce at compile time than at runtime
  arr match {
    // only need to check Seqs of size >= 2
    // we add one value at a time, so just need to check that first and last type match
    case first +: middle :+ last =>
      (first, last) match {
      case (_: TomlString, _: TomlString) => ()
      case (_: TomlLong, _: TomlLong) => ()
      case (_: TomlDouble, _: TomlDouble) => ()
      case (_: TomlDateTime, _: TomlDateTime) => ()
      case (_: TomlBool, _: TomlBool) => ()
      case (_: TomlArray[_], _: TomlArray[_]) => ()
      case (_: TomlTable, _: TomlTable) => ()
      case _ => throw HeterogeneousArrayException(first, last)
    }
    case _ => ()
  }
  def value = arr.map(_.value).toList
}
object TomlArray {
  val empty = TomlArray(Seq())
}
// TODO: Use shapeless records? hmaps?
case class TomlTable(table: Map[String, TomlValue[Any]])
    extends TomlValue[Map[String, Any]] with TomlContainer{

  def value = table.mapValues(_.value)

  def addKeyPath(pathType: KeyPath, path: Seq[String]): TomlTable = {
    val updatedMap: Map[String, TomlValue[Any]] = path match {
      case Seq() => throw EmptyKeyException()
      case Seq(head, tail @ _*) =>
        val newValue = tail match {
          case Seq() =>
            // final key in the KeyPath.
            // If a value already exists at this location, it must match the KeyPath type.
            val oldValue = table.getOrElse(head, pathType.default)
            (pathType, oldValue) match {
              case (_: ArrayKeyPath, TomlArray(childArray)) => TomlArray(childArray :+ TomlTable.empty)
              case (_: TableKeyPath, childTable: TomlTable) => childTable
              case _ => throw DuplicateKeyException(path)  // path doesn't match existing value
            }
          case Seq(_, _*) =>
            // multiple keys in the KeyPath
            // If a value already exists at this location, it must be a table
            table.getOrElse(head, TomlTable.empty) match {
              case child: TomlTable => child.addKeyPath(pathType, tail)
              case TomlArray(init :+ (last: TomlTable)) =>
                // update the most recent table in the array
                TomlArray(init :+ last.addKeyPath(pathType, tail))
              case _ => throw DuplicateKeyException(path)  // existing value not a table or array of tables
            }
        }
        table.updated(head, newValue)
    }
    TomlTable(updatedMap)
  }

  def assign[T](prefix: Seq[String], ident: String, value: TomlValue[T]): TomlTable = prefix match {
    case Seq() =>
      if (table.contains(ident)) throw DuplicateKeyException(Seq(ident))
      TomlTable(table.updated(ident, value))
    case Seq(head, tail @ _*) =>
      val newValue = table(head) match {
        case child: TomlTable => child.assign(tail, ident, value)
        case TomlArray(init :+ (last: TomlTable)) =>
          // update the most recent table in the array
          TomlArray(init :+ last.assign(tail, ident, value))
        case _ =>
          throw DuplicateKeyException(Seq(head))
      }
      TomlTable(table.updated(head, newValue))
  }
}
object TomlTable {
  val empty = TomlTable(Map())
}
