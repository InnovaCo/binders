package eu.inn.binders.annotations

import scala.annotation.StaticAnnotation

trait FieldNameMarker
case class fieldName(name: String, applyConverter: Boolean = false) extends StaticAnnotation with FieldNameMarker
