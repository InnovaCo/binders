package eu.inn.binders.annotations

import scala.annotation.StaticAnnotation

case class fieldName(name: String, applyConverter: Boolean = false) extends StaticAnnotation
