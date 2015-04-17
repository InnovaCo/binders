package eu.inn.binders.core

class BindersException(val msg: String, cause: Throwable = null) extends RuntimeException(msg, cause)
