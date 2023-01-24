package Exceptions

final case class UnexpectedAngleException(private val message:String = "",
										  private val cause:Throwable = None.orNull
										 ) extends Exception(message,cause)
