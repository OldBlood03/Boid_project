package Exceptions

final case class NonUnitVectorException(private val message:String = "",
										private val cause:Throwable = None.orNull
									   ) extends Exception(message,cause)
