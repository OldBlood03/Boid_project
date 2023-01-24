package Exceptions

final case class NonSquareMatrixException(private val message:String = "",
										  private val cause:Throwable = None.orNull
										 ) extends Exception(message,cause)
