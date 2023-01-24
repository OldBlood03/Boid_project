import Exceptions.NonUnitVectorException

import scala.math.{cos, sin, sqrt}
package Utility:

	class UnitVector (X: Double, Y: Double) extends SpacialVector(X, Y) {}

	object UnitVector:
		private val accuracy:Double = 0.001
		def getAccuracy = accuracy
		def apply (X:Double, Y:Double): UnitVector = {
			val length = sqrt(X*X + Y*Y)
			if (length > 1.toDouble + accuracy || length < 1.toDouble - accuracy) then
				throw NonUnitVectorException()
			new UnitVector(X,Y)
		}
		// in radians
		def apply (angle:Double): UnitVector = {
			UnitVector(cos(angle),sin(angle))
		}
		def apply (copy: UnitVector, offsetAngle:Double): UnitVector = {
			val vec = copy.rotate(offsetAngle)
			new UnitVector (vec.x, vec.y)
		}

