
package Behaviour:
	import Utility.*

	import scala.util.Try
	case class Boid (var pos:SpacialVector,var velocity:SpacialVector, var heading:SpacialVector):
		def updatePosition () = pos += velocity
		def shiftVelocity (vec: SpacialVector) =
			velocity += vec
			updateHeading ()
		private def updateHeading () = if (velocity.length != 0) then heading = velocity
