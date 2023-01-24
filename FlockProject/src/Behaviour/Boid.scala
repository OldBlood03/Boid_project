
package Behaviour:
	import Utility.*

	import scala.util.Try
	import Exceptions.IndefiniteVectorException
	case class Boid (var pos:SpacialVector,var velocity:SpacialVector):
		var heading:SpacialVector = velocity/velocity.length
		def updatePosition () = pos += velocity
		def shiftVelocity (vec: SpacialVector) =
			velocity += vec
			updateHeading ()
		private def updateHeading () = if (velocity.length != 0) then heading = velocity/velocity.length

		override def toString:String = s"pos: ${pos}, velocity: ${velocity}, heading: ${heading}"
	object Boid:
		def apply (pos:SpacialVector, velocity:SpacialVector) = {
			if velocity.length <= 0 then
				throw IndefiniteVectorException ("cannot have a zero velocity vector on instantiation")
			new Boid (pos, velocity)
		}