
package Behaviour:
	import Utility.*

	import scala.util.{Random, Try}
	import Exceptions.IndefiniteVectorException
	case class Boid (var pos:SpacialVector,var velocity:VelocityVector):
		def updatePosition () = pos += velocity
		def shiftVelocity (vec:SpacialVector) = velocity.shift(vec)

		def veer (strength:Double, edge: UnitVector) =
			shiftVelocity(velocity*SpacialMatrix.rotationMatrix(strength*(edge dot velocity)/
						(velocity.length*edge.length)))

		def heading = velocity.getHeading
		override def toString:String = s"pos: ${pos}, velocity: ${velocity}, heading: ${heading}"
	end Boid

	object Boid:
		def apply (pos:SpacialVector, velocity:VelocityVector) = {
			if velocity.length <= 0 then
				throw IndefiniteVectorException ("cannot have a zero velocity vector on instantiation")
			new Boid (pos, velocity)
		}
	end Boid