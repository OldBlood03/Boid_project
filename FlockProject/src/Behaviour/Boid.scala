package Behaviour:
	import Utility.*
	class Boid (val position:SpacialVector, val body:UnitVector) {
			// code in these three:
			//  1. separation
				// define a cone of vision
					// do this with a sweeping raycast from the tip of its alignment vector
			//  2. alignment
			//  3. cohesion
			// each boid follows these rules for the emergent behaviour that matches
			// flocking in the real world
			// follow the black box & open-closed principles
		}
	object Boid:
		def apply (position:SpacialVector, body:UnitVector) = {
			new Boid(position, body)
		}
		def apply (position:SpacialVector, angle:Double) = {
			new Boid(position, UnitVector(angle))
		}
