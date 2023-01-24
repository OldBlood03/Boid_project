
package Tests:
	import Utility.SpacialVector
	import org.scalatest.flatspec.AnyFlatSpec
	import org.scalatest.matchers.should.*
	import main_stuff.*
	import Behaviour.Boid

	class BehaviourTest extends AnyFlatSpec with Matchers {
		val instance:main_stuff.Game = main_stuff.Game()
		"the boids of a game " should " all have a non-zero heading" in {
			assert(instance.boids.forall (_.velocity.length > 0))
		}

		"the distance method " should " return the correct distance" in {
			val x = Boid (SpacialVector(0,0), SpacialVector(1,0))
			val y = Boid (SpacialVector(1,0), SpacialVector(1,0))
			instance.distance (x,y) shouldEqual (1)
		}
		"The FOVNeighbours method" should "return correct boids and reject incorrec ones" in {
			val boid = Boid (SpacialVector(0,0.5),SpacialVector(0,1))
			val boids = Array (
				Boid (SpacialVector(5,0),SpacialVector(0,1)),
				Boid (SpacialVector(0,0),SpacialVector(0,2)),
				Boid (SpacialVector(0,1),SpacialVector(0,3)),
				Boid (SpacialVector(0,2),SpacialVector(1,1)))

			assert((for i <- boids if instance.inFOV(boid, i) yield i)
				.forall(x=> x.pos.y == 2 || x.pos.y == 1))
		}
	}
