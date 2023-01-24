import Behaviour.*
import Utility.*

import scala.collection.mutable.Buffer
import scala.util.Random

package main_stuff:

	final class Game:
		// viewport width
		val width = 200

		// viewport height
		val height = 150
		// max speed of bird
		val maxSpeed = 10

		val numberOfBoids = 15
		// in radians
		val FOV = math.Pi
		// arbitrary range of view
		val range = 10

		// how aggressively the birds gravitate towards the center of their local group
		var centeringFactor = 0.005

		// how xenophobic the birds are
		var separationFactor = 0.005
		// minimum distance after which birds start avoiding each other
		var minDistance = 20

		val random = (x:Double) => {
			Random.nextDouble() * x
		}
		// initialize the boids in random positions and random velocities in bounds
		val boids = Array.fill[Boid](numberOfBoids)(Boid(SpacialVector(random(width),random(height)),SpacialVector(random(1)+1 ,random(1)+1 )))

		// field of view
		def inFOV(of:Boid,target:Boid) = of.heading.inArc(target.pos - of.pos,FOV)

		def distance(boid:Boid,other:Boid) ={
			(boid.pos - other.pos).length
		}

		def keepWithinBounds(boid:Boid) ={
			val margin = 200
			val turnFactor = 1
			if boid.pos.x < margin then
				boid.shiftVelocity(SpacialVector(turnFactor,0))
			if boid.pos.x > width - margin then
				boid.shiftVelocity(SpacialVector(-turnFactor,0))
			if boid.pos.y < margin then
				boid.shiftVelocity(SpacialVector(0,turnFactor))
			if boid.pos.y > height - margin then
				boid.shiftVelocity(SpacialVector(0,-turnFactor))
		}

		// this is primarily used for visualization: to draw lines between boids that are in range
		def rangeNeighbours (boid:Boid):Array[Boid] = for i <- boids if distance(boid, i) <= range yield i

		// this is primarily used for visualization: to draw lines between boids that are in fov
		def FOVNeighbours (boid:Boid):Array[Boid] = for i <- boids if inFOV(boid, i) yield i

		def influencingNeighbours (boid:Boid):Array[Boid] =
			for i <- boids if inFOV(boid, i) && distance(boid, i) <= range yield i

		def moveTowardsCenter (boid:Boid) = {
			var dv = SpacialVector(0,0)
			val neighbours = influencingNeighbours (boid)
			for i:Boid <- neighbours do
				dv += (i.pos-boid.pos)*centeringFactor
			if (neighbours.length > 0) then
				boid.shiftVelocity(dv/neighbours.length)
		}

		def separate (boid: Boid) = {
			var dv = SpacialVector (0,0)
			val neighbours = influencingNeighbours (boid)
			for i:Boid <- neighbours if distance(boid,i) < minDistance do
				dv += (boid.pos-i.pos)*separationFactor
			boid.shiftVelocity(dv)
		}
		// look at neighbours and go at the average velocity
		def matchVelocity (boid: Boid) = {
			var average = SpacialVector (0,0)
			val neighbours = influencingNeighbours (boid)
			for i:Boid <- neighbours do
				average += i.velocity
			if neighbours.length >0 then
				boid.shiftVelocity((average/neighbours.length) - boid.velocity)
		}

		def limitSpeed (boid:Boid) = {
			boid.shiftVelocity(-boid.velocity*math.max(0,boid.velocity.length/maxSpeed))
		}

	end Game