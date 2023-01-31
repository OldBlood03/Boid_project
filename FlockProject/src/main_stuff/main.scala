import Behaviour.*
import Utility.*

import scala.collection.mutable.Buffer
import scala.util.Random

package main_stuff:

	final class Game:
		// viewport
		val width = 500
		val height = 600
		// padding for the boids to turn around
		val margin = 300


		// max speed of bird
		val speedLimit = 50
		// min speed of bird
		val minSpeed = 5
		// in radians
		val FOV = math.Pi
		// arbitrary range of view
		private val range = 100

		private val numberOfBoids = 70

		// how aggressively the birds gravitate towards the center of their local group
		private var centeringFactor = 0.005

		// how xenophobic the birds are
		private var separationFactor = 0.005
		// minimum distance after which birds start avoiding each other
		private var minDistance = 20

		private val random = (x:Double) => {
			Random.nextDouble() * x
		}
		// initialize the boids in random positions and random velocities in bounds
		private val boids = Array.fill[Boid](numberOfBoids)(Boid(SpacialVector(random(width),random(height)),SpacialVector(random(30)+1 ,random(30)+1 )))

		// field of view
		private def inFOV(of:Boid,target:Boid) = of.heading.inArc(target.pos - of.pos,FOV)

		private def distance(boid:Boid,other:Boid) ={
			(boid.pos - other.pos).length
		}
		// this is primarily used for visualization: to draw lines between boids that are in range
		private def rangeNeighbours (boid:Boid):Array[Boid] = for i <- boids if distance(boid, i) <= range yield i

		// this is primarily used for visualization: to draw lines between boids that are in fov
		private def FOVNeighbours (boid:Boid):Array[Boid] = for i <- boids if inFOV(boid, i) yield i

		private def influencingNeighbours (boid:Boid):Array[Boid] =
			for i <- boids if inFOV(boid, i) && distance(boid, i) <= range yield i

		private def moveTowardsCenter (boid:Boid) = {
			var dv = SpacialVector(0,0)
			val neighbours = influencingNeighbours (boid)
			for i:Boid <- neighbours do
				dv += (i.pos-boid.pos)*centeringFactor
			if (neighbours.length > 0) then
				boid.shiftVelocity(dv/neighbours.length)
		}

		private def separate (boid: Boid) = {
			var dv = SpacialVector (0,0)
			val neighbours = influencingNeighbours (boid)
			for i:Boid <- neighbours if distance(boid,i) < minDistance do
				dv += (boid.pos-i.pos)*separationFactor
			boid.shiftVelocity(dv)
		}
		// look at neighbours and go at the average velocity
		private def matchVelocity (boid: Boid) = {
			var average = SpacialVector (0,0)
			val neighbours = influencingNeighbours (boid)
			for i:Boid <- neighbours do
				average += i.velocity
			if neighbours.length >0 then
				boid.shiftVelocity((average/neighbours.length) - boid.velocity)
		}
		private def limitSpeed (boid:Boid) = {
			boid.shiftVelocity(-boid.velocity*math.max(0,boid.velocity.length/speedLimit))
			boid.shiftVelocity(boid.velocity*math.max(0,boid.velocity.length/minSpeed))
		}

		private def keepWithinBounds(boid:Boid) ={
			val turnFactor = 10
			if boid.pos.x < margin then
				boid.shiftVelocity(SpacialVector(turnFactor,0))
			if boid.pos.x > width - margin then
				boid.shiftVelocity(SpacialVector(-turnFactor,0))
			if boid.pos.y < margin then
				boid.shiftVelocity(SpacialVector(0,turnFactor))
			if boid.pos.y > height - margin then
				boid.shiftVelocity(SpacialVector(0,-turnFactor))
		}

		def getBoids = boids

		def update (boid: Boid) = {

			moveTowardsCenter(boid)
			keepWithinBounds(boid)
			limitSpeed(boid)
			separate(boid)
			matchVelocity(boid)
			boid.updatePosition()
		}

	end Game

	import scalafx.animation.AnimationTimer
	import scalafx.application.JFXApp3
	import scalafx.geometry.Insets
	import scalafx.scene.Scene
	import scalafx.scene.effect.DropShadow
	import scalafx.scene.layout.{BorderPane, HBox}
	import scalafx.scene.paint.Color.*
	import scalafx.scene.paint.*
	import scalafx.scene.text.Text
	import scalafx.scene.canvas.Canvas
	import scalafx.scene.input.MouseEvent

	import java.awt.event.MouseEvent

	object ScalaFXHelloWorld extends JFXApp3 {
	val game = Game ()
	override def start(): Unit = {
		stage = new JFXApp3.PrimaryStage {
		  //    initStyle(StageStyle.Unified)
		  title = "Boids"
		  scene = new Scene {
			val border = new BorderPane
			fill = Color.rgb(38, 38, 38)
			val canvas = new Canvas(game.width, game.height)
			val gc = canvas.graphicsContext2D
			border.center = canvas
			root = border
			val timer = AnimationTimer { time =>
				gc.fill = Color.rgb(38, 38, 38)
				gc.fillRect(0,0,game.width, game.height)
				for boid <- game.getBoids do
					game.update(boid)
					gc.fill = Color.LightBlue
					gc.fillOval(boid.pos.x, boid.pos.y, 5, 5)
			}
			timer.start()
		}
		}
	}
	}