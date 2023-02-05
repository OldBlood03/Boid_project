import Behaviour.*
import Utility.*

import scala.collection.mutable.Buffer
import scala.util.Random

package main_stuff:

	final class Game:
		// viewport
		val width = 1000
		val height = 600
		// padding for the boids to turn around
		val margin = 10

		// max speed of bird
		val speedLimit = 10
		// in radians
		val FOV = math.Pi
		// arbitrary range of view
		private val range = 40

		private val numberOfBoids = 200

		// how aggressively the birds gravitate towards the center of their local group
		private var centeringFactor = 0.1
		// how xenophobic the birds are
		private var separationFactor = 0.05
		// minimum distance after which birds start avoiding each other
		private val turnFactor = speedLimit/10
		private var minDistance = 20

		private val random = (x:Double) => {
			Random.nextDouble() * x
		}
		// initialize the boids in random positions and random velocities in bounds
		private val boids = Array.fill[Boid](numberOfBoids)(Boid(SpacialVector(random(width),random(height)),SpacialVector(random(5)+1 ,random(5)+1 )))

		// field of view
		private def inFOV(of:Boid,target:Boid) = of.heading.inArc(target.pos - of.pos,FOV)

		private def distance(boid:Boid,other:Boid) ={
			(boid.pos - other.pos).length
		}
		// this is primarily used for visualization: to draw lines between boids that are in range
		private def rangeNeighbours (boid:Boid):Array[Boid] = for i <- boids if distance(boid, i) <= range yield i

		// this is primarily used for visualization: to draw lines between boids that are in fov
		private def FOVNeighbours (boid:Boid):Array[Boid] = for i <- boids if inFOV(boid, i) yield i

		// neighbours that are both in fov and in range of sight
		private def neighboursInView (boid:Boid):Array[Boid] =
			for i <- boids if inFOV(boid, i) && distance(boid, i) <= range yield i

		// Cohesion: move towards the centre of every boid in sight
		private def moveTowardsCenter (boid:Boid) = {
			var dv = SpacialVector(0,0)
			val neighbours = neighboursInView (boid)
			for i:Boid <- neighbours do
				dv += (i.pos-boid.pos)*centeringFactor
			if (neighbours.length > 0) then
				boid.shiftVelocity(dv/neighbours.length)
		}

		// Separation: don't get too close to other boids
		private def separate (boid: Boid) = {
			var dv = SpacialVector (0,0)
			val neighbours = rangeNeighbours(boid)
			for i:Boid <- neighbours if distance(boid,i) < minDistance do
				dv += (boid.pos-i.pos)*separationFactor
			boid.shiftVelocity(dv)
		}

		// Alignment: look at neighbours and go at the average velocity
		private def matchVelocity (boid: Boid) = {
			var average = SpacialVector (0,0)
			val neighbours = neighboursInView(boid)
			for i:Boid <- neighbours do
				average += i.velocity
			if neighbours.length >0 then
				boid.shiftVelocity((average/neighbours.length) - boid.velocity)
		}

		private def limitSpeed (boid:Boid) = {
			if (boid.velocity.length > speedLimit) then
				boid.shiftVelocity(-boid.velocity + boid.velocity.direction*speedLimit)
		}

		private def keepWithinBounds(boid:Boid) ={
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
			matchVelocity(boid)
			moveTowardsCenter(boid)
			separate(boid)
			limitSpeed(boid)
			keepWithinBounds(boid)
			boid.updatePosition()
		}

	end Game

	import javafx.scene.control.Button
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
	import scalafx.scene.layout._
	import scalafx.scene.control._

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
			border.children += new Button("sdadasdasdas")
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