import Behaviour.*
import Utility.*

import scala.collection.mutable.Buffer
import scala.util.Random

package main_stuff:
	import scalafx.animation.AnimationTimer
	import scalafx.application.JFXApp3
	import scalafx.geometry.Insets
	import scalafx.scene.Scene
	import scalafx.scene.effect.DropShadow
	import scalafx.scene.layout.{BorderPane, HBox, GridPane}
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
		}
		stage.resizable = false
		stage.scene = new Scene {
			val border = new GridPane()
			val leftBar = new VBox
			val bottomBar = new HBox
			fill = Color.rgb(38, 38, 38)
			val canvas = new Canvas(game.width, game.height)
			val gc = canvas.graphicsContext2D
			val button = new Button("I am button")
			val toggleDonut = new CheckBox("Torus space")
			leftBar.children += button
			leftBar.children += toggleDonut
			button.onMouseClicked = (event) => println ("test")
			root = border
			border.add(leftBar,0,0,1,1)
			border.add(canvas, 1,0,1,1)
			border.columnConstraints = makeColumnConstraints(10,90)

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
	def makeColumnConstraints (c:Int*):Array[ColumnConstraints] = {
		c.map(x => new ColumnConstraints {percentWidth = x}).toArray
	}
	def makeRowConstraints (r:Int*):Array[RowConstraints] = {
		r.map(x => new RowConstraints {percentHeight = x}).toArray
	}
}
