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
	import scalafx.scene.layout.{BorderPane, HBox}
	import scalafx.scene.paint.Color.*
	import scalafx.scene.paint.*
	import scalafx.scene.text.Text
	import scalafx.scene.canvas.Canvas
	import scalafx.scene.input.MouseEvent

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