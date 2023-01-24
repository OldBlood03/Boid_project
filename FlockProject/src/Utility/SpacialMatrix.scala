import scala.annotation.targetName
import scala.math.{cos, sin}
package Utility:

	// the constructor argument is a seq of seq of double. That is columns and rows in order
	class SpacialMatrix (private val X1:Double,private val Y1:Double,private val X2:Double,private val Y2:Double):
		def x1 = X1
		def x2 = X2
		def y1 = Y1
		def y2 = Y2
	end SpacialMatrix
	object  SpacialMatrix:
		def rotationMatrix (angle:Double) = SpacialMatrix ( cos(angle), sin(angle), -sin(angle), cos(angle))
