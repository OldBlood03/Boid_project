import scala.annotation.targetName
import scala.math.sqrt
import scala.math.{cos, sin, tan}
package Utility:

	class SpacialVector (private var X:Double,
						 private var Y:Double){

		def x = X
		def y = Y
		def length = sqrt(X*X+Y*Y)

		// returns a direction (unit) vector
		def direction = SpacialVector(X,Y)/length
//------------------operator overloads-------------------------------
		@targetName("scalarMultiplication")
		def *(scalar:Double):SpacialVector = SpacialVector (X*scalar, Y*scalar)
		@targetName("scalarDivision")
		def /(scalar:Double):SpacialVector = SpacialVector (X/scalar, Y/scalar)
		@targetName("vectorAddition")
		def +(vec:SpacialVector):SpacialVector = SpacialVector (X+vec.x, Y+vec.y)
		@targetName("vectorSubtraction")
		def -(vec:SpacialVector):SpacialVector = SpacialVector(X - vec.x,Y - vec.y)
		@targetName("matrixMultiplication")
		def *(matrix:SpacialMatrix):SpacialVector =
			SpacialVector(matrix.x1,matrix.y1) * X + SpacialVector(matrix.x2,matrix.y2) * Y
		@targetName("vectorNegation")
		def unary_- : SpacialVector = SpacialVector (-X, -Y)
//-----------------------------------------------------------------------------

		def rotateTo (vecTo: SpacialVector) =
			val angle = tan((vecTo-this).length/this.length)
			val potentialMatrix = SpacialMatrix (sin(angle), cos(angle), -sin(angle), cos(angle))
			val transformedVector = this*potentialMatrix
			if transformedVector == vecTo then
				transformedVector
			else
				-transformedVector

		override def toString:String = s"($x, $y)"
	}