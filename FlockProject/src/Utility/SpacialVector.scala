import scala.annotation.targetName
import scala.math.sqrt
import scala.math.{cos, sin, tan}
import Exceptions.*
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
		@targetName("vectorAdditionAssignment")
		def +=(vec:SpacialVector) =
			X += vec.x
			Y += vec.y
		@targetName("vectorSubtractionAssignment")
		def -=(vec:SpacialVector) =
			X -= vec.x
			Y -= vec.y
		@targetName("matrixMultiplication")
		def *(matrix:SpacialMatrix):SpacialVector =
			SpacialVector(matrix.x1,matrix.y1) * X + SpacialVector(matrix.x2,matrix.y2) * Y
		@targetName("vectorNegation")
		def unary_- : SpacialVector = SpacialVector (-X, -Y)

		def dot(vector:SpacialVector):Double ={
			X * vector.x + Y * vector.y
		}
//-----------------------------------------------------------------------------

		def rotateTo (vecTo: SpacialVector) =
			val angle = tan((vecTo-this).length/this.length)
			val transformedVector = this*SpacialMatrix.rotationMatrix(angle)
			if transformedVector == vecTo then
				transformedVector
			else
				-transformedVector
		// radians: positive -> left, negative -> right
		def rotate (angle: Double): SpacialVector = {
			this*SpacialMatrix.rotationMatrix(angle)
		}
		// tells if the argument vector is within an angle arc of where this
		// is pointing
		def inArc (vec: SpacialVector, angle:Double):Boolean = {

			val arcAnd = () => {
				val left = SpacialVector (this, java.lang.Math.PI/2 - angle/2)
				val right = SpacialVector (this, -java.lang.Math.PI/2 + angle/2)
				((vec dot left) >= 0) && ((vec dot right) >= 0)
			}
			val arcOr = () => {
				val left = SpacialVector (this,-angle/2)
				val right = SpacialVector (this, angle/2)
				((vec dot left) >= 0) || ((vec dot right) >= 0)
			}
			if this.length == 0 then throw IndefiniteVectorException("vector length 0 cannot have an arc in front")
			if angle>=0 && angle<=java.lang.Math.PI then
				arcAnd()
			else if (angle > java.lang.Math.PI && angle <= java.lang.Math.PI*2) then
				arcOr()
			else
				throw UnexpectedAngleException("angle of arc has to be between 0 and 360 inclusive")
		}

		override def toString:String = s"($x, $y)"
	}
	object SpacialVector:
		def apply (X:Double, Y:Double) = {
			new SpacialVector(X,Y)
		}
		def apply (copy: SpacialVector, offsetAngle:Double) = {
			copy.rotate(offsetAngle)
		}