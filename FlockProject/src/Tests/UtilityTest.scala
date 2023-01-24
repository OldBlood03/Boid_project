package Tests:
	import Exceptions.NonUnitVectorException
	import Utility.*
	import org.scalatest.flatspec.AnyFlatSpec
	import org.scalatest.matchers.should.*

	class UtilityTest extends AnyFlatSpec with Matchers{
		"Correct unit vector" should "not throw an exception" in {
			noException should be thrownBy( UnitVector(0,1))
		}
		it should "have correct length" in {
			 UnitVector(1/math.sqrt(2) ,1/math.sqrt(2)).length shouldBe 1.toDouble+- UnitVector.getAccuracy
		}
		it should "convert to SpacialVector when any method is calld" in {
			val vec =  UnitVector (0,1)
			(vec+vec).getClass.getName shouldEqual (classOf[ SpacialVector].getName)
			(vec-vec).getClass.getName shouldEqual (classOf[ SpacialVector].getName)
			(vec/2).getClass.getName shouldEqual (classOf[ SpacialVector].getName)
			(vec*2).getClass.getName shouldEqual (classOf[ SpacialVector].getName)
		}
		it should "be correctly initialized from an angle" in {
			val vec =  UnitVector (java.lang.Math.PI)
			val accuracy =  UnitVector.getAccuracy
			vec.length shouldEqual(1.toDouble+-accuracy)
			vec.y shouldEqual(0.toDouble +-accuracy)
			vec.x shouldEqual(-1.toDouble +-accuracy)
		}
		"An incorrect Unit vector" should "result in a NonUnitVectorException" in {
			assertThrows[Exceptions.NonUnitVectorException] ( UnitVector(1,1))
		}
		"two unit vectors" should "have a negative dot if they face away" in {
			val vec =  UnitVector (java.lang.Math.PI)
			val vec2 =  UnitVector (0)
			assert((vec dot vec2) < 0)
		}
		"a SpacialVector" should "be correctly rotated by the rotate method" in {
			val vec =  SpacialVector (10,0)
			vec.rotate(java.lang.Math.PI).x shouldBe(-10.toDouble +- UnitVector.getAccuracy)
			vec.rotate(java.lang.Math.PI/2).y shouldBe(10.toDouble +- UnitVector.getAccuracy)
			vec.rotate(-java.lang.Math.PI/2).y shouldBe(-10.toDouble +- UnitVector.getAccuracy)
		}
		it should "correctly use the addition assignment and subtraction assignment operators" in {
			val vec =  SpacialVector (10,0)
			vec += SpacialVector(1,1)
			vec.x shouldEqual (11)
			vec.y shouldEqual (1)
			vec -= SpacialVector(1,1)
			vec.x shouldEqual (10)
			vec.y shouldEqual (0)
		}
		"A unit vector" should "be correctly rotated when constructed through the rotate apply overload" in {
			val vec =  UnitVector (java.lang.Math.PI)
			vec.x shouldEqual  (-1.toDouble +-  UnitVector.getAccuracy)
			val vec2 =  UnitVector(vec, -java.lang.Math.PI/2)
			vec2.y shouldEqual (1.toDouble +-  UnitVector.getAccuracy)
			vec2.x shouldEqual (0.toDouble +-  UnitVector.getAccuracy)
		}

		"SpacialVector.inArc" should "return true for edge cases" in {
			val vec =  SpacialVector (10,0)
			val testVec = SpacialVector (math.sqrt(2),math.sqrt(2))
			vec.inArc(testVec, java.lang.Math.PI/2) shouldEqual(true)
		}
		it should "return false for outside the arc" in {
			val vec =  SpacialVector (10,0)
			val testVec = SpacialVector (-1,10)
			vec.inArc(testVec, java.lang.Math.PI/2) shouldEqual(false)
		}
		it should "have correct arc angle" in {
			val vec =  SpacialVector (10,0)
			var testVec = SpacialVector (math.sqrt(2),-math.sqrt(2))
			vec.inArc(testVec, java.lang.Math.PI/2) shouldEqual(true)
			testVec = SpacialVector (-1,-10)
			vec.inArc(testVec, java.lang.Math.PI/2) shouldEqual(false)
		}
		it should "throw an exception when angle is too big or small" in {
			val vec =  SpacialVector (10,0)
			var testVec = SpacialVector (0,10)
			assertThrows[Exceptions.UnexpectedAngleException]( vec.inArc(testVec, java.lang.Math.PI*2.1))
			assertThrows[Exceptions.UnexpectedAngleException]( vec.inArc(testVec, -1))

		}

		it should "accept any angle at 2pi arc" in {
			val vec =  SpacialVector (10,0)
			var testVec = SpacialVector (0,10)
			vec.inArc(testVec, java.lang.Math.PI*2) shouldEqual(true)
			testVec = SpacialVector (0,-10)
			vec.inArc(testVec, java.lang.Math.PI*2) shouldEqual(true)
			testVec = SpacialVector (-1,-10)
			vec.inArc(testVec, java.lang.Math.PI*2) shouldEqual(true)
		}

	}
