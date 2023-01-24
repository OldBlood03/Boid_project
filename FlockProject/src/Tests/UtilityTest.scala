package Tests:
	import Exceptions.NonUnitVectorException
	import Utility.SpacialVector
	import org.scalatest.flatspec.AnyFlatSpec
	import org.scalatest.matchers.should.*

	class UtilityTest extends AnyFlatSpec with Matchers{
		"Correct unit vector" should "not throw an exception" in {
			noException should be thrownBy(Utility.UnitVector(0,1))
		}
		it should "have correct length" in {
			Utility.UnitVector(1/math.sqrt(2) ,1/math.sqrt(2)).length shouldBe 1.toDouble+-Utility.UnitVector.getAccuracy
		}
		it should "convert to SpacialVector when any method is calld" in {
			val vec = Utility.UnitVector (0,1)
			(vec+vec).getClass.getName shouldEqual (classOf[Utility.SpacialVector].getName)
			(vec-vec).getClass.getName shouldEqual (classOf[Utility.SpacialVector].getName)
			(vec/2).getClass.getName shouldEqual (classOf[Utility.SpacialVector].getName)
			(vec*2).getClass.getName shouldEqual (classOf[Utility.SpacialVector].getName)
		}
		it should "be correctly initialized from an angle" in {
			val vec = Utility.UnitVector (java.lang.Math.PI)
			val accuracy = Utility.UnitVector.getAccuracy
			vec.length shouldEqual(1.toDouble+-accuracy)
			vec.y shouldEqual(0.toDouble +-accuracy)
			vec.x shouldEqual(-1.toDouble +-accuracy)
		}
		"An incorrect Unit vector" should "result in a NonUnitVectorException" in {
			assertThrows[Exceptions.NonUnitVectorException] (Utility.UnitVector(1,1))
		}

	}
