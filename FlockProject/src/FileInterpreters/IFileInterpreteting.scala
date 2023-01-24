package FileInterpreters

import scala.util.matching.Regex

sealed trait IFileInterpreteting (block: Regex,value:Regex, interpreter){

}
object testing:
	println("hello")
	def tes = None
@main def test()={
	testing.tes
}