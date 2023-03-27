package FileInterpreters
import main_stuff.Game
import Exceptions.FileFormatException
import java.nio.file.{Files,FileSystems}
import scala.collection.JavaConverters._
import scala.util.matching.Regex
import java.io.{File,FileReader, BufferedReader}
import collection.mutable.Buffer
final class BoidFileInterpreter {
	private val fileFormatRegex = """[Bb][Oo][Ii][Dd][Ss]:\s*(\d+)$""".r;
	private val lineRegex = """[Rr][Ee][Gg][Ii][Oo][Nn]:\s*(\d+)\s*(\d+)\s*(\d+)\s*(\d+)\s*(\d+)""".r.unanchored
	// metod to find a list of files from directory and initialize a sequence based on them
	// separate this method out maybe to use it with the wav file decoder
	def findFile (path: String): Seq[File]= {
		val dir = FileSystems.getDefault.getPath(path)
		val files = Files.walk(dir).iterator().asScala.filter(_.getFileName.toString.endsWith(".boid"))
		files.toSeq.map(x=> new File(x.toString))
	}
	def sortFile (files: Seq[File]):Seq[File] = {
		files.sortBy(_.getName().trim.toArray.sum)
	}
	// method to read file and return a game object
	def readFile (file: File):Game = {
		val reader = new BufferedReader(new FileReader (file))
		var numOfBoids = fileFormatRegex.findFirstMatchIn(reader.readLine())
				.getOrElse(throw FileFormatException ("Need header in boids file")).group(1).toInt
		while(reader.ready()) {
			lineRegex.findAllMatchIn(reader.readLine()).toSeq.map( x=> (for m <- (1 to x.groupCount) yield x.group(m)).toSeq)
		}
		Game()
	}
}
