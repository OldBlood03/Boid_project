
import java.io.*

implicit class ImpFileReader (val filePath: String):
  def read (x: Int):Seq[String] = {
    val reader = try BufferedReader(FileReader(filePath))
    catch
      case e:FileNotFoundException =>
        println("unable to find file")
        throw e
    try
      (for i <- 1 to x yield
        reader.readLine()).toSeq
    catch
      case e:IOException =>
        println(e.getMessage())
        throw e

  }
end ImpFileReader
@main def testing() = {
  println("ImpReader.scala" read 2)
}