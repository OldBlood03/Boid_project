package s2.kalevala.io

import java.io.*

/** In this exercise you will practice the basics of file handling. You will be handling a part of a
  * Finnish poem from the Finnish folk lore epic Kalevala.
  *
  * We will be using the BufferedReader and the FileReader classes from the java.io package to read
  * the poem line by line. When reading a file this way, it is possible to choose some parts of the
  * file and leave some parts out. In the exercise you will be making different filters to choose
  * only particular lines of the poem in each part.
  *
  * In the exercise there is included the text file of the third poem of the Kalevala epic
  * (kalevala3.txt). The poem consists of lines of the poem separated into verses with an empty
  * line. Thus, from the empty lines you will know that a new verse will start on the following
  * line.
  *
  * In the exercise we want to design filters that will extract only certain lines from the poem and
  * save them into a string sequence Seq[String]. The filter methods takes the BufferedWriter as a
  * parameter which can be used to read through a file line by line.
  *
  * You can take a look at the noFilter method to get an idea how the filter methods are supposed to
  * traverse through the lines of the file.
  *
  * Finally once your filters are working correctly the writeToFile method should write the results
  * of your file reading and filtering to a new file. Here we use FileWriter and BufferedWriter.
  *
  * The writeToFile takes a string sequence as a parameter, which corresponds to the sequence of the
  * filtered lines produced by the filters. This method should simply write the contents of the
  * string sequence to a designated file location.
  *
  * Note: Included in the project, in the "models" folder, are example results of what your filters
  * should produce. You can use these files to compare your output and what the result should look
  * like to help you come up what you might need to change in your solution if the results don't
  * match. Don't make changes to these files as they are used to determine your local unit tests.
  */

object KalevalaReader:
  def alphabet = "abcdefghijklmnopqrstuvxyzåäöABCDEFGHIJKLMNOPQRSTUVXYZÅÄÖ\""
  private val poemFile = "s2/kalevala/io/kalevala3.txt"

  private val targets =
    Array(
      "no_filter.txt",
      "first_line.txt",
      "vainamoinen.txt",
      "every_other.txt"
    ) // File names for the different filters.

  private val filters =
    Seq[(BufferedReader => Seq[String])](
      noFilter,
      firstLineOfEachVerse,
      rowsWithVainamoinen,
      everyOtherRowOfVerses
    ) // All the different filters.

  /** Helper method for reading the file with all the different filters and writing the results into
    * a file.
    */
  def writeAllFilters(
      source: String,
      filters: Seq[BufferedReader => Seq[String]],
      targets: Seq[String] ): Unit =
    for
      (f, t) <- filters zip targets
    do writeToFile(t, readFile(source, f))

  // The for loop above could have been fritten as a foreach as follows
  //
  // filters zip targets).foreach { case (f, t) =>
  //   writeToFile(t, readFile(source, f))
  
  /**
    * This method will process the test file provided and dump the results in the root of the Kalevala project
    */
  @main
  def tryOut() = {
    writeAllFilters(
      "Kalevala/kalevala3.txt",
      filters,
      targets.map("Kalevala/my_" + _)
    )
  }


  /** Write a method that reads a file using a FileReader to read the file and a BufferedReader to
    * read the lines in the file. Use the BufferedReader as a parameter for your FileOperation
    * function.
    *
    * Check for Exceptions in the code. Particularly whether the file was found and whether there
    * was a reading error i.e. IOException.
    *
    * The reader should return a sequence containing each line of the filtered file.
    */
  def readFile(
      sourceFile: String,
      fileOperation: BufferedReader => Seq[String]
  ): Seq[String] = {
   try
       val input = FileReader(sourceFile)
       val bufInput = BufferedReader(input)

       try
        fileOperation(bufInput)
       finally
         input.close()
         bufInput.close()
       end try
   catch
    case notFound: FileNotFoundException => Seq()
    case e: IOException => Seq()
  }
  /** A helper method that shows you how to read from a stream without applying any filtering.
    */
  def noFilter(lineReader: BufferedReader): Seq[String] =
    import collection.mutable.Buffer
    val strBuffer = Buffer[String]()
    var line = lineReader.readLine()
    while (line != null) do {
      strBuffer += line
      line = lineReader.readLine()
    }
    strBuffer.toSeq


  /*
  def noFilter2(lineReader: BufferedReader) =
    var resList = Seq[String]()
    var oneLine: String = lineReader.readLine()

    while oneLine != null do
      resList = resList :+ oneLine
      oneLine = lineReader.readLine()
    resList
  */


  /** Returns the first line of each verse. Here you need to be able to distinguish the separation
    * of two consecutive verses as well as pick only the first line of each of the verses you find.
    * 
    * Note that the name of the poem is not a verse and that verses might be separated by more than one empty line.
    */

  def firstLineOfEachVerse(lineReader: BufferedReader): Seq[String] = {
    val lines = noFilter(lineReader)
    (" " +: lines).zip(lines).map((x,y) => if (x.trim == "") then y else "").filter(_!="").drop(1)
  }

  /** Returns a sequence of the rows that contain the string "Väinämöinen". Make sure that you have
    * the initial letter capitalized.
    */
  def rowsWithVainamoinen(lineReader: BufferedReader): Seq[String] = {

    noFilter(lineReader).filter(_.contains("Väinämöinen")).toSeq
  }

  /** Reads the file and returns a version where every other line in each verse starting from the
    * second line is filtered out. E.g. you keep the lines 1, 3, 5, ... This method keeps the header
    * line.
    *
    * Note: You should keep the line spaces between verses!
    */
  def everyOtherRowOfVerses(lineReader: BufferedReader): Seq[String] = {
    val lines = noFilter(lineReader)
    val buf = collection.mutable.Buffer[String]()
    var indexer = 1
    for i <- lines do
      indexer -= 1
      if (i.trim=="") then
        buf += i
        indexer = 1
      else
        if (indexer == 0) then
          buf += i
          indexer = 2
    buf.toSeq
  }

  /** Create a method for writing the contents of a string sequence into a file.
    *
    * Here you don't have to consider any filters, but simply write each string in the string
    * sequence onto its own line.
    *
    * You can use the FileWriter to access the file and the BufferedWriter to write strings onto
    * separate lines.
    *
    * Again consider exceptions, e.g. whether the file was found and whether there was a reading
    * error i.e. IOException.
    */

  def writeToFile(fileName: String, arr: Seq[String]) = {
    try
      val buffWriter = BufferedWriter(FileWriter(fileName))
      try
        arr.foreach(x => buffWriter.write(x + "\n"))
      finally
        buffWriter.close()
    catch
      case e:FileNotFoundException => throw e
      case e:IOException => throw e
  }

end KalevalaReader

