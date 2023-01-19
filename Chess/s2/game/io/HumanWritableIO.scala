package s2.game.io

import s2.game.*
import java.io.{BufferedReader, IOException, Reader}

/** ********************************************************************
  *
  * This file is returned in Exercise 3.2
  *
  * The idea is to read a file written manually by someone. There can be useless whitespace all around the file.
  */

object HumanWritableIO:

  def loadGame(input: Reader): Game =

    /** This is the game object this method will fill with data. The object is returned when the file ends and
      * everything is ok.
      */

    val board = Board()
    val game = Game(board)

    /*
    You might want to keep track of different required parts of the file
     if some of these are missing, the file is not valid and a CorruptedChessFileException should be thrown
     If you figure out something better, you don't have to keep these variables.
     */
    var infoRead = false
    var whiteRead = false
    var blackRead = false

    // BufferedReader allows us to read line by line (readLine method)
    val lineReader = BufferedReader(input)

    try

      /*
       * Read the first line, i.e. the header.
       * You can also use this variable for reading all the section headers,
       * or you can do better and use only vals and get rid of this.
       */

      var currentLine = lineReader.readLine().trim.toLowerCase

      // Process the header we just read.
      // NOTE: To test the line below you must test the class once with a
      // broken header

      if !((currentLine.startsWith("chess")) && (currentLine.endsWith("save file"))) then
        throw new CorruptedChessFileException("Unknown file type")

      // The version information and the date are not used in this
      // exercise

      // *************************************************************
      //
      // EXERCISE
      //
      // ADD CODE HERE FOR READING THE
      // DATA FOLLOWING THE MAIN HEADERS
      //
      //
      // *************************************************************

      // If we reach this point the Game-object should now have the proper
      // players and
      // a fully set up chess board. Therefore we might as well return it.

      // If there is something missing, throw a CorruptedChessFileException

      game

    catch
      case e: IOException =>
        // To test this part the stream would have to cause an
        // IOException. That's a bit complicated to test. Therefore we have
        // given you a "secret tool", class BrokenReader, which will throw
        // an IOException at a requested position in the stream.
        // Throw the exception inside any chunk, but not in the chunk
        // header.

        val chessExc = CorruptedChessFileException("Reading the chess data failed.")

        // Append the information about the initial cause for use in
        // debugging. Otherwise the programmer cannot know the method or
        // line number causing the problem.

        chessExc.initCause(e)

        throw chessExc
    end try
  end loadGame
end HumanWritableIO
