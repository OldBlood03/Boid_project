package s2.game.io

import s2.game.*

import java.io.{BufferedReader, IOException, InputStreamReader, Reader}
import scala.collection.mutable.Buffer

/** ********************************************************************
  *
  * This file is returned in chapter 15.4, in exercises ChunkIO 1, 2 and 3.
  *
  * The idea is to read a file written automatically by a program. This is a chunked format like PNG, TIF, MPEG, etc.
  * explained in the exercise description
  *
  * In ChunkIO 1, you only need to traverse through the file and extract the data portions of the Player block (PLR) and
  * the Comment block (CMT). These are the characters that follow the chunk tag and the block size. E.g. for a player
  * block "PLR17B5MarkoKa4Ra6b3c3" the data portion would be "B5MarkoKa4Ra6b3c3". Use the playerChunks and commentString
  * variables inside loadGame to store the results.
  *
  * In ChunkIO 2, you will implement a working version of the loadGame method. However the method only needs to work on
  * correctly formatted files. Here you only need to handle the player blocks as they are the only thing that contains
  * the game information. You can use helper methods to separate the work into smaller subtasks and you can check the
  * material for some suggestions.
  *
  * ChunkIO 3 adds in the Unknown or Unused blocks and handling Broken Files.
  *
  * You have to read through unknown blocks, but you don't have to interact with them further. If someone would want to
  * update this application later with this feature, they could easily add a new block and simply add a feature to the
  * code associated with that block without having to worry about whether the program can read bit or not.
  *
  * For broken files, if the method encounters exceptions in the file structure, it should throw a
  * CorruptedChessFileException. The material details what kind of problems you should expect so here you should adjust
  * your code to deal with these erroneous situations. The broken files tests require that your program works also with
  * correctly formatted input so doing exercise ChunkIO 2 before this one is advised.
  *
  * For each exercise, you will always submit the same file, but each exercise has different requirements as described
  * above and further in the materials, meaning that you only get points if you fulfil the requirements of that
  * particular exercise.
  *
  * Note: ChunkIO 1 & 2 use correctly formatted files, so you have to worry about broken files only in ChunkIO 3.
  */

object ChunkIO:

  /** */
  private var prevPlayerData = collection.mutable.Seq[String]()
  private var prevCommentData: Option[String] = None

  /** Processes a data file into a game object.
    *
    * @param input
    *   a reader (or stream) to a data file
    */
  def loadGame(input: Reader): Game =

    /** This is the game object this method will fill with data. The object is returned when the END chunk is reached.
      */

    val board = Board()
    val game = Game(board)

    /*
     * Use these variables for reading all the file header, date and chunk headers.
     *
     * HINT: check the helper methods in the end of this class, a few lines below we read the header
     *       as an example
     */

    var header = new Array[Char](8)
    var date = new Array[Char](8)
    var chunkHeader = new Array[Char](5)

    try
      Helpers.readFully(header, input);
      Helpers.readFully(date, input);
      Helpers.readFully(chunkHeader, input)
      if !header.mkString.startsWith("CHESS") then throw CorruptedChessFileException("Unknown file type");
      // Read the file header and the save date
      def chunkRead (buff:Buffer[(String,String)]) : Buffer[(String,String)] = {
        // one chunk as a string
        val chunk = chunkHeader.mkString
        if !chunk.contains("END") then
          // the contents of said chunk as an array
          val contents = new Array [Char](Helpers.extractChunkSize(chunkHeader))
          // reading the contents
          Helpers.readFully(contents, input)
          // reading the next header
          Helpers.readFully(chunkHeader, input)
          // adding the contents to the buffer
          buff += chunk->contents.mkString
          // reading the next chunk
          chunkRead(buff)
        buff
      }
      // collecting all the chunks as header -> content pairs
      val chunks = chunkRead(Buffer[(String, String)]())
      val players = chunks.groupBy(_._1.toUpperCase.contains("PLR"))(true)
      // finding if there are the correct number of players
      val enoughPlayers :Boolean = players.size == 2
      // making a collection of pairs of color -> player name
      val playerColorName = players.map(x =>
        (if x._2.head == 'W' then
          White
        else if x._2.head == 'B' then
          Black
        else
          throw CorruptedChessFileException("Unexpected player color"))
        // drop the color and name length and take the name number of characters from the string
        -> x._2.drop(2).take(x._2(1).asDigit))
      if !enoughPlayers || !playerColorName.exists(_._1 == White) || !playerColorName.exists(_._1 == Black) then
        throw CorruptedChessFileException("Unexpected players");
      playerColorName.foreach(x=>game.addPlayer(Player(x._2, x._1)))

      // working on the pieces
      val PlayerAndPiece = players.map(x=>((if x._2(0) == 'B' then game.getBlack else game.getWhite),x._2.drop(x._2(1).asDigit + 2)))
      // method that uses a regex to match all the pieces and returns them in a buffer
      def findPieces (player: Player,input:String): Buffer[(Piece, Int,Int)] = {
        // purposefully making the regex broader so that errors in the file format can cause exceptions
        val regex = """([A-X])?([a-x])([1-8])""".r.unanchored
        val matches = regex.findAllMatchIn(input)
        (for m <- matches yield (m.group(1),m.group(2),m.group(3))).map(x=>
          (x._1 match
            case "K" => King(player)
            case "Q" => Queen(player)
            case "R" => Rook(player)
            case "B" => Bishop(player)
            case "N" => Knight(player)
            case null => Pawn(player)
            case a => throw CorruptedChessFileException ("No such piece")
          , if x._2(0).toInt - 96 <= 8 && x._2(0).toInt - 96 > 0 then
            x._2(0).toInt - 96
          else
            throw CorruptedChessFileException ("no such position")
            , x._3(0).asDigit )).toBuffer

      }
      // set the game pieces on the board
      PlayerAndPiece.foreach(x=>findPieces(x._1.getOrElse(throw CorruptedChessFileException("Unexpected players")),x._2).foreach(y=>game.board.setPiece(y._1, y._2-1, y._3-1)))
      val playerChunks = players.map(_._2).toBuffer
      var commentString: Option[String] = chunks.find(_._1.toUpperCase.contains("CMT")).map(_._2)
      // save the player and comment data in global variables for testing
      this.prevPlayerData = playerChunks
      this.prevCommentData = commentString

      game;

    catch
      case e: IOException =>
        // To test this part the stream would have to cause an
        // IOException. That's a bit complicated to test. Therefore we have
        // given you a "secret tool", class BrokenReader, which will throw
        // an IOException at a requested position in the stream.
        // Throw the exception inside any chunk, but not in the chunk header.

        val chessExc = CorruptedChessFileException("Reading the chess data failed.")

        // Append the information about the initial cause for use in
        // debugging. Otherwise the programmer cannot know the method or
        // line number causing the problem.

        chessExc.initCause(e)

        throw chessExc
    end try
  end loadGame

  /** Method used in testing to check that player data was read correctly
    */
  def getPrevPlayerData = this.prevPlayerData

  /** Method used in testing to check that comment data was read correctly
    */
  def getPrevCommentData = this.prevCommentData

  object Helpers:
    // HELPER METHODS -------------------------------------------------------

    /** Given a chunk header (an array of 5 chars) will return the size of this chunks data.
      *
      * @param chunkHeader
      *   a chunk header to process
      * @return
      *   the size of this chunks data
      */

    def extractChunkSize(chunkHeader: Array[Char]): Int = 10 * chunkHeader(3).asDigit + chunkHeader(4).asDigit

    /** Given a chunk header (an array of 5 chars) will return the name of this chunk as a 3-letter String.
      *
      * @param chunkHeader
      *   a chunk header to process
      * @return
      *   the name of this chunk
      */
    def extractChunkName(chunkHeader: Array[Char]): String = chunkHeader.take(3).mkString

    /** The read-method of the Reader class will occasionally read only part of the characters that were requested. This
      * method will repeatedly call read to completely fill the given buffer. The size of the buffer tells the algorithm
      * how many bytes should be read.
      *
      * @param result
      *   The result of the reading will be stored in this array.
      * @param input
      *   The character stream to read from
      * @throws IOException
      * @throws CorruptedChessFileException
      */
    def readFully(result: Array[Char], input: Reader) =
      var cursor = 0

      while cursor != result.length do
        var numCharactersRead = input.read(result, cursor, result.length - cursor)

        // If the file end is reached before the buffer is filled
        // an exception is thrown.

        if numCharactersRead == -1 then throw CorruptedChessFileException("Unexpected end of file.")

        cursor += numCharactersRead
      end while
    end readFully
  end Helpers
end ChunkIO

@main def test () =
  val ag = ChunkIO.loadGame(BufferedReader(InputStreamReader(System.in)))
