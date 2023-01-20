package s2.game.io

import collection.mutable.Buffer
import s2.game.*

import scala.util.matching.*
import java.io.{BufferedReader, FileNotFoundException, FileReader, IOException, InputStreamReader, Reader, StreamCorruptedException}

def interpretPiece (piece:String, game:Game, player:Player, column:String, row:String) =
  try
    game.board.setPiece( piece match
      case "king" => King(player)
      case "queen" => Queen(player)
      case "rook" => Rook(player)
      case "bishop" => Bishop(player)
      case "knight" => Knight(player)
      case "pawn" => Pawn(player)
      case _ => throw CorruptedChessFileException("unknown chess piece"), column(0).toInt - 97, row.toInt -1)
    catch
      case a:CorruptedChessFileException => throw a
      case e => throw CorruptedChessFileException("invalid position")


trait IReader(val outerBlock: Regex, val innerBlock: Regex):
  def readToGame(game: Game, line: String): Option[Array[String]]
end IReader

class ReadPlayer extends IReader("""game\W*metadata""".r.unanchored, """\w*(black|white)\W*:\s*(.*)""".r.unanchored) :
  override def readToGame(game: Game, line: String): Option[Array[String]] =
    line match
      case innerBlock(color, name) =>
        game.addPlayer(
          Player(name.takeWhile(x => !"""\w""".r.matches(x.toString)).concat(name.dropWhile(x => !"""\w""".r.matches(x.toString)).split(' ').map(_.capitalize).mkString(" ")),
            if color == "white" && !game.getWhite.isDefined then
              White
            else if color == "black" && !game.getWhite.isDefined then
              Black
            else
              throw CorruptedChessFileException("too many players of same color")))
    None

class ReadDate extends IReader("""game\W*metadata""".r.unanchored, """date:(\d|\d{2})\.(\d|\d{2})\.(\d{4})""".r) :
  override def readToGame(game: Game, line: String): Option[Array[String]] =
    line match
      case innerBlock(day, month, year) => //implement what you want for processing the date
    None
/* The inner block regex is shitty because it includes more than it should: incorrect inputs should be ignored
 for backwards compatability, but there is a test that requires you to throw an exception for an incorrect position
 so what do I know */
class ReadWhitePiece extends IReader("""white""".r, """([kqbrpn]\w*).*:.*([a-k]).*([1-8])""".r) :
  override def readToGame(game: Game, line: String): Option[Array[String]] =
    val player = game.getWhite.getOrElse(throw CorruptedChessFileException("player not defined") )
    line match
      case innerBlock(piece, column, row) => interpretPiece(piece, game, player, column, row)
    None

class ReadBlackPiece extends IReader("""black""".r, """([kqbrpn]\w*)\s*:\s*([a-k])\s*([1-8])""".r) :
  override def readToGame(game: Game, line: String): Option[Array[String]] =
   val player = game.getBlack.getOrElse(throw CorruptedChessFileException("player not defined") )
    line match
      case innerBlock(piece, column, row) =>
        interpretPiece(piece, game, player, column, row)
    None


object HumanWritableIO:

  def loadGame(input: Reader): Game =

    def readTrimmed(lineReader: BufferedReader) = {
      val line = lineReader.readLine()
      if line != null then
        line.trim.toLowerCase
      else
        line
    }
    val readerBuffer = Buffer[IReader]()

    readerBuffer += ReadPlayer()
    readerBuffer += ReadDate()
    readerBuffer += ReadWhitePiece()
    readerBuffer += ReadBlackPiece()

    val board = Board()
    val game = Game(board)

    // BufferedReader allows us to read line by line (readLine method)
    val lineReader = BufferedReader(input)

    try

      var line = readTrimmed(lineReader)
      while line == "" do
        line = readTrimmed(lineReader)
      if !((line.startsWith("chess")) && (line.endsWith("save file"))) then
        throw new CorruptedChessFileException("Unknown file type")
      line = readTrimmed(lineReader)
      val blockRegex = """\w*\#(.*)""".r
      var currentBlock = ""
      val pastBlocks = Buffer[String]()
      while line != null do
        readerBuffer.foreach(x => if (x.outerBlock.matches(currentBlock) && x.innerBlock.matches(line)) then x.readToGame(game,line))
        line match
          case blockRegex (block) =>
            currentBlock = block
            pastBlocks += block
          case _ =>
        line = readTrimmed(lineReader)
      if !pastBlocks.exists("""white""".r.matches(_)) || !pastBlocks.exists("""black""".r.matches(_)) then
        throw CorruptedChessFileException("Missing block")

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