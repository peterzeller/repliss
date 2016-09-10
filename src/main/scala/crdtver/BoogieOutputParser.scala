package crdtver


/**
  * Created by peter on 10.09.16.
  */
class BoogieOutputParser {

  private val labelLine = "(.*?)\\(([0-9]+),([0-9]+)\\): (.*?)".r

  def parse(output: String):Unit = {
    for (line <- output.lines) {
      line match {
        case labelLine(file, lineNr, column, message) =>
          println(s"Error in $file in $lineNr:$column >> $message")
        case _ =>
          println(s"no match: $line")
      }

    }

  }

}
