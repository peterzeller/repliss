package crdtver.language

import org.antlr.v4.runtime._
import org.antlr.v4.runtime.misc.Pair
import java.util

import crdtver.language.ExtendedLexer.ExtendedReplissLexer.State
import crdtver.language.ExtendedLexer.ExtendedReplissLexer.State.{INIT, State}
import crdtver.parser.{LangLexer, LangParser}
import org.antlr.v4.runtime.atn.ATNConfigSet
import org.antlr.v4.runtime.dfa.DFA

import scala.annotation.tailrec
import scala.collection.mutable
import scala.jdk.CollectionConverters.ListHasAsScala


object ExtendedLexer {

  object WLogger {
    def info(str: String): Unit = {
      println(str)
    }

    def trace(str: String): Unit = {
      println(str)
    }
  }

  object ExtendedReplissLexer {

    object State extends Enumeration {
      type State = Value
      val INIT, NEWLINES, BEGIN_LINE = Value
    }


  }

  class ExtendedReplissLexer(val input: CharStream) extends TokenSource {
    final private val orig: LangLexer = new LangLexer(input)
    private val sourcePair: Pair[TokenSource, CharStream] = new Pair[TokenSource, CharStream](orig, input)
    private val indentationLevels: mutable.Stack[Integer] = new mutable.Stack[Integer]
    indentationLevels.push(0)
    private val nextTokens: util.Queue[Token] = new util.LinkedList[Token]
    private var state: State = ExtendedReplissLexer.State.INIT

    private var spacesPerIndent: Int = -(1)
    private var eof: Option[Token] = None
    private var firstNewline: Option[Token] = None
    private var numberOfTabs: Int = 0
    //    private val lineOffsets: LineOffsets = new LineOffsets
    private val debug: Boolean = false
    private var lastCharWasWrap: Boolean = false
    private var lastToken: Option[Token] = None
    // which character is used for indentation
    // counts the number of open parentheses
    private var parenthesesLevel: Int = 0

    override def getCharPositionInLine: Int = {
      orig.getCharPositionInLine
    }

    override def getInputStream: CharStream = {
      orig.getInputStream
    }

    override def getLine: Int = {
      orig.getLine
    }

    override def getSourceName: String = {
      orig.getSourceName
    }

    override def getTokenFactory: TokenFactory[_] = {
      orig.getTokenFactory
    }

    override def nextToken: Token = {
      val t: Token = nextTokenIntern
      lastToken = Some(t)
      if (debug) {
        WLogger.trace("     new token  = " + LangParser.VOCABULARY.getSymbolicName(t.getType) + " '" + t.getText + "'")
      }
      t
    }

    def addErrorListener(errorListener: ANTLRErrorListener): Unit =
      orig.addErrorListener(errorListener)

    private def nextTokenIntern: Token = {
      if (!(nextTokens.isEmpty)) {
        return nextTokens.poll
      }
      val l_eof: Option[Token] = eof
      for (eof <- l_eof) {
        return makeToken(Token.EOF, "$EOF", eof.getStartIndex, eof.getStopIndex)
      }

      //      @tailrec
      def continue(): Token = {
        val token1: Token = orig.nextToken
        if (debug) {
          WLogger.info(s"$state orig token = " + LangParser.VOCABULARY.getSymbolicName(token1.getType) + " '" + token1.getText + "'")
        }
        if (token1 == null) {
          return null
        }
        val token: Token = token1
        if (token.getType == LangParser.NL) {
          var line: Int = 0
          for (i <- 0 until token.getText.length) {
            val c: Char = token.getText.charAt(i)
            if (c == '\n') {
              line += 1
            }
          }
        } else {
          if (token.getType == LangParser.PAREN_LEFT) {
            parenthesesLevel += 1
          } else if (token.getType == LangParser.PAREN_RIGHT) {
            parenthesesLevel -= 1
          } else if (token.getType == Token.EOF) { // at EOF close all blocks and return an extra newline
            handleIndent(0, token, token.getStartIndex, token.getStopIndex, Some(token))
            eof = Some(token)
            // if inside Repliss, add a closing newline
            nextTokens.add(makeToken(LangParser.NL, "$NL", token.getStartIndex, token.getStopIndex))
            // add a single newline
            return makeToken(LangParser.NL, "$NL", token.getStartIndex, token.getStopIndex)
          }
        }
        state match {
          case INIT =>
            if (token.getType == LangParser.NL) {
              if (lastCharWasWrap) { // ignore the newline following a wrap-character
                return continue()
              } else {
                firstNewline = Some(token)
                state(ExtendedReplissLexer.State.NEWLINES)
                return continue()
              }
            } else if (isTab(token)) {
              return continue()
            }
            lastCharWasWrap = isWrapCharEndLine(token.getType)
            return token
          case State.NEWLINES =>
            if (isWrapCharBeginLine(token.getType)) {
              // ignore all the newlines when a wrap char comes after newlines
              lastCharWasWrap = isWrapChar(token.getType)
              state(ExtendedReplissLexer.State.INIT)
              return token
            } else if (token.getType == LangParser.NL) {
              return continue() //todo: continue is not supported
            }
            else if (isTab(token)) {
              state(ExtendedReplissLexer.State.BEGIN_LINE)
              numberOfTabs = tabWidth(token)
              return continue() //todo: continue is not supported

            } else { // no tabs after newline
              handleIndent(0, token, token.getStartIndex, token.getStopIndex, firstNewline)
              nextTokens.add(token)
              state(ExtendedReplissLexer.State.INIT)
              return firstNewline.get
            }
          case State.BEGIN_LINE =>
            if (isTab(token)) {
              numberOfTabs += tabWidth(token)
            }
            else {
              if (token.getType == LangParser.NL) {
                state(ExtendedReplissLexer.State.NEWLINES)
              }
              else {
                if (isWrapCharBeginLine(token.getType)) {
                  lastCharWasWrap = isWrapChar(token.getType)
                  state(ExtendedReplissLexer.State.INIT)
                  return token
                }
                else {
                  if (lastCharWasWrap && numberOfTabs > indentationLevels.top) { // ignore the newline, only return the token
                    state(ExtendedReplissLexer.State.INIT)
                    return token
                  }
                  else {
                    handleIndent(numberOfTabs, token, token.getStartIndex, token.getStopIndex, firstNewline)
                    state(ExtendedReplissLexer.State.INIT)
                    nextTokens.add(token)
                    return firstNewline.get
                  }
                }
              }
            }
        }
        continue()
      }

      continue()
    }

    def tabWidth(token: Token): Int = {
      val len: Int = 1 + token.getStopIndex - token.getStartIndex
      token.getType match {
        case LangParser.SPACETAB =>
          len
        case _ =>
          throw new IllegalArgumentException
      }
    }


    private def isTab(token: Token): Boolean = {
      token.getType == LangParser.SPACETAB
    }


    private def state(s: ExtendedReplissLexer.State.State): Unit = {
      if (debug) {
        WLogger.info("state " + state + " -> " + s)
      }
      state = s
    }

    private def handleIndent(n: Int, token: Token, start: Int, stop: Int, endBlockToken: Option[Token]): Unit = {
      if (debug) {
        WLogger.info("handleIndent " + n + "	 " + indentationLevels)
      }
      if (n > indentationLevels.top) {
        if (spacesPerIndent < 0) {
          spacesPerIndent = n
        }
        indentationLevels.push(n)
        nextTokens.add(makeToken(LangParser.STARTBLOCK, "$begin", start, stop))
      }
      else {
        while ( {
          n < indentationLevels.top
        }) {
          indentationLevels.pop
          nextTokens.add(makeToken(LangParser.ENDBLOCK, "$end",
            endBlockToken.map(_.getStartIndex).getOrElse(0),
            endBlockToken.map(_.getStartIndex).getOrElse(0)))
        }
        val expectedIndentation: Integer = indentationLevels.top
        if (n != expectedIndentation) { // all lines must be indented on the same level,
          // with the exception of the 'end' keyword, which can be on a different line
          val msg: String = "Invalid indentation level. Current indentation is " + expectedIndentation + ", but this is indented by " + n + "."
          for (el <- orig.getErrorListeners.asScala) {
            val line = token.getLine
            el.syntaxError(orig, "", line, token.getCharPositionInLine, msg, null)
          }
        }
      }
    }

    private def isWrapChar(tokenKind: Int): Boolean = {
      tokenKind match {
        case LangParser.COMMA
             | LangParser.PLUS
             | LangParser.MULT
             | LangParser.MINUS
             | LangParser.DIV
             | LangParser.MOD
             | LangParser.AND
             | LangParser.OR
             | LangParser.COLON
             | LangParser.COLONCOLON
             | LangParser.EQ
             | LangParser.EQEQ
             | LangParser.NOTEQ
             | LangParser.BAR
             | LangParser.IMPLIES
             | LangParser.IFF => true
        case _ => false
      }
    }

    private def isWrapCharEndLine(tokenKind: Int): Boolean = {
      tokenKind match {
        case LangParser.PAREN_LEFT | LangParser.BRACKET_LEFT | LangParser.BRACE_LEFT =>
          true
        case _ =>
          isWrapChar(tokenKind)
      }
    }

    private def isWrapCharBeginLine(`type`: Int): Boolean = {
      `type` match {
        case LangParser.PAREN_RIGHT
             | LangParser.BRACKET_RIGHT
             | LangParser.BRACE_RIGHT
             | LangParser.NOT
             | LangParser.DOT =>
          true
        case _ =>
          isWrapChar(`type`)
      }
    }

    private def makeToken(`type`: Int, text: String, start: Int, stop: Int): Token = {
      val source: Pair[TokenSource, CharStream] = sourcePair
      val channel: Int = 0
      val t: CommonToken = new CommonToken(source, `type`, channel, start, stop)
      t
    }

    override def setTokenFactory(factory: TokenFactory[_]): Unit = {
      orig.setTokenFactory(factory)
    }


    def setErrorListener(listener: ANTLRErrorListener): Unit = {
      orig.removeErrorListeners()
      orig.addErrorListener(listener)
    }


  }


}
