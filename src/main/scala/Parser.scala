package main.scala

/**
 * Created with IntelliJ IDEA.
 * User: jeremiefourbil
 * Date: 18/06/13
 * Time: 14:33
 * To change this template use File | Settings | File Templates.
 */

import scala.util.parsing.combinator.RegexParsers

abstract class Bulletin

case class MetarBulletin (
                           icao:String
                           ,datetime:String
                           ,windDirection:String
                           ,windSpeed:String
                           ,windShift:String
                           ,visibility:String
                           ,rvr:List[String]
                           ,weather:String
                           ,cloud:List[String]
                           ,temperature:String
                           ,airPressure:String
                           ,significant:String
                           ,remarks:List[String]
                           ) extends Bulletin

class MetarParser extends RegexParsers {
  def parse(data:String) = parseAll(all, data)
  def all:Parser[MetarBulletin] = {
    icao ~ datetime ~ wind ~ opt(windShift) ~ visibility ~ rep(rvr) ~ weather ~ rep(cloud) ~ temperature ~ airPressure ~ opt(significant) ~ """(RMK)?""".r ~ opt(rep(remarks)) ^^ {
      case icao ~ datetime ~ wind ~ windShift ~ visibility ~ rvr ~ weather ~ cloud ~ temperature ~ airPressure ~ significant ~ rmk ~ remarks =>
        MetarBulletin(
          icao
          ,datetime
          ,wind._1
          ,wind._2
          ,windShift.getOrElse("")
          ,visibility
          ,rvr
          ,weather
          ,cloud
          ,temperature
          ,airPressure
          ,significant.getOrElse("")
          ,remarks.getOrElse(List())
        )
    }
  }
  def icao:Parser[String] = {
    """[A-Z]{4}""".r
  }
  def datetime:Parser[String] = {
    """[0-9]{6}Z""".r
  }
  def wind:Parser[(String, String)] = {
    """([0-9]{3}|VRB)[0-9]{2}KT""".r ^^ {
      case wind:String => wind.splitAt(3)
    }
  }
  def windShift:Parser[String] = {
    """[0-9]{3}V[0-9]{3}""".r
  }
  def visibility:Parser[String] = {
    """[0-9]{4}""".r
  }
  def rvr:Parser[String] = {
    """R[0-9]{2}/P[0-9]{4}[UN]""".r
  }
  def weather:Parser[String] = {
    """[+-]?(TS|SH|FZ|BC|MI|PR)?(RA|SN|DZ|SG|GR|GS|IC|PL)?(BA|FG|SA|FU|DU|HZ|VA)?""".r
  }
  def cloud:Parser[String] = {
    """(FEW|SCT|BKN|OVC)([0-9]{3}|[/]{3})(CB)?""".r
  }
  def temperature:Parser[String] = {
    """M?[0-9]{2}/M?[0-9]{2}""".r
  }
  def airPressure:Parser[String] = {
    """Q[0-9]{4}""".r
  }
  def significant:Parser[String] = {
    """(NOSIG|BECMG|TEMPO|WS)(\s[0-9]{5}KT)?""".r
  }
  def remarks:Parser[String] = {
    """[A-Z0-9]+""".r
  }
}

object ParserApp {
  def main(args: Array[String]) {
    val bulletin = List(
      "RJCC 030810Z 21010KT 140V260 9999 SHRA FEW025 BKN030 FEW030CB 07/03 Q1001 RMK 1CU025 6CU030 2CB030 A2957 CB OHD MOV",
      "LFPN 181230Z VRB05KT 9999 FEW033 BKN200 27/17 Q1014 NOSIG",
      "LFQQ 181230Z 10005KT 060V150 CAVOK 27/16 Q1014 NOSIG"
    )
    val parser = new MetarParser
    bulletin.map(parser.parse(_)).foreach(println(_))
  }
}
