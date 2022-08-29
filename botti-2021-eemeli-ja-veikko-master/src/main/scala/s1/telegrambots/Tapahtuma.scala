package s1.telegrambots


import collection.mutable.Map


class Tapahtuma(var nimi: Option[String], var päivämäärä: Option[String], var osallistujaMaara: Int, val osallistujat: Map[String,String]) {
  def kerroOsallistujat() = {osallistujat.foreach(println)}
  def korjaaTietoja( string: Option[String], integer: Option[String] ) = {
    nimi = string
    päivämäärä = integer}
  def vastaamatta(user: String) = {if (osallistujat.contains(user) ) {false} else {true} }
  def annaPaivays = päivämäärä.getOrElse("")
  override def toString: String = {
    var string = nimi.getOrElse("")+", "+päivämäärä.getOrElse("")+": "
    string += osallistujat.values.mkString(",")
    string
}}
