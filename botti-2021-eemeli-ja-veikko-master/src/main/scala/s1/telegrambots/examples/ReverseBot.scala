package s1.telegrambots.examples

import s1.telegrambots.BasicBot

import java.text.SimpleDateFormat
import java.util.Calendar
import collection.mutable.Buffer
import s1.telegrambots.Tapahtuma
import collection.mutable.Map




object ReverseBot extends App {

  val bot = new BasicBot() {


    //buffeeri mihin tapahtumat säilötään
   var tapahtumat = Buffer[Tapahtuma]()


/*

Funktio lisää taphtumalistaan tapahtuman annetulla paikalla ja päivämäärällä.
Funktio palauttaa tekstinä tapahtuman tiedot ja ohjeet osallistumiseen

 */
    def lisaa(msg: Message) : String = {

      val tiedot = getString(msg).split(",")
      val paivays = tiedot.lift(1)
      val paikka = tiedot.lift(0)
      if ( tapahtumat.exists(x=> x.päivämäärä == paivays && x.nimi == paikka)) {

        "tapahtuma on jo olemassa"
      }else if (paivays.nonEmpty && paikka.nonEmpty) {
      val lisattavaTapahtuma = new Tapahtuma(paikka,paivays,0,Map[String,String]())
      tapahtumat += lisattavaTapahtuma

      "Luotiin uusi tapahtuma: "+ "\n"+ "----------------------"  + "\n"+  paikka.getOrElse("Ei ilmoitettu paikkaa") + "," +
        paivays.getOrElse("").patch(2,".",0).patch(5,".",0)  +"\n"+
        "osallistu tapahtumaan kirjoittamalla: " + "/osallistu " + paikka.getOrElse("")+ "," + paivays.getOrElse("").patch(2,".",0).patch(5,".",0)
      }else "Tapahtuman luomiseen tarvittavia tietoja puuttuu."
    }
/*
Poistaa tapahtuman kutsulla \poista tapahtuma,päivämäärä(muotoa ddmmyyyy, ilman pisteitä)
Palauttaa poistetun tapahtuman tiedot


 */
    def poistaTapahatuma(msg: Message) = {
       val paikka = getString(msg).split(",").headOption.getOrElse("")
        val aika = getString(msg).split(",").lift(1).getOrElse("")
        val pvmPisteilla = aika.patch(2,".",0).patch(5,".",0)

      tapahtumat = tapahtumat.filter(x => !x.nimi.contains(paikka) && !x.päivämäärä.contains(aika) || !x.nimi.contains(paikka) && x.päivämäärä.contains(aika) || x.nimi.contains(paikka) && !x.päivämäärä.contains(aika))

      s"Poistit tapahtuman: $paikka($pvmPisteilla)"

    }


/*
Käytetään komennolla \tapahtumat
Palauttaa listan tapahtumista ja niiden tiedoista:
Tapahtuman paikka,päivämäärä ja osallistujalista.



 */
    def tapahtumaLista(msg: Message) = {

      if (tapahtumat.nonEmpty) {
     val tulossa = tapahtumat.map(x=> "***"+ x.nimi.getOrElse("") + "(" + x.päivämäärä.getOrElse("").patch(2,".",0).patch(5,".",0) + ")***" + "\n"+ "Tällä hetkellä osallistumassa: " + x.osallistujat.values.mkString(",")).mkString("\n\n")
       "Tulevat tapahtumat:" +"\n"+ "-------------------" + "\n\n" + tulossa
     }else " Ei tulevia tapahtumia."
    }

/*
Apufunktio joka selvittää, onko tapahtuman päivämäärä jo mennyt.
Palauttaa boolean arvon.


 */
    def paivamaaraOnamennyt(paivays: String) =  {

      val PV = new SimpleDateFormat("dd")
       val PVNyt = PV.format(Calendar.getInstance().getTime).toInt

      val KK = new SimpleDateFormat("MM")
      val KKNyt = KK.format(Calendar.getInstance().getTime).toInt

      val V = new SimpleDateFormat("YYYY")
      val VNyt = V.format(Calendar.getInstance().getTime).toInt

      val paivaysPV = paivays.substring(0,2).toInt
      val paivaysKK = paivays.substring(2,4).toInt
      val paivaysV = paivays.substring(4,8).toInt

      paivaysV > VNyt || paivaysV == VNyt && paivaysKK > KKNyt || paivaysV == VNyt && paivaysKK == KKNyt && paivaysPV > PVNyt

    }

/*
Käytetään komennolla \paivita
Poistaa tapahtumalistasta tapahtumat, joiden päivämäärä on jo mennyt.
Käyttäää apunaan paivamaaraOnmennyt funktiota


 */



   def poistaMenneet(msg: Message) = {

     tapahtumat = tapahtumat.filter(x => x.päivämäärä.forall(paivamaaraOnamennyt(_)))
     "Menneet tapahtumat poistettiin"

     }


      def osallistu_nro(msg: Message) : String = {
      // Funktio antaa tapahtumille lukuarvon, jota kautta voi ilmoittautua helpommin
      var string = ""
      if ( tapahtumat.length == 0) {string += "Ei tapahtumia"}
      else {
        string+=""
        }

      try {

          def askForInteger(msg: Message): Option[Int] = {
            var luku: Int= tapahtumat.length + 1
            var pieces = getString(msg).split(" ")
            //val aika = util.Try(getString(message).toInt).toOption

           var kakkoslukuluku = util.Try(getString(msg).toInt).toOption
           luku = kakkoslukuluku.get



            Some(luku)
          }

        var luku: Option[Int] = askForInteger(msg)

        // var luku = Some(2)
        if (luku.get <= tapahtumat.length) {
          tapahtumat(luku.get.asInstanceOf[Int]).osallistujat += getUsername(msg) -> getUserFirstName(msg)
          string += "\n Osallistuit tapahtumaan "+ tapahtumat(luku.get).toString       }
      } catch {
        case a: NumberFormatException => string+="NumberFormatException"
        case b: NoSuchElementException => tapahtumat.zipWithIndex.foreach{ case ( x, i) => string+= (i).toString+"   "+(x.nimi.getOrElse("")).toString+": "+x.annaPaivays+"\n"
      }}
      string
    }
  def poistaTiettyTapahtuma(msg: Message) = {
          // Funktio antaa tapahtumille lukuarvon, jota kautta voi ilmoittautua helpommin
      var string = ""
      if ( tapahtumat.length == 0) {string += "Ei tapahtumia"}
      else { string += ""
      }
      try {

          def askForInteger(msg: Message): Option[Int] = {
            var luku: Int= tapahtumat.length + 1
            var pieces = getString(msg).split(" ")
            //val aika = util.Try(getString(message).toInt).toOption

           var kakkoslukuluku = util.Try(getString(msg).toInt).toOption
           luku = kakkoslukuluku.get
           Some(luku)}

        var luku: Option[Int] = askForInteger(msg)
        string+= "\n Poistit tapahtuman indeksiltä "+luku.get.toString


        if (luku.get <= tapahtumat.length) {tapahtumat.remove(luku.get)} else {string+= "Else branch"}
      } catch {
        case a: NumberFormatException => string+="NumberFormatException"
        case b: NoSuchElementException => tapahtumat.zipWithIndex.foreach{ case ( x, i) => string+= (i).toString+"   "+(x.nimi.getOrElse("")).toString+": "+x.annaPaivays.toString+"\n"}

      }
      string
    }



 /*
 Funktio lisää osallistujan tapahtumaan ja päivittää osallistujamäärää.
 Jos käyttäjä on jo ilmoittauntunut tapahtumaan, ei funkto lisää käyttäjää uudestaan.
 Palauttaa onnistuneen ilmoittautumisen tiedot tai kertoo käyttäjän jo ilmoittautuneen.


 */
    def osallistu(msg: Message) = {
      val paikka = getString(msg).split(",").headOption.getOrElse("")
        val aika = getString(msg).split(",").lift(1).getOrElse("")

      if (tapahtumat.exists(x =>x.nimi.forall(_ == paikka   ) && x.päivämäärä.forall(_ == aika ))) {

        val oikeat =  tapahtumat.filter(x =>x.nimi match {
                  case  Some(nimi) => nimi == paikka
                  case None => false
                  }
                )
        if(oikeat.forall(_.osallistujat.contains(getUsername(msg)))) {

        "Olet jo ilmoittautunut."
        }
      else {
          oikeat.foreach(_.osallistujaMaara += 1)
          val nimi = getUserFirstName(msg) + " "+ getUserLastName(msg)
            oikeat.foreach(_.osallistujat += getUsername(msg) -> nimi )
           s"osallistuit tapahtumaan: $paikka($aika)"
       }
     }
     else  "Tapahtumaa ei ole olemassa."
    }

     // Apufunktio, joka funktio palauttaa päivämäärän muotoa ddmmyyyy)

    def paivamaara(msg: Message) : String = {

      val format = new SimpleDateFormat("ddMMYYYY")
        format.format(Calendar.getInstance().getTime)

    }
/*
Kutsutaan komennolla \help
Palauttaa ohjeet botin käyttämiseen
 */
    def help(msg: Message) =  {

      "Lisää tapahtuma: /uusitapahtuma tapahtuma,päivämäärä(muotoa ddmmyyyy ilman pisteitä)" + "\n\n" +
      "Osallistu tapahtumaan: /osallistu tapahtuma,päivämäärä(muotoa ddmmyyyy ilman pisteitä)" + "\n\n" +
      "Poista tapahtumat joiden päivämäärä on jo mennyt: /paivita" + "\n\n" +
      "Näytä kaikki tulevat tapahtumat: /tapahtumat" + "\n\n" +
      "Peru ilmoittautumisesi tapahtumaan: /peru tapahtuma,päivämäärä(muotoa ddmmyyyy ilman pisteitä)"+ "\n\n" +
      "Poista tapahtuma: /poista tapahtuma,päivämäärä(muotoa ddmmyyyy ilman pisteitä)"


    }
/*
Poistaa oman ilmoittautumisen
 */
    def poistaIlmoittautuminen(msg: Message) =  {
  val paikka = getString(msg).split(",").headOption.getOrElse("")
        val aika = getString(msg).split(",").lift(1).getOrElse("")
      val pvmPisteilla = aika.patch(2,".",0).patch(5,".",0)
if( tapahtumat.filter(x => x.nimi.contains(paikka) && x.päivämäärä.contains(aika)).exists(_.osallistujat.contains(getUsername(msg)))){


      tapahtumat.filter(x => x.nimi.contains(paikka) && x.päivämäärä.contains(aika)).foreach(_.osallistujat -= getUsername(msg))



      s"Peruit ilmoittautumisesi tapahtumaan: $paikka($pvmPisteilla) "
}else "Et ole ilmoittautunut kyseiseen tapahtumaan"

    }
    def vastaamatta(msg: Message) : String = {
      var palautusString = ""
      if (tapahtumat.isEmpty) {
        palautusString =  "Ei tapahtumia"
        palautusString}

        tapahtumat.zipWithIndex.foreach{ case ( x, i) => {
          if (tapahtumat(i).osallistujat.contains( getUsername(msg) )) { palautusString += "\n"}
          else { palautusString+= (i).toString+"   "+(x.nimi.getOrElse("")).toString+": "+x.annaPaivays+"\n" }

        }
        }
        palautusString
    }


  command("vastaamatta", vastaamatta)
  command("peru",poistaIlmoittautuminen)
  command("help",help)
  command("poista",poistaTapahatuma)
  command("osallistu", osallistu)
  command("uusitapahtuma",lisaa)
  command("tapahtumat", tapahtumaLista)
  command("paivita", poistaMenneet)
  command("poista_tapahtuma_", poistaTiettyTapahtuma)
  command("osallistunro", osallistu_nro)

    this.run()

    println("Started")


  }

}
