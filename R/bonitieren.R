#===============================================================================
#
#  Wrapper Funktion zum Bonitieren
#
#  greift auf die funktionalisierte und klassische Variante zurück
#
#===============================================================================


#' Bonitiert einen Bestand
#'
#' Für eine gegebene Baumart und Höhe (Mittel- oder Oberhöhe) bei gegebenem Alter
#' wird die relative oder absolute Bonität eines gleichaltrigen (ggf. ideellen)
#' Reinbestandes bestimmt. Erstere entspricht der Ertragsklasse und Zweitere der
#' Oberhöhe H100 im Alter 100 in Metern (Site Index).
#' Die Bonitierung erfolgt entweder über einen funktionalisierten Bonitätsfächer
#' oder über Inter-/Extrapolation der Ertragstafeln mittels Dreisatz.
#'
#' Die Bonitierung über die funktionalisierten Bonitätsfächer basiert auf
#' nichtlinearen Modellen, die durch Anpassung an die Ober- und Mittelhöhen über
#' dem Alter bei mäßiger Durchforstung bzw. für Eichen-Oberhöhen bei starker
#' Durchforstung aus der Ertragstafelsammlung von Schober (1995) entstanden. Für
#' die Kiefer wurde hierbei die Chapman-Richards-Funktion (Richards 1959) und
#' für alle anderen Baumarten die Wachstumsfunktion von Sloboda (1971) genutzt.
#' Die Bestandeshöhen der neuen Ertragstafeln (Albert et al. 2021) sind die
#' tabellarisierten Werte eben dieser funktionalisierten Bonitätsfächer.
#'
#' Der entscheidende Vorteil der Bonitätsfächermodelle gegenüber dem klassischen
#' Verfahren der linearen Extrapolation liegt darin, dass sie auch über den
#' Bonitäts- und Altersrahmen der Ertragstafeln hinaus robuste und biologisch
#' plausible Bestandeshöhen liefern. Daher unterscheiden sich die mit der
#' Methode `"funktional"` ermittelten Bonitäten von den der Methode `"klassisch"`
#' v.a. im Extrapolationsbereich der Ertragstafeln, d.h. bei
#' Alter-Bestandeshöhen-Kombination, die durch die Ertragstafeln nicht abgedeckt
#' sind.
#'
#' Für alle Baumarten außer Eiche werden bei `hoss=TRUE`, alternativ zu den oben
#' beschriebenen Standardmodellen, Oberhöhen-Verläufe, die auf der Anpassung der
#' Hossfeld IV-Funktion (Hossfeld 1822) beruhen, zugrunde gelegt. Diese kann für
#' die Bonitierung sehr junger Buchen-, Fichten- und Douglasien-Bestände (< 15
#' Jahre) geeigneter sein, da in dem hier standardmäßig genutzten Sloboda-Modell
#' die Höhenwerte in diesem Altersbereich unplausibel langsam ansteigen. Im
#' Gegensatz zum Standardmodell werden die in den Ertragstafeln angegebenen
#' Oberhöhen mit dem Hossfeld-Modell nicht exakt reproduziert.
#'
#' @param art Baumartenbezeichnung entweder als Kürzel, deutscher Name,
#'   lateinischer Name oder in niedersächsischer Kodierung.
#'   Für vorhandene Arten siehe [et_liste()].
#' @param alter Bestandesalter in Jahren. Bei Methode `"klassisch"` zwischen 5
#'   und max. zulässigem Alter (Ei 220, Bu 180 und Fi, Dgl, Ki 160). Ein Zahlwert.
#' @param hoehe Bestandeshöhe in Meter. Ein Zahlwert.
#' @param hoehe_typ Die Bestandeshöhe kann als Mittelhöhe (`"mittel"`) oder
#'   Oberhöhe (`"ober"`) angegeben werden. Parameter kann gekürzt werden, solange
#'   er eindeutig bleibt.
#' @param bon_typ Die Ausgabe der Bonität kann als relative Ertragsklasse
#'   (`"relativ"`) oder absolute Oberhöhenbonität (H100 im Alter 100 in Metern,
#'   `"absolut"`) erfolgen. Parameter kann gekürzt werden, solange er eindeutig
#'   bleibt.
#' @param methode Die Bonitierung erfolgt über funktionalisierte Bonitätsfächer
#'   (`"funktional"`) oder über Inter-/Extrapolation der Ertragstafeln mittels
#'   Dreisatz (`"klassisch"`). Parameter kann gekürzt werden, solange er
#'   eindeutig bleibt.
#' @param kapp_na Bonitäten werden bei Methode `"klassisch"` jenseits der -2. und
#'   4. und bei `"funktional"` jenseits der -3. und 7. Ertragsklasse gekappt.
#'   Wenn `TRUE`, wird für gekappte Werte `NA` ausgegeben, ansonsten der
#'   jeweilige Grenzwert. Ein Wahrheitswert.
#' @param ... Weitere Parameter, wie z.B. für funkt. Bonitätsfächermodell auf
#'   Basis der Hossfeld-Funktion (s. Details).
#'
#' @return Ein numerischer Vektor mit relativen oder absoluten Bonitäten,
#'   entsprechend Parameter `bon_typ`. Wenn Ertragsklassen gekappt wurden und
#'   `kapp_na == TRUE`, enthält der Vektor `NA`.
#'
#' @author Robert Nuske (klassisch), Kai Staupendahl (funktional)
#'
#' @seealso Informationen über vorhandene Tafeln [et_liste()], [et_info()],
#'   Ertragstafeln ausgeben [et_tafel()].
#'
#' @references
#' Albert M., Nagel J., Schmidt M., Nagel R.-V., Spellmann H. (2021): Eine neue
#'   Generation von Ertragstafeln für Eiche, Buche, Fichte, Douglasie und Kiefer
#'   \[Datensatz\]. Version 1.0. Zenodo. https://doi.org/10.5281/zenodo.6343906
#'
#' Hossfeld J.W. (1822): Mathematik für Forstmänner, Ökonomen und Cameralisten.
#'   Band 4, Gotha.
#'
#' Richards F.J. (1959): A flexible growth function for empirical use.
#'   Journal of Experimental Botany (10) 2: 290-301.
#'
#' Schober R. (1995): Ertragstafeln wichtiger Baumarten. 4. Aufl.,
#'   J. D. Sauerländer’s Verlag, Frankfurt a.M., 166 S.
#'
#' Sloboda B. (1971): Darstellung von Wachstumsprozessen mit Hilfe von
#'   Differentialgleichungen erster Ordnung. Mitteilungen der
#'   Baden-Württembergischen Forstlichen Versuchs- und Forschungsanstalt,
#'   Heft 32, Freiburg, 109 S.
#'
#' @export
#'
#' @examples
#' # moderne Bonitierung mittels funktionalisiertem Bonitätsfächer
#' et_bonitaet(art="bu", alter=100, hoehe=42)
#' et_bonitaet(art="fi", alter=100, hoehe=c(27, 29, 31))
#'
#' et_bonitaet("bu", 180, c(45.37, 40.36, 35.47, 30.77, 26.33),
#'              hoehe_typ="mittel", methode="funktional")
#'
#'
#' # klassische Bonitierung über Inter-/Extrapolation von Tafelnwerten
#' et_bonitaet(art="fi", alter=100, hoehe=30, methode="klassisch")
#' et_bonitaet(art="fi", alter=100, hoehe=c(27, 29, 31), methode="klassisch")
#'
#' et_bonitaet('Fagus sylvatica', alter=75, hoehe=25.3, methode="klassisch")
#' et_bonitaet('Bu', alter=42, hoehe=18, hoehe_typ='ober', methode="klassisch")
#'
#' et_bonitaet(611, alter=37, hoehe=18, hoehe_typ="mittel", methode="klassisch")
#' et_bonitaet(611, alter=37, hoehe=18, Hoehe_typ="mittel", methode="klassisch", bon_typ="absolut")
#'
#'
#' # mit gekappten Bonitäten
#' et_bonitaet("fi", 100, c(55, 39, 35, 31, 27, 8), methode="funktional")
#' et_bonitaet("fi", 100, c(55, 39, 35, 31, 27, 8), methode="funktional", kapp_na=FALSE)
#'
#' et_bonitaet("fi", 100, c(55, 39, 35, 31, 27, 8), methode="klassisch", kapp_na=TRUE)
#' et_bonitaet("fi", 100, c(55, 39, 35, 31, 27, 8), methode="klassisch", kapp_na=FALSE)

et_bonitaet <- function(art, alter, hoehe,
                        hoehe_typ="ober", bon_typ="relativ",
                        methode="funktional", kapp_na=TRUE, ...){

  if(missing(art) | missing(alter) | missing(hoehe))
    stop("art, alter und hoehe m\u00fcssen angegeben werden.")
  if(!is.numeric(alter) || !is.numeric(hoehe))
    stop("alter und hoehe m\u00fcssen numerisch sein.")
  hoehe_typ <- match.arg(hoehe_typ, c("mittel", "ober"))
  bon_typ <- match.arg(bon_typ, c("relativ", "absolut"))
  methode <- match.arg(methode, c("funktional", "klassisch"))
  if(length(kapp_na) > 1 || !is.logical(kapp_na) || is.na(kapp_na))
    stop("kapp_na muss ein Boolean/Wahrheitswert sein.")

  if(methode == "funktional"){
    # behandle dot-dot-dot auf der Suche nach hoss
    if(...length()){
      if(n <- match("hoss", ...names(), nomatch=0)){
        hoss <- ...elt(n)
        if(!is.logical(hoss)) stop("hoss ist nicht Boolean.")
      } else { stop("Unbekannte Parameter in ... \u00fcbergeben.") }
    } else { hoss <- FALSE }

    bon <- funk_bonitieren(art, alter, h=hoehe,
                           h_als_hg=isTRUE(hoehe_typ == "mittel"), hoss, bon_typ, kapp_na)
  } else {
    # klassisch
    df <- data.frame(art, alter, hoehe, hoehe_typ, bon_typ, kapp_na)
    bon <- unlist(.mapply(klas_bonitieren, df, NULL))
  }
  return(round(bon, 1))
}
