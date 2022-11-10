#===============================================================================
#
#  Wrapper Funktion zum Umrechnen von Bonitäten
#
#  greift auf die funktionalisierte und klassische Variante zurück
#
#===============================================================================


#' Umrechnung von Bonitäten (Ertragsklasse und Oberhöhenbonitiät)
#'
#' Bestimmt für eine gegebene Baumart und Ausgangs-Bonität die Ziel-Bonität als
#' relative Ertragsklasse oder absolute Oberhöhenbonität (Oberhöhe H100 im Alter
#' 100 in Metern). Die Umrechnung basiert auf den in den neuen Ertragstafeln der
#' NW-FVA (Albert et al. 2021) je Baumart und Ertragsklasse angegebenen
#' Oberhöhen H100 im Alter 100, d.h. der dort gewählten Bonitätsstaffelung. Die
#' Umrechnung erfolgt entweder über einen funktionalisierten Bonitätsfächer oder
#' über Inter-/Extrapolation der Ertragstafeln mittels Dreisatz.
#'
#' @param art Baumartenbezeichnung entweder als Kürzel, deutscher Name,
#'   lateinischer Name oder in niedersächsischer Kodierung.
#'   Für vorhandene Arten siehe [et_liste()].
#' @param bon Bonität als Zahlwert. Zulässig sind relative Ertragsklassen im
#'   Interval \[-2,4\] bzw. \[-3,7\] bei Methode `"klassisch"` bzw.
#'   `"funktional"` und absolute Bonitäten entsprechend. Welche Art der
#'   Ausgangs-Bonität hier übergeben wird bestimmt der Parameter `richtung`.
#' @param richtung Umrechnung von absoluter Oberhöhenbonitäten zu relativer
#'   Ertragsklasse (`"abs_zu_rel"`) oder umgkehrt (`"rel_zu_abs"`).
#'   Parameter kann gekürzt werden, solange er eindeutig bleibt.
#' @param methode Die Umrechnung erfolgt über funktionalisierte Bonitätsfächer
#'   (`"funktional"`) oder über Inter-/Extrapolation der Ertragstafeln mittels
#'   Dreisatz (`"klassisch"`). Parameter kann gekürzt werden, solange er
#'   eindeutig bleibt.
#'
#' @return Numerischer Vektor mit gesuchten Bonitäten, je nach `richtung`
#'   entweder absolute Oberhöhenbonitäten oder relative Ertragsklassen. Für
#'   Werte außerhalb des zulässigen Bonitätsintervalls wird `NA` ausgegeben.
#'
#' @author Robert Nuske (klassisch), Kai Staupendahl (funktional)
#'
#' @seealso [et_hoehe()] zur Ermittlung der Bestandeshöhen und [et_bonitaet()]
#'   zur Bonitierung.
#'
#' @references
#' Albert M., Nagel J., Schmidt M., Nagel R.-V., Spellmann H. (2021): Eine neue
#'   Generation von Ertragstafeln für Eiche, Buche, Fichte, Douglasie und Kiefer
#'   \[Datensatz\]. Version 1.0. Zenodo. https://doi.org/10.5281/zenodo.6343906
#'
#' @export
#'
#' @examples
#' et_bontrans("Kiefer", bon=27)
#' et_bontrans("Kiefer", bon=27, richtung="abs_zu_rel", methode="klassisch")
#'
#' arten <- c("fi", "fi", "bu", "dgl")
#' h100 <- c(34.5, 29.3, 36, 40)
#' et_bontrans(art=arten, bon=h100, richtung="abs_zu_rel")
#' et_bontrans(art=arten, bon=h100, richtung="abs_zu_rel", methode="klassisch")
#'
#' et_bontrans("Kiefer", bon=1.5, richtung="rel_zu_abs", methode="funk")
#' et_bontrans("Kiefer", bon=1.5, richtung="rel_zu_abs", methode="klass")
#'
#' arten <- c("fi", "fi", "bu", "dgl")
#' ertragsklassen <- c(2.5, 0.3, 1, 2.4)
#' et_bontrans(art=arten, bon=ertragsklassen, richtung="rel_zu", methode="funk")
#' et_bontrans(art=arten, bon=ertragsklassen, richtung="rel_zu", methode="klass")

et_bontrans <- function(art, bon, richtung="abs_zu_rel", methode="funktional") {

  if(missing(art) | missing(bon))
    stop("art und bon m\u00fcssen angegeben werden.")
  if(!is.numeric(bon))
    stop("bon muss numerisch sein.")
  methode <- match.arg(methode, c("funktional", "klassisch"))
  richtung <- match.arg(richtung, c("abs_zu_rel", "rel_zu_abs"))

  if(richtung == "abs_zu_rel"){
    if(methode == "funktional"){
      out <- funk_si2ekl(art, bon)
    } else { # klassisch
      out <- klas_si2ekl(art, bon)
    }
  } else { # rel_zu_abs
    if(methode == "funktional"){
      out <- funk_ekl2si(art, bon)
    } else { # klassisch
      out <- klas_ekl2si(art, bon)
    }
  }

  # Die Bonitätsbereiche werden in den nachgeordneten Funktionen geprüft

  return(round(out, 1))
}

