#===============================================================================
#
#  Tafel ausgeben, ganz, in Teilen oder auch inter-/extrapoliert
#
#===============================================================================

#' Ertragstafelwerte einer Baumart
#'
#' Gibt die vollständige Ertragstafel einer Baumart aus oder ermittelt die
#' Ertragstafelwerte für gegebene Alter und/oder Bonitäten. Wenn Letztere von
#' den in der Ertragstafel (Albert et al. 2021)  enthaltenen Werten abweichen,
#' werden die Variablen der Tafel auf die gewünschten Alter und/oder Bonitäten
#' inter-/extrapoliert. Derzeit ist nur die klassische Variante mittels Dreisatz
#' implementiert.
#'
#' Bei der klassischen linearen Extrapolation in den Bereich sehr junger
#' Bestandesalter können negative Werte entstehen. Die jeweiligen Werte werden
#' dann auf `NA` (N, Hg, H100, Dg, Dw) oder `0` (G, V, N_aus, G_aus, Dg_aus,
#' V_aus, iV, GWL, dGZ) gesetzt.
#'
#' @param art Baumartenbezeichnung entweder als Kürzel, deutscher Name,
#'   lateinischer Name oder in niedersächsischer Kodierung.
#'   Für vorhandene Arten siehe [et_liste()].
#' @param alter Bestandesalter in Jahren als Zahlwert (optional). Zwischen 5
#'   und max. zulässigem Alter (Ei 220, Bu 180 und Fi, Dgl, Ki 160).
#' @param bon Ertragsklasse als Zahlwert (optional).  Bei Methode `"klassisch"`
#'   im Interval \[-2,4\].
#' @param bon_typ Die Bonität kann als relative Ertragsklasse (`"relativ"`) oder
#'   absolute Oberhöhenbonität (H100 im Alter 100, `"absolut"`) angegeben werden.
#'   Parameter kann gekürzt werden, solange er eindeutig bleibt.
#' @param methode Derzeit wird nur die Inter-/Extrapolation der Ertragstafel
#'   mittels Dreisatz (`"klassisch"`) angeboten. Parameter kann gekürzt werden,
#'   solange er eindeutig bleibt.
#'
#' @return Ein Dataframe mit den Spalten `Ekl`, `Alter`,
#'   `N`, `Hg`, `H100`, `G`, `Dg`, `Dw`, `V`, `N_aus`, `G_aus`, `Dg_aus`,
#'   `V_aus`, `iV`, `GWL`, `dGZ`.
#'
#'  \tabular{ll}{
#'    \strong{Kürzel} \tab \strong{Beschreibung}\cr
#'    `Ekl`    \tab relative Ertragsklasse \[-\]\cr
#'    `Alter`  \tab Bestandesalter \[a\]\cr
#'    `N`      \tab Stammzahl (verbleibend) \[Stk/ha\]\cr
#'    `Hg`     \tab Mittelhöhe (verbleibend) \[m\]\cr
#'    `H100`   \tab Oberhöhe (verbleibend) \[m\]\cr
#'    `G`      \tab Grundfläche (verbleibend) \[m^2/ha\] \cr
#'    `Dg`     \tab mittlerer BHD (verbleibend) \[cm\]\cr
#'    `Dw`     \tab mittlerer Durchmesser nach Weise (verbleibend) \[cm\]\cr
#'    `V`      \tab Vorrat (verbleibend) \[m^3/ha\]\cr
#'    `N_aus`  \tab Stammzahl (ausscheidend) \[Stk/ha\]\cr
#'    `G_aus`  \tab Grundfläche (ausscheidend) \[m^2/ha\]\cr
#'    `Dg_aus` \tab mittlerer Durchmesser (ausscheidend) \[cm\]\cr
#'    `V_aus`  \tab Vorrat (ausscheidend) \[m^3/ha\]\cr
#'    `iV`     \tab laufender Volumenzuwachs \[m^3/ha/a\]\cr
#'    `GWL`    \tab Gesamtwuchsleistung \[m^3/ha\]\cr
#'    `dGZ`    \tab durchschnittlicher Gesamtzuwachs \[m^3/ha/a\]
#' }
#'
#' Wenn durch die klassische Interpolation mittels Dreisatz negative Werte
#' entstehen sollten, werden die jeweiligen Werte auf `NA` (N, Hg, H100, Dg, Dw)
#' oder `0` (G, V, N_aus, G_aus, Dg_aus, V_aus, iV, GWL, dGZ) gesetzt.
#'
#' @author Robert Nuske
#' @references
#' Albert M., Nagel J., Schmidt M., Nagel R.-V., Spellmann H. (2021): Eine neue
#'   Generation von Ertragstafeln für Eiche, Buche, Fichte, Douglasie und Kiefer
#'   \[Datensatz\]. Version 1.0. Zenodo. https://doi.org/10.5281/zenodo.6343906
#'
#' @seealso Informationen über vorhandene Tafeln [et_liste()], [et_info()],
#'   Bonitieren [et_bonitaet()].
#'
#' @examples
#'   et_tafel('Bu')
#'   et_tafel(711, alter=100)
#'   et_tafel(611, bon=2.3)
#'   et_tafel(511, alter=80, bon=-0.5)
#'
#' @export

et_tafel <- function(art, alter=NULL, bon=NULL, bon_typ="relativ",
                     methode="klassisch"){
  if(missing(art))
    stop("Es muss mindestens eine Baumart angegeben werden.")
  if((!is.null(alter) && !is.numeric(alter)) ||
     (!is.null(bon) && !is.numeric(bon)))
    stop("alter und bon m\u00fcssen NULL oder Zahlen sein.")
  bon_typ <- match.arg(bon_typ, c("relativ", "absolut"))
  methode <- match.arg(methode, c("funktional", "klassisch"))

  if(methode == "funktional"){
    stop('Zurzeit ist nur die Methode \"klassisch\" implementiert.')
   } else { # klassisch
     out <- klas_tafel(art, alter, bon, bon_typ)
   }

  # Anzahlen auf ganze Zahlen und Rest auf eine Nachkommastelle runden
  out[c('N', 'N_aus')] <- round(out[c('N', 'N_aus')])
  out[! names(out) %in% c('N', 'N_aus')] <- round(out[! names(out) %in% c('N', 'N_aus')], 1)

  return(out)
}
