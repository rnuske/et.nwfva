#===============================================================================
#
#  Info-Funktionen
#
#===============================================================================



#' Liste der verfügbaren Ertragstafeln
#'
#' @return Knappe Beschreibung der vorhandenen Ertragstafeln in einem Dataframe
#'   mit den Spalten `Baumart`, `ArtCodeNds`, `Tafelname`, `Jahr`, `Region`.
#'
#'  \tabular{ll}{
#'    \strong{Kürzel} \tab \strong{Beschreibung}\cr
#'    `Baumart`    \tab deutscher Name der Baumart\cr
#'    `ArtCodeNds` \tab Codenummer der niedersächsischen Landesforsten\cr
#'    `Tafelname`  \tab Name der Tafel\cr
#'    `Jahr`       \tab Jahr der Veröffentlichung\cr
#'    `Region`     \tab Gültigkeitsgebiet der Tafel\cr
#' }
#'
#'
#' @author Robert Nuske
#' @seealso [et_info()] für detaillierte Informationen zu einer Tafel.
#' @examples
#' et_liste()
#' @export

et_liste <- function(){
  return(liste)
}


#' Informationen zu einer Ertragstafel
#'
#' Detaillierte Information zur Ertragstafel einer gewählten Baumart.
#'
#' @param art Baumartenbezeichnung entweder als Kürzel, deutscher Name,
#'   lateinischer Name oder in niedersächsischer Kodierung.
#'   Für vorhandene Arten siehe [et_liste()].
#'
#' @return Informationen zu einer Tafel als Liste mit den Elementen `Tafelname`,
#'   `Baumart`, `WissName`, `ArtCodeNds`, `Autor`, `Jahr`, `Quelle`, `Region`.
#'
#'  \tabular{ll}{
#'    \strong{Kürzel} \tab \strong{Beschreibung}\cr
#'    `Tafelname`  \tab Name der Tafel\cr
#'    `Baumart`    \tab deutscher Name der Baumart\cr
#'    `WissName`   \tab wissenschaftlicher Name der Baumart\cr
#'    `ArtCodeNds` \tab Codenummer der niedersächsischen Landesforsten\cr
#'    `Autor`      \tab Autor der Tafel\cr
#'    `Jahr`       \tab Jahr der Veröffentlichung\cr
#'    `Quelle`     \tab Zitat der Tafel\cr
#'    `Region`     \tab Gültigkeitsgebiet der Tafel
#' }
#'
#' @author Robert Nuske
#' @seealso [et_liste()] bietet eine Übersicht aller verfügbaren Tafeln.
#' @references
#' Albert M., Nagel J., Schmidt M., Nagel R.-V., Spellmann H. (2021): Eine neue
#'   Generation von Ertragstafeln für Eiche, Buche, Fichte, Douglasie und Kiefer
#'   \[Datensatz\]. Version 1.0. Zenodo. https://doi.org/10.5281/zenodo.6343906
#'
#' @examples
#' et_info(211)
#'
#' @export

et_info <- function(art){
  return(hole_et(art)[["info"]])
}

