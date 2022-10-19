#===============================================================================
#
#  Utilities
#
#===============================================================================



#------------------------------------------------------------------------------
#  Hilfsfunktionen
#------------------------------------------------------------------------------

hole_et <- function(species){

  # Stieleiche und Traubeneiche werden zu Eiche

  code <- art_code(species)
  code <- ifelse(code %in% c(111, 112), 110, code)
  pos <- match(code, liste$ArtCodeNds)

  if(is.na(pos))
    stop(paste0("Die angeforderte Ertragstafel ", species, " ist nicht vorhanden."))

  return(et[[pos]])
}


art_code <- function(species){
  # Annahme: species ist numeric oder character
  if(is.numeric(species)){
    if(!(species %in% art_tabelle$code))
      stop(paste('Der Baumartencode', sQuote(species), 'ist nicht vorhanden.'))
  } else {
    pos <- pmatch(tolower(species),
                  tolower(c(art_tabelle$kurz, art_tabelle$lang,
                            art_tabelle$latein)))
    if(any(is.na(pos))){
      stop(paste0('Die Baumart ', paste(sQuote(species[is.na(pos)]), collapse=", "),
                  ' ist nicht vorhanden ',
                  'oder der Name ist mehrdeutig (z.B. ', sQuote('Berg'), ').'))
    } else {
      pos <- pos %% nrow(art_tabelle)
      species <- art_tabelle[ifelse(pos == 0, nrow(art_tabelle), pos), 'code']
    }
  }
  return(species)
}
