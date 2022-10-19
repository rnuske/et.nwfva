#==============================================================================
#
#   Klassische Interpolation von Extratafeln inkl Bonitierung
#
# 2017-09-15 Robert Nuske
# Updates: 2017-11-27, 2017-12-11, 2020-05-18, 2022-03-15, ...,
#          2022-08-26, ..., 2022-10-10
#==============================================================================


# Bonitieren
#
# Bestimmt die Ertragsklasse (relative Bonität) oder Site Indx (absolute Bonität)
# für ein Bestandesalter und eine Bestandeshöhe gemäß Ertragstafel.
#
# @param art Baumartencode, Kürzel, deutscher oder lateinischer Name
# @param alter ein Bestandesalter (5 bis max zul Alter)
# @param hoehe ein Bestandeshöhe
# @param hoehe_typ 'ober' oder 'mittel' Höhe?
# @param bon_typ 'relativ' oder 'absolut' Bonität ausgegeben?
# @param kapp_na Bonitäten jenseits von -2 und 4 Ertragsklasse werden gekappt.
#   Wenn `TRUE`, wird für gekappt Werte `NA` ausgegeben, ansonsten -2 bzw. 4.
#
# @return Ein Vektor mit der ermittelten Ertragsklasse.
#
# Außerhalb des zulässigen Altersbereichs von 5 bis max. zul. Alter
# (Ei 220, Bu 180 und Fi, Dgl, Ki 160) wird NA zurückgeben.
#
# Außerhalb des zulässigen Bonitätsbereichs [-2,4] wird gemäß Parameter kapp_na
# entweder NA oder der Grenzwert zurückgegeben.
#
# Die Werte werden mit allen Nachkommastellen ausgegeben. Rundung muss in
# aufrufender Funktion erfolgen.
#
# @examples
# klas_bonitieren('Fagus sylvatica', alter=75, hoehe=25.3)
# klas_bonitieren('Bu', alter=42, hoehe=18, hoehe_typ='ober')
# klas_bonitieren(611, alter=37, hoehe=11.8)


klas_bonitieren <- function(art, alter, hoehe, hoehe_typ="mittel",
                            bon_typ="relativ", kapp_na=TRUE){
  # Vorgehen:
  # * Tafel der Baumart besorgen
  # * Alterszeilen holen/erzeugen
  # * Höhe inter-/extrapolieren => Bonität
  # * Aufräumen

  if(missing(art) | missing(alter) | missing(hoehe))
    stop("art, alter und hoehe m\u00fcssen angegeben werden.")
  if(length(art) != 1 | length(alter) != 1 | length(hoehe) != 1)
    stop("art, alter und hoehe m\u00fcssen die L\u00e4nge 1 haben.")
  if(alter < 5){
    warning("alter ", sQuote(alter), " < 5. ",
            "Keine klassische Bonitierung m\u00f6glich => NA.")
    return(NA)
  }
  art_c <- as.character(art_code(art))
  if( alter > max_alter_klass[art_c] ){
    warning("alter ", sQuote(alter), " > ", max_alter_klass[art_c],
            " (max. zul\u00e4ssigem Alter der Baumart ", sQuote(art),
            "). Keine klassische Bonitierung m\u00f6glich => NA.")
    return(NA)
  }

  hoehe_typ <- match.arg(hoehe_typ, c('mittel', 'ober'))
  hoehe_typ <- switch(hoehe_typ, mittel='Hg', ober='H100')

  a_schritt <- 5   # Abstand der Altersklassen
  toleranz = 0.01  # eine Zehnerpotenz genauer als Höhen in Tafel (Dezimeter)
  extpol_oben <- extpol_unten <- NULL


  # Tafel besorgen
  #-----------------------------------------------------------------------------
  et <- hole_et(art)[["tafel"]]

  # Alterszeilen holen/machen
  #-----------------------------------------------------------------------------
  # In einer Tafel haben die einzelnen Ertragsklassen unterschiedliche kleinste
  # und größte Alter. In den Altersbereichen in denen nicht alle benötigten
  # Ekl Werte haben, muss extrapoliert werden.

  # Alter holen (tabelliertes Alter gefordert) ---------------------------------
  df_alter <- et[et$Alter == alter, ]

  # Alters Inter-/Extrapolation notwendig? -------------------------------------
  # kein Alterstreffer ODER nicht (Höhentreffer ODER (kleinere UND größere Höhe))
  h_dist <- df_alter[[hoehe_typ]] - hoehe
  if( (nrow(df_alter) == 0) ||
      !(any(abs(h_dist) < toleranz) || length(unique(sign(h_dist))) > 1L) ){

    # Interpoliere alle Ertragsklassen, die zwei Alter aufweisen
    # häufigster Fall!
    df_t <- et[abs(et$Alter - alter) < a_schritt, ]
    if( any(duplicated(df_t$Ekl)) ){
      df_t <- df_t[df_t$Ekl %in% df_t$Ekl[duplicated(df_t$Ekl)], ]
      alters <- unique(df_t$Alter)
      ratio <- (alter - alters[1]) / (alters[2] - alters[1])
      df_alter <- as.data.frame(do.call(rbind, by(df_t, df_t$Ekl, FUN=function(x){
        sapply(x, FUN=function(y){y[1] + (y[2] - y[1]) * ratio},
               simplify=TRUE)})))
    }

    # Extrapolation ertragsklassenweise
    # solange keine geeigneten Höhen vorliegen UND noch unbearbeitet Ekl vorhanden
    #   (Höhentreffer ODER (kleinere UND größere Höhe benachbarter Ekls))
    h_dist <- df_alter[[hoehe_typ]] - hoehe
    while( !( any(abs(h_dist) < toleranz) ||
              ( (length(idx <- which(diff(sign(h_dist)) != 0)) > 0) &&
               (abs(df_alter$Ekl[idx] - df_alter$Ekl[idx+1]) == 1) ) ) &&
           !is.na(ek <- setdiff(et$Ekl, df_alter$Ekl)[1]) ) {

      if(alter >= max(et$Alter[et$Ekl == ek])){
        # nach oben extrapolieren
        df_t <- et[et$Alter >= (max(et$Alter[et$Ekl == ek]) - a_schritt) & et$Ekl == ek, ]
        extpol_oben <- c(extpol_oben, ek)
      } else {
        # nach unten extrapolieren
        df_t <- et[et$Alter <= (min(et$Alter[et$Ekl == ek]) + a_schritt) & et$Ekl == ek, ]
        extpol_unten <- c(extpol_unten, ek)
      }
      alters <- unique(df_t$Alter)
      ratio <- (alter - alters[1]) / (alters[2] - alters[1])
      df_t <- as.data.frame(t(
               vapply(df_t,
                      FUN=function(x){x[1] + (x[2] - x[1]) * ratio},
                      FUN.VALUE=numeric(1))
               ))

      df_alter <- rbind(df_alter, df_t)
      df_alter <- df_alter[order(df_alter$Ekl), ] # Ekl müssen aufsteigen sein wg Vorzeichenwechseltest
      h_dist <- df_alter[[hoehe_typ]] - hoehe
    }
  }

  # Höhen Inter-/Extrapolation  =>  Bonität
  #-----------------------------------------------------------------------------
  h_dist <- abs(df_alter[[hoehe_typ]] - hoehe)
  if( any(h_dist < toleranz) ){
    # Höhentreffer
    bon <- df_alter[h_dist < toleranz, "Ekl", drop=TRUE]
  } else {
    # Höhen Inter-/Extrapolation notwendig
    df_zwei <- df_alter[h_dist <= sort(h_dist)[2], ]

    ratio <- (hoehe - df_zwei[1, hoehe_typ]) /
      (df_zwei[2, hoehe_typ] - df_zwei[1, hoehe_typ])

    bon <- df_zwei[1, "Ekl"] + (df_zwei[2, "Ekl"] - df_zwei[1, "Ekl"]) * ratio
  }

  # Aufräumen
  #-----------------------------------------------------------------------------

  # Warnungen, wenn extrapolierte Alter verwendet wurden
  if(!is.null(extpol_unten) | !is.null(extpol_oben) ){
    verw_eks <- if(exists("df_zwei")) df_zwei$Ekl else bon

    if( !is.null(extpol_oben) & any(verw_eks %in% extpol_oben) ){
      w_eks <- intersect(verw_eks, extpol_oben)
      w_et_max_alters <- tapply(et$Alter[et$Ekl %in% w_eks], et$Ekl[et$Ekl %in% w_eks], max)
      message("F\u00fcr die Ertragsklasse(n) ", sQuote(paste(w_eks, collapse=", ")),
              " ist das h\u00f6chste Alter in der Tafel ", sQuote(paste(w_et_max_alters, collapse=", ")),
              ". Es wurde auf das Alter ", sQuote(alter), " extrapoliert.")
    }
    if( !is.null(extpol_unten) & any(verw_eks %in% extpol_unten) ){
      w_eks <- intersect(verw_eks, extpol_unten)
      w_et_min_alters <- tapply(et$Alter[et$Ekl %in% w_eks], et$Ekl[et$Ekl %in% w_eks], min)
      message("F\u00fcr die Ertragsklasse(n) ", sQuote(paste(w_eks, collapse=", ")),
              " ist das niedrigste Alter in der Tafel ", sQuote(paste(w_et_min_alters, collapse=", ")),
              ". Es wurde auf das Alter ", sQuote(alter), " extrapoliert.")
    }
  }

  # Ekl außerhalb von [-2,4] kappen bzw NA
  if(bon < -2 | bon > 4){
    if(isTRUE(kapp_na)){
      warning('Die Bestandesh\u00f6he ', hoehe, ' im Alter ', alter,
              " ergibt eine Bonit\u00e4t au\u00dferhalb des Extrapolationsbereiches [-2,4].",
              " Da kapp_na=TRUE, wurde die Bonit\u00e4t auf NA gesetzt!")
      bon <- NA
    } else {
      bon <- ifelse(bon < -2, -2, 4)
      warning('Die Bestandesh\u00f6he ', hoehe, ' im Alter ', alter,
              " ergibt eine Bonit\u00e4t au\u00dferhalb des Extrapolationsbereiches [-2,4].",
              " Da kapp_na=FALSE, wurde die Bonit\u00e4t auf ", bon, " gesetzt!")
    }
  }

  if(is.na(bon))
    return(NA)

  if(bon_typ == "relativ"){
    # relative Ertragsklasse
    return(bon)
  } else {
    # Absolute Oberhöhenbonität
    return(klas_tafel(art, alter=100, bon)$H100)
  }
}


# Ertragstafel
#
# Interpoliert eine oder mehrere Ertragstafel gemäß der gegebenen Baumart,
# Bestandesalter und Ertragsklasse.
#
# @param art Baumartencode, Kürzel, deutscher oder lateinischer Name
# @param alter Bestandesalter (optional) aber mindestens 5 (integer, optional)
# @param bon Ertragklasse kann angegeben werden (numeric, optional)
# @param bon_typ Die Bonität kann als relative Ertragsklasse (`"relativ"`) oder
#   absolute Oberhöhenbonität (H100 im Alter 100, `"absolut"`) angegeben werden.
#   Parameter kann gekürzt werden, solange er eindeutig bleibt.
#
# @return Ein Dataframe mit den Ertragstafelwerten.
#
# Die Werte werden mit allen Nachkommastellen ausgegeben. Rundung muss in
# aufrufender Funktion erfolgen.
#
# Außerhalb des zulässigen Bonitätsbereichs [-2,4] wird NA zurückgegeben.
#
# Wenn durch die Extrapolation in den Bereich der jungen Alter negative Werte
# entstehen (z.B. wenn im ersten tabellierten Alter 0 oder ein sehr niedriger
# Wert ausgewiesen ist), werden die jeweiligen Werte auf `NA` (N, Hg, H100, Dg, Dw)
# oder `0` (G, V, N_aus, G_aus, Dg_aus, V_aus, iV, GWL, dGZ) gesetzt.
#
# Außerhalb des zulässigen Altersbereichs von 5 bis max. zul. Alter
# (Ei 220, Bu 180 und Fi, Dgl, Ki 160) bricht die Funktion mit Fehler ab.
#
# @examples
#   klas_tafel('Bu')
#   klas_tafel(711, alter=100)
#   klas_tafel(611, bon=2.3)
#   klas_tafel(511, alter=80, bon=-0.5)

klas_tafel <- function(art, alter=NULL, bon=NULL, bon_typ="relativ"){
  # Vorgehen:
  # * Tafel besorgen
  # * nur art     => alles ausgeben
  # * alter       => Alterszeilen holen/erzeugen
  # * alter & bon => Alterszeilen nach Ekl inter-/extrapolieren
  # * bon         => Ertragsklassenzeilen holen/erzeugen

  if(missing(art))
    stop("Eine Baumart muss mindestens angegeben werden.")

  # Check Bonitätsbereich & Umrechnung abs -> rel
  if(!is.null(bon)){
    if(bon_typ == "relativ" && (bon < -2 || bon > 4))
      stop("Relative Bonit\u00e4t muss im Intervall [-2,4] sein.")

    if(bon_typ == "absolut"){
      art_c <- as.character(art_code(art))
      if(bon < absbon_min_klas[art_c] | bon > absbon_max_klas[art_c])
        stop("Absolute Bonit\u00e4t muss im Intervall [-2,4] sein.")

      bon <- klas_bonitieren(art, alter=100, hoehe=bon, hoehe_typ="ober", bon_typ="relativ")
    }
  }

  a_schritt <- 5   # Abstand der Altersklassen
  toleranz = 0.01  # eine Zehnerpotenz genauer als Höhen in Tafel (Dezimeter)
  extpol_unten <- extpol_oben <- NULL

  # Tafel besorgen
  #-----------------------------------------------------------------------------
  et <- hole_et(art)[["tafel"]]

  if(is.null(alter) & is.null(bon)){
    # Alles ausgeben, da nix spezifiziert
    #---------------------------------------------------------------------------
    out <- et

  } else {
    if(!is.null(alter)){
      # Alterszeilen holen/machen
      #-------------------------------------------------------------------------
      # In einer Tafel haben die einzelnen Ertragsklassen unterschiedliche
      # kleinste und größte Alter. In den Altersbereichen in denen nicht alle
      # benötigten Ekl Werte haben, muss extrapoliert werden.

      if(alter < 5)
        stop("alter muss gr\u00f6\u00dfer als 5 sein.")
      art_c <- as.character(art_code(art))
      if(alter > max_alter_klass[art_c])
        stop("alter muss kleiner als ", max_alter_klass[art_c],
             " (max. zul\u00e4ssigem Alter der Baumart ", sQuote(art), ") sein.")

      # Alters Inter-/Extrapolation notwendig?
      df_alter <- et[et$Alter == alter, ]
      if( nrow(df_alter) < length(unique(et$Ekl)) ){

        # Interpoliere alle Ertragsklassen, die zwei Alter aufweisen
        # häufigster Fall!
        df_t <- et[abs(et$Alter - alter) < a_schritt, ]
        if( any(duplicated(df_t$Ekl)) ){
          df_t <- df_t[df_t$Ekl %in% df_t$Ekl[duplicated(df_t$Ekl)], ]
          alters <- unique(df_t$Alter)
          ratio <- (alter - alters[1]) / (alters[2] - alters[1])
          df_alter <- as.data.frame(do.call(rbind, by(df_t, df_t$Ekl, FUN=function(x){
            sapply(x, FUN=function(y){y[1] + (y[2] - y[1]) * ratio},
                   simplify=TRUE)})))
        }

        # Extrapoliere ertragsklassenweise fehlende Alter
        ekls_ext <- setdiff(et$Ekl, df_alter$Ekl)
        for(ekl in ekls_ext) {

          if(alter >= max(et$Alter[et$Ekl == ekl])){
            # nach oben extrapolieren
            df_t <- et[et$Alter >= (max(et$Alter[et$Ekl == ekl]) - a_schritt) & et$Ekl == ekl, ]
            extpol_oben <- c(extpol_oben, ekl)
          } else {
            # nach unten extrapolieren
            df_t <- et[et$Alter <= (min(et$Alter[et$Ekl == ekl]) + a_schritt) & et$Ekl == ekl, ]
            extpol_unten <- c(extpol_unten, ekl)
          }
          alters <- unique(df_t$Alter)
          ratio <- (alter - alters[1]) / (alters[2] - alters[1])
          df_t <- as.data.frame(t(
            vapply(df_t,
                   FUN=function(x){x[1] + (x[2] - x[1]) * ratio},
                   FUN.VALUE=numeric(1))
          ))

          df_alter <- rbind(df_alter, df_t)
        }
        df_alter <- df_alter[order(df_alter$Ekl), ]
      }

      if(is.null(bon)){
        # kein bon => Alterszeilen ausgeben
        out <- df_alter

      } else {
        # alter & bon
        #-----------------------------------------------------------------------

        # Ekl Inter-/Extrapolation notwendig?
        out <- df_alter[df_alter$Ekl == bon, ]
        if(nrow(out) == 0){
          # Inter-/Extrapolation notwendig
          ek_dist <- abs(df_alter[, 'Ekl'] - bon)
          df_zwei <- df_alter[ek_dist <= sort(ek_dist)[2], ]
          ratio <- (bon - df_zwei[1, 'Ekl']) /
            (df_zwei[2, 'Ekl'] - df_zwei[1, 'Ekl'])
          out <- as.data.frame(t(
            vapply(df_zwei,
                   FUN=function(y){y[1] + (y[2] - y[1]) * ratio},
                   FUN.VALUE=numeric(1))
          ))
        }
      }
    } else {
      # nur bon
      #-------------------------------------------------------------------------

      # Ekl Inter-/Extrapolation notwendig?
      out <- et[et$Ekl == bon, ]
      if(nrow(out) == 0){
        # Inter-/Extrapolation
        ek_dist <- abs(et[, 'Ekl'] - bon)
        df_zwei <- et[ek_dist <= sort(unique(ek_dist))[2], ]
        eks <- unique(df_zwei[, 'Ekl'])
        ratio <- (bon - eks[1]) / (eks[2] - eks[1])
        out <- as.data.frame(do.call(rbind, by(df_zwei, df_zwei$Alter,
                     FUN=function(x){
                       vapply(x, FUN=function(y){y[1] + (y[2] - y[1]) * ratio},
                              FUN.VALUE=numeric(1))
                       })))

        # eine der beiden Ekl könnte weniger Alter haben, was zu NA führt
        out <- out[!is.na(out[,1]), ]
      }
    }
  }
  # Zeilennummern begradigen
  row.names(out) <- 1:nrow(out)


  # Behandlung unzulässiger negativer Interpolationsergebnisse -----------------
  if(any(out[names(out) != 'Ekl'] < 0)){
  # G, V, N_aus, G_aus, Dg_aus, V_aus, iV, GWL und dGZ auf 0, falls negativ
  out[, c(6, 9:16)] <- sapply(out[, c(6, 9:16)],
                              FUN=function(x){ifelse(x < 0, 0, x)})

  # N, Hg, H100, Dg, Dw auf NA, falls negativ
  out[, c(3:5, 7, 8)] <- sapply(out[, c(3:5, 7, 8)],
                                FUN=function(x){ifelse(x < 0, NA, x)})
  }

  # Warnungen, wenn extrapolierte Alter verwendet wurden -----------------------
  if(!is.null(extpol_unten) | !is.null(extpol_oben) ){
    verw_eks <- if(exists("df_zwei")) df_zwei$Ekl else out$Ekl

    if( !is.null(extpol_oben) & any(verw_eks %in% extpol_oben) ){
      w_eks <- intersect(verw_eks, extpol_oben)
      w_et_max_alters <- tapply(et$Alter[et$Ekl %in% w_eks], et$Ekl[et$Ekl %in% w_eks], max)
      message("F\u00fcr die Ertragsklasse(n) ", sQuote(paste(w_eks, collapse=", ")),
              " ist das h\u00f6chste Alter in der Tafel ", sQuote(paste(w_et_max_alters, collapse=", ")),
              ". Es wurde auf das Alter ", sQuote(alter), " extrapoliert.")
    }
    if( !is.null(extpol_unten) & any(verw_eks %in% extpol_unten) ){
      w_eks <- intersect(verw_eks, extpol_unten)
      w_et_min_alters <- tapply(et$Alter[et$Ekl %in% w_eks], et$Ekl[et$Ekl %in% w_eks], min)
      message("F\u00fcr die Ertragsklasse(n) ", sQuote(paste(w_eks, collapse=", ")),
              " ist das niedrigste Alter in der Tafel ", sQuote(paste(w_et_min_alters, collapse=", ")),
              ". Es wurde auf das Alter ", sQuote(alter), " extrapoliert.")
    }
  }

  return(out)
}


# Umrechnungen von rel zu abs Bonitäten (vektorisiert)
#
# @param art Baumartencode, Kürzel, deutscher oder lateinischer Name
# @param ekl relative Bonitäten (Ertragsklassen).
#
# @return Vektor mit absoluten Ertragsklassen
#
# Die Ekl werden mit allen Nachkommastellen ausgegeben. Rundung muss in
# aufrufender Funktion erfolgen.
#
# Außerhalb des zulässigen Bonitätsbereiches gibt die nachgeordnete Funktion
# klas_hoehe() NA aus und somit auch diese Funktion.

klas_ekl2si <- function(art, ekl){
  # Bonitätsbereiche werden in et_hoehe geprüft

  if(any(length(art) > 1, length(ekl) > 1)){
    # mehrere
    df <- data.frame(art, bon=ekl)
    si <- unlist(.mapply(klas_hoehe, df,
                  MoreArgs=list(alter=100, bon_typ="relativ", hoehe_typ="ober")))
  } else {
    # einer
    si <- klas_hoehe(art, alter=100, bon=ekl, bon_typ="relativ", hoehe_typ="ober")
  }

  return(si)
}


# Umrechnungen von abs zu rel Bonitäten (vektorisiert)
#
# @param art Baumartencode, Kürzel, deutscher oder lateinischer Name
# @param si Oberhöhenbonitäten H100 im Alter 100 in m (Site Index).
#
# @return Vektor mit relativen Ertragsklassen
#
# Die Ekl werden mit allen Nachkommastellen ausgegeben. Rundung muss in
# aufrufender Funktion erfolgen.
#
# Außerhalb des zulässigen Bonitätsbereiches gibt die nachgeordnete Funktion
# klas_bonitieren() NA aus und somit auch diese Funktion.

klas_si2ekl <- function(art, si){
  if(any(length(art) > 1, length(si) > 1)){
    # mehrere
    df <- data.frame(art, hoehe=si)
    ekl <- suppressWarnings(suppressMessages(
      unlist(.mapply(klas_bonitieren, df,
                     MoreArgs=list(alter=100, hoehe_typ="ober", bon_typ="relativ", kapp_na=TRUE)))
    ))
  } else {
    # einer
    ekl <- suppressWarnings(suppressMessages(
      klas_bonitieren(art, hoehe=si, alter=100, hoehe_typ="ober", bon_typ="relativ", kapp_na=TRUE)
    ))
  }

  return(ekl)
}


# Bestandeshöhen (vektorisiert)
#
# @param art Baumartencode, Kürzel, deutscher oder lateinischer Name
# @param alter Bestandesalter in Jahren
# @param bon Bonität
# @param bon_typ Art der Bonität "relativ" oder "absolut"
# @param hoehe_typ "ober" oder "mittel" Höhe
#
# @return Vektor der Bestandesoberhöhen
#
# Die Höhen werden mit allen Nachkommastellen ausgegeben. Rundung muss in
# aufrufender Funktion erfolgen.
#
# Außerhalb des zulässigen Alters- und Bonitätsbereichs wird es NA sein.
# Kann zusätzlich für kleine Alter und schlechte Bonitäten auch NA sein.

klas_hoehe <- function(art, alter, bon, bon_typ, hoehe_typ){
  ht <- switch(hoehe_typ, mittel='Hg', ober='H100')

  if(any(length(art) > 1, length(alter) > 1,
         length(bon) > 1, length(bon_typ) > 1)){
    # mehrere
    df <- data.frame(art, alter, bon, bon_typ)
    h <- unlist(.mapply(klas_hoehe_skalar, df, MoreArgs=list(ht=ht)))
  } else {
    # einer
    h <- klas_hoehe_skalar(art, alter, bon, bon_typ, ht)
  }
  return(h)
}


#===============================================================================
# Hilfsfunktionen (nur zu internen Verwendung)
#===============================================================================

# EINE Bestandeshöhe
#
# @param art Baumartencode, Kürzel, deutscher oder lateinischer Name
# @param alter Bestandesalter in Jahren
# @param bon Bonität
# @param bon_typ "relativ" oder "absolut" Bonität
# @param ht "Hg" oder "H100" für Mittel- bzw. Oberhöhe
#
# @return Eine Bestandesoberhöhe, mit allen Nachkommastellen,
# außerhalb des Bonitätsbereichs NA, kann bei kleinen Altern NA sein.

klas_hoehe_skalar <- function(art, alter, bon, bon_typ, ht){
  art_c <- as.character(art_code(art))

  # Altersbereich
  if(alter < 5 | alter > max_alter_klass[art_c])
    return(NA)

  # Bonitätsbereich (relativ)
  if(bon_typ == "relativ" && (bon < -2 | bon > 4))
    return(NA)

  # Umwandeln von abs in rel Bonität
  if(bon_typ == "absolut"){
    # Bonitätsbereich (absolut)
    if(bon < absbon_min_klas[art_c] | bon > absbon_max_klas[art_c])
      return(NA)
    bon <- klas_bonitieren(art, alter=100, hoehe=bon, hoehe_typ="ober", bon_typ="relativ")
  }

  # Bestandeshöhe bestimmen
  return(klas_tafel(art, alter, bon)[, ht])
}
