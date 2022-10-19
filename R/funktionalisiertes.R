#===============================================================================
#
#  Funktionen zur Beschreibung der bonitäts- und altersabhängigen Entwicklung der
#  Spitzen- und Mittelhöhen (Bonitätsfächer) gemäß ausgewählter Ertragstafeln
#  für die Hauptbaumarten

#  Eiche (Qercus robur L. und Quercus petraea (MATT.) LIEBL.),
#  Buche (Fagus sylvatica L.),
#  Fichte (Picea abies (L.) H. KARST.),
#  Douglasie (Pseudotsuga menziesii (MIRBEL) FRANCO) und
#  Kiefer (Pinus sylvestris L.)
#
#  - Umrechnung von Ertragsklasse in Site Index (SI) und vice versa
#  - Ausgabe der Spitzen- oder Mittelhöhe bei gegebenem Alter und gegebener Bonität
#  - Ausgabe der Bonität bei gegebenem Alter und gegebener Spitzen- oder Mittelhöhe
#
#  2022-09-19 Kai Staupendahl
#
#===============================================================================


# Umrechnung von Site Index in Ertragsklasse
#
# Bestimmt für eine gegebene Baumart und einen gegebenen Site Index (Spitzenhöhe
# H100 im Alter 100 in m -> absolute Höhenbonität) die zugehörige Ertragsklasse
# (relative Bonität).
#
# Die Umrechnung basiert auf den in den neuen Ertragstafeln der NW-FVA je
# Baumart und Ertragsklasse angegebenen Spitzenhöhen H100 im Alter 100. Die
# Umrechnungsergebnisse gelten also nur für diese Ertagstafeln!
#
# @param art Vektor von Baumartencode, Kürzel, deutscher oder lateinischer Name
# @param si Vektor mit Site Indizes == abs Bonität == H100 im Alter 100
#
# @return Numerischer Vektor mit relativen Ertragsklassen.
#
# Die Werte werden mit mehr als einer Nachkommastellen ausgegeben.
# Rundung muss in aufrufender Funktion erfolgen.
#
# @examples
# funk_si2ekl(art="Kiefer", si=27)
# funk_si2ekl(art=c("fi", "fi", "bu", "dgl"), si=c(34.5, 29.3, 36, 40))

funk_si2ekl <- function(art, si) {
  art = sapply(art, art_code)
  if (length(art) == 1 & length(si) > 1) {
    art = rep(art, length(si))
  } else if (length(si) == 1 & length(art) > 1) {
    si = rep(si, length(art))
  } else if (length(art) != length(si) & length(art) > 1 & length(si) > 1) {
    stop("Die \u00fcbergebenen Vektoren sind unterschiedlich lang!")
  }

  ekl = ifelse(art %in% c(110,111,112), (30 - si) / 3,
        ifelse(art == 211, (36.5 - si) / 4,
        ifelse(art == 511, (39 - si) / 4,
        ifelse(art == 611, (50 - si) / 5,
        ifelse(art == 711, (33 - si) / 4,
               NA)))))
  if (sum(art %in% c(110,111,112,211,511,611,711)) != length(art)) {
    warning("Es wurden unbekannte Baumarten \u00fcbergeben!")  # never seen, art_code() stoppt vorher
  }

  # Bonitätsbereich [-3,7] checken
  ekl <- ifelse(ekl < -3 | ekl > 7, NA, ekl)

  return(ekl)
}


# Umrechnung von Ertragsklasse in Site Index
#
# Bestimmt für eine gegebene Baumart und Ertragsklasse (relative Bonität) den
# zugehörigen Site Index (Spitzenhöhe H100 im Alter 100 in m = absolute
# Höhenbonität).
#
# Die Umrechnung basiert auf den in den neuen Ertragstafeln der NW-FVA je
# Baumart und Ertragsklasse angegebenen Spitzenhöhen H100 im Alter 100. Die
# Umrechnungsergebnisse gelten also nur für diese Ertagstafeln!
#
# @param art Vektor von Baumartencode, Kürzel, deutscher oder lateinischer Name
# @param ekl Vektor mit relativen Ertragsklassen.
#
# @return Numerischer Vektor mit Site Indizes.
#
# Die Werte werden mit mehr als einer Nachkommastellen ausgegeben.
# Rundung muss in aufrufender Funktion erfolgen.
#
# @examples
# funk_ekl2si(art="Kiefer", ekl=1.5)
# funk_ekl2si(art=c("fi", "fi", "bu", "dgl"), ekl=c(2.5, 0.3, 1, 2.4))

funk_ekl2si <- function(art, ekl) {
  art = sapply(art, art_code)
  if (length(art) == 1 & length(ekl) > 1) {
    art = rep(art, length(ekl))
  } else if (length(ekl) == 1 & length(art) > 1) {
    ekl = rep(ekl, length(art))
  } else if (length(art) != length(ekl)) {
    stop("Die \u00fcbergebenen Vektoren sind unterschiedlich lang!")
  }

  si = ifelse(art %in% c(110,111,112), 30 - 3 * ekl,
       ifelse(art == 211, 36.5 - 4 * ekl,
       ifelse(art == 511, 39 - 4 * ekl,
       ifelse(art == 611, 50 - 5 * ekl,
       ifelse(art == 711, 33 - 4 * ekl,
              NA)))))
  if (sum(art %in% c(110,111,112,211,511,611,711)) != length(art)) {
    warning("Es wurden unbekannte Baumarten \u00fcbergeben!") # never seen, art_code() stoppt vorher
  }

  # Bonitätsbereich [-3,7] checken
  si <- ifelse(ekl < -3 | ekl > 7, NA, si)

  return(si)
}


# Bestandesspitzenhöhen
#
# Liefert für eine gegebene Baumart und Bonität bei gegebenem Alter die
# Spitzenhöhe H100 (in m) gemäß ausgewählter Ertragstafeln (s. Details).
# Die Spitzenhöhe entspricht der Höhe des Grundflächenmittelstamms der 100
# durchmesserstärksten Bäume je ha und ist Maßstab für die Bonität eines
# gleichaltrigen (ggf. ideellen) Reinbestandes.
#
# @param art Vektor von Baumartencode, Kürzel, deutscher oder lateinischer Name
# @param alter Vektor mit Bestandesaltern in Jahren.
# @param bon Vektor mit Bonitäten, entweder Site Indix (abs Bonität) oder
#   Ertragsklasse (relative Bonität) bestimmt durch Parameter bon_als_ekl
# @param bon_als_ekl Bonität Ertragsklasse (TRUE) oder als Site Index (FALSE)
# @param hoss Wahrheitswert; wenn TRUE, wird alternativ ein auf der
#   Hossfeld-Funktion basierendes Modell genutzt
#
# @return Numerischer Vektor mit Spitzenhöhen H100 in m.
#
# Außerhalb des zulässigen Bonitätsbereichs [-3,7] wird NA zurückgegeben.
#
# Die Höhen werden mit mehr als einer Nachkommastellen ausgegeben.
# Rundung muss in aufrufender Funktion erfolgen.
#
# @examples
# funk_h100(art=c("fi", "fi", "bu", "dgl"), alter=80, bon= c(34.5, 29.3, 36, 40))
# funk_h100(art="ei", alter=seq(20, 160, by=20), bon=1.5, bon_als_ekl=TRUE, hoss=TRUE)

funk_h100 <- function(art, alter, bon, bon_als_ekl = FALSE, hoss = FALSE) {
  n.max = max(length(art), length(alter), length(bon))
  if (n.max > 1) {
    if (length(art) == 1) art = rep(art, n.max)
    if (length(alter) == 1) alter = rep(alter, n.max)
    if (length(bon) == 1) bon = rep(bon, n.max)
  }
  if (length(art) != length(alter) | length(art) != length(bon) | length(alter) != length(bon)) {
    stop("Die \u00fcbergebenen Vektoren sind unterschiedlich lang!")
  }
  if(hoss) message("Verwende das Hossfeld Modell.")
  art = sapply(art, art_code)
  h100 = ifelse(art %in% c(110,111,112), funk_h100_ei_juettner(alter, bon, bon_als_ekl),  # für Eiche gibt es kein Hossfeld-Modell
         ifelse(art == 211, funk_h100_bu_schober(alter, bon, bon_als_ekl, hoss),
         ifelse(art == 511, funk_h100_fi_wiedemann(alter, bon, bon_als_ekl, hoss),
         ifelse(art == 611, funk_h100_dgl_bergel(alter, bon, bon_als_ekl, hoss),
         ifelse(art == 711, funk_h100_ki_wiedemann(alter, bon, bon_als_ekl, hoss),
                NA)))))
  if (sum(art %in% c(110,111,112,211,511,611,711)) != length(art)) {
    warning("Es wurden unbekannte Baumarten \u00fcbergeben!")  # never seen, art_code() stoppt vorher
  }
  # Negative Alter führen zu NaN, es soll aber NA sein
  h100[is.na(h100)] <- NA

  # Bonitäten außerhalb des Extrapolationsbereichs [-3,7]
  if(bon_als_ekl){
    h100 <- ifelse(bon < -3 | bon >  7, NA, h100)
  } else {
    # vorgerechnete abs Bonitätsgrenzen in named vector
    art_c <- as.character(art)
    h100 <- unname(ifelse(bon < absbon_min_funk[art_c] |
                            bon > absbon_max_funk[art_c], NA, h100))
  }

  return(h100)
}


# Bestandesmittelhöhen
#
# Liefert für eine gegebene Baumart und Bonität bei gegebenem Alter die
# Mittelhöhe Hg (in m) gemäß ausgewählter Ertragstafeln (s. Details). Die
# Mittelhöhe entspricht der Höhe des Grundflächenmittelstamms und ist Maßstab
# für die Bonität eines gleichaltrigen (ggf. ideellen) Reinbestandes.
#
# @param art Vektor von Baumartencode, Kürzel, deutscher oder lateinischer Name
# @param alter Vektor mit Bestandesaltern in Jahren
# @param bon Vektor mit Bonitäten, entweder Site Indix (abs Bonität) oder
#   Ertragsklasse (relative Bonität) bestimmt durch Parameter bon_als_ekl
# @param bon_als_ekl Bonität Ertragsklasse (TRUE) oder als Site Index (FALSE)
#
# @return Numerischer Vektor mit Mittelhöhen in m
#
# Außerhalb des zulässigen Bonitätsbereichs [-3,7] wird NA zurückgegeben.
#
# Die Höhen werden mit mehr als einer Nachkommastellen ausgegeben.
# Rundung muss in aufrufender Funktion erfolgen.
#
# @examples
# funk_hg(art=c("fi", "fi", "bu", "dgl"), alter=80, bon=c(34.5, 29.3, 36, 40))
# funk_hg(art="ei", alter=seq(20, 160, by=20), bon=1.5, bon_als_ekl=TRUE)

funk_hg <- function(art, alter, bon, bon_als_ekl = FALSE) {
  n.max = max(length(art), length(alter), length(bon))
  if (n.max > 1) {
    if (length(art) == 1) art = rep(art, n.max)
    if (length(alter) == 1) alter = rep(alter, n.max)
    if (length(bon) == 1) bon = rep(bon, n.max)
  }
  if (length(art) != length(alter) | length(art) != length(bon) | length(alter) != length(bon)) {
    stop("Die \u00fcbergebenen Vektoren sind unterschiedlich lang!")
  }
  art = sapply(art, art_code)
  hg = ifelse(art %in% c(110,111,112) , funk_hg_ei_juettner(alter, bon, bon_als_ekl),
       ifelse(art == 211, funk_hg_bu_schober(alter, bon, bon_als_ekl),
       ifelse(art == 511, funk_hg_fi_wiedemann(alter, bon, bon_als_ekl),
       ifelse(art == 611, funk_hg_dgl_bergel(alter, bon, bon_als_ekl),
       ifelse(art == 711, funk_hg_ki_wiedemann(alter, bon, bon_als_ekl),
              NA)))))
  if (sum(art %in% c(110,111,112,211,511,611,711)) != length(art)) {
    warning("Es wurden unbekannte Baumarten \u00fcbergeben!") # never seen, art_code() stoppt vorher
  }

  # Negative Alter führen zu NaN, es soll aber NA sein
  hg[is.na(hg)] <- NA

  # Bonitäten außerhalb des Extrapolationsbereichs [-3,7]
  if(bon_als_ekl){
    hg <- ifelse(bon < -3 | bon >  7, NA, hg)
  } else {
    # vorgerechnete abs Bonitätsgrenzen in named vector
    art_c <- as.character(art)
    hg <- unname(ifelse(bon < absbon_min_funk[art_c] |
                          bon > absbon_max_funk[art_c], NA, hg))
  }

  return(hg)
}


# Bonitierung
#
# Liefert für eine gegebene Baumart und Mittel- oder Spitzenhöhe (H100 bzw. Hg)
# bei gegebenem Alter die absolute und relative Bonität eines gleichaltrigen
# (ggf. ideellen) Reinbestandes. Erstere entspricht dem Site Index (Spitzenhöhe
# H100 im Alter 100 in m), letztere der Ertragsklasse. Als Referenz dienen die
# in den Funktionen [funk_h100] und [funk_hg] implementierten Bonitätsfächer.
#
# @param art Vektor von Baumartencode, Kürzel, deutscher oder lateinischer Name
# @param alter Vektor mit Bestandesaltern in Jahren.
# @param h Vektor mit Bestandeshöhen in m, entweder als Spitzenhöhen H100 oder
#   Mittelhöhen Hg bestimmt durch Parameter h_als_hg
# @param h_als_hg Bestandeshöhe als Mittelhöhe (TRUE) oder Spitzenhöhe (FALSE)
# @param hoss Wahrheitswert; wenn TRUE, wird alternativ ein auf der
#   Hossfeld-Funktion basierendes Modell genutzt
# @param bon_typ "relativ" oder "absolut" Bonität ausgeben?
# @param kapp_na Bonitäten jenseits von -3 und 7 Ertragsklasse werden gekappt.
#   Wenn `TRUE`, wird für gekappt Werte `NA` ausgegeben, ansonsten -3 bzw. 7.
#
# @return numerischer Vektor der Bonitäten, je nach bon_typ "relativ" oder "absolut"
#
# Außerhalb des zulässigen Bonitätsbereichs [-3,7] wird gemäß Parameter kapp_na
# entweder NA oder der Grenzwert zurückgegeben.
#
# Für negative Alter wird NA zurückgeben.
#
# Die Werte werden mit mehr als einer Nachkommastellen ausgegeben.
# Rundung muss in aufrufender Funktion erfolgen.
#
# @examples
# funk_bonitieren(art="fi", alter=100, h=c(27, 31, 35))
#
# # Teilweise gekappte Bonitäten
# funk_bonitieren("fi", 100, c(55, 39, 35, 31, 27, 8))
# funk_bonitieren("bu", 180, c(43.9, 39.2, 34.6, 30.2, 25.9), h_als_hg=TRUE)

funk_bonitieren <- function(art, alter, h, h_als_hg = FALSE, hoss = FALSE,
                        bon_typ = "relativ", kapp_na = TRUE) {
  # Die Konstante diff.critical dient dazu festzustellen, ob die Bonität, die der
  # übergebenen Alter-Höhen-Kombination entspricht, besser oder schlechter ist als
  # die beste oder schlechteste Bonität des in der Funktion abgedeckten Bonitäts-
  # rahmens. Der beginnt bei der -3. und endet bei der 7. Ertragsklasse.
  # Hintergrund ist, dass sich die Bonität leider nur numerisch bestimmen lässt.
  # Dafür berechnet die Funktion mithilfe der Bonitätsfächerfunktionen funk_h100
  # bzw. funk_hg die Spitzen- oder Mittelhöhe, die bei dem gegebenen Alter den
  # Werten der Ertragsklassenreihe von -3 bis 7 mit einer Schrittweite von
  # 0,01 entspricht. Die gesuchte Ekl. ist dann jene, bei der die absolute
  # Differenz zwischen der so ermittelten Höhe und der übergebenen Höhe minimal
  # ist (ggf. wird die Ekl. anschließend noch in den Site Index umgerechnet).
  # Danach wird überprüft, ob diese minimale Höhendifferenz größer ist als
  # diff.critical. Dieser Schwellenwert wurde auf der Basis von Testläufen so
  # gewählt, dass er nur überschritten wird, wenn der Rahmen des geprüften Ekl.-
  # Vektors verlassen wird. Ohne diese Überprüfung würde man nicht bemerken, dass
  # die Bonität eigentlich besser oder schlechter ist als die zurückgegebene. Im
  # Fall der Schwellenwertüberschreitung wird eine Warnung ausgegeben, dass die
  # Bonität bei der -3. bzw. 7. Ekl. gekappt wurde.
  # -3 und 7 scheinen sinnvolle Grenzwerte zu sein, da die Bonitätsfächer darüber
  # hinaus tlw. keine plausiblen Höhenwerte mehr liefern und bessere und schlech-
  # tere Bonitäten extrem unwahrscheinlich sind. Wahrscheinlicher ist dann, dass
  # das Alter und/oder die Höhe nicht stimmt, worauf man anhand der Warnung und
  # der "Markierung" der zurückgelieferten gekappten Bonität aufmerksam gemacht
  # wird.
  diff.critical = 0.05
  n = max(length(art), length(alter), length(h))
  if (n > 1) {
    if (length(art) == 1) art = rep(art, n)
    if (length(alter) == 1) alter = rep(alter, n)
    if (length(h) == 1) h = rep(h, n)
  }
  if (length(art) != length(alter) | length(art) != length(h) | length(alter) != length(h)) {
    stop('Die \u00fcbergebenen Vektoren sind unterschiedlich lang!')
  }

  if(hoss) message("Verwende das Hossfeld Modell.")

  art = sapply(art, art_code)
  ekl = rep(NA, n)

  for (i in 1:length(art)) {
    if (!(art[i] %in% c(110,111,112,211,511,611,711))) {
      warning('Unbekannte Baumart!')  # never seen, art_code() stoppt vorher
    } else if (alter[i] > 0) {
      df <-  data.frame(ekl = seq(7, -3, by =-0.01))

      if (h_als_hg == TRUE) {
        df$h = funk_hg(art[i], alter[i], df$ekl, bon_als_ekl = TRUE)
      } else {
        df$h = funk_h100(art[i], alter[i], df$ekl, bon_als_ekl = TRUE, hoss)
      }

      df = df[!is.na(df$h) & !is.nan(df$h), ]  # Fehlerwerte löschen

      df$diff.abs = abs(df$h - h[i])
      diff.abs.min = min(df$diff.abs)

      if(length(idx <- which(diff.abs.min == df$diff.abs)) > 1){
        # Wenn der beste Wert exakt mittig zwischen zwei Zeilen liegt,
        # gibt es zwei Minima. Mitte/0.5 wird in R immer aufgerundet,
        # daher nehmen wir dann den zweiten/größeren => idx[2].
        ekl[i] <- df[idx[2], 'ekl']
      } else {
        # meistens ein Treffer
        ekl[i] <- df[idx, 'ekl']
      }

      if (diff.abs.min > diff.critical & (ekl[i] == -3 | ekl[i] == 7)) {
        if(isTRUE(kapp_na)){
          ekl[i] <- NA
          warning('Die Bestandesh\u00f6he ', h[i], ' im Alter ', alter[i],
                  " ergibt eine Bonit\u00e4t au\u00dferhalb des Extrapolationsbereiches [-3,7].",
                  " Da kapp_na=TRUE, wurde die Bonit\u00e4t auf NA gesetzt!")
        } else {
          warning('Die Bestandesh\u00f6he ', h[i], ' im Alter ', alter[i],
                  " ergibt eine Bonit\u00e4t au\u00dferhalb des Extrapolationsbereiches [-3,7].",
                  " Da kapp_na=FALSE, wurde die Bonit\u00e4t auf ", ekl[i], " gesetzt!")
        }
      }
    }
  }

  if(bon_typ == "absolut") ekl <- funk_ekl2si(art, ekl)

  return(ekl)
}



#===============================================================================
# Hilfsfunktionen (nur zu internen Verwendung)
#===============================================================================

funk_si2ekl_intern <- function(art, si) {
  art = sapply(art, art_code)
  if (length(art) == 1 & length(si) > 1) {
    art = rep(art, length(si))
  } else if (length(si) == 1 & length(art) > 1) {
    si = rep(si, length(art))
  } else if (length(art) != length(si) & length(art) > 1 & length(si) > 1) {
    stop("Die \u00fcbergebenen Vektoren sind unterschiedlich lang!")
  }

  ekl = ifelse(art %in% c(110,111,112), (31.18178 - si) / 3.54286,
        ifelse(art == 211, (36.16887 - si) / 3.81357,
        ifelse(art == 511, (38.99023 - si) / 3.86093,
        ifelse(art == 611, (50 - si) / 5,
        ifelse(art == 711, (33.00531 - si) / 3.88125,
               NA)))))
  if (sum(art %in% c(110,111,112,211,511,611,711)) != length(art)) {
    warning("Es wurden unbekannte Baumarten \u00fcbergeben!")  # never seen, art_code() stoppt vorher
  }
  return(ekl)
}


funk_ekl2si_intern <- function(art, ekl) {
  art = sapply(art, art_code)
  if (length(art) == 1 & length(ekl) > 1) {
    art = rep(art, length(ekl))
  } else if (length(ekl) == 1 & length(art) > 1) {
    ekl = rep(ekl, length(art))
  } else if (length(art) != length(ekl)) {
    stop("Die \u00fcbergebenen Vektoren sind unterschiedlich lang!")
  }

  si = ifelse(art %in% c(110,111,112), 31.18178 - 3.54286 * ekl,
       ifelse(art == 211, 36.16887 - 3.81357 * ekl,
       ifelse(art == 511, 38.99023 - 3.86093 * ekl,
       ifelse(art == 611, 50 - 5 * ekl,
       ifelse(art == 711, 33.00531 - 3.88125 * ekl,
              NA)))))
  if (sum(art %in% c(110,111,112,211,511,611,711)) != length(art)) {
    warning("Es wurden unbekannte Baumarten \u00fcbergeben!") # never seen, art_code() stoppt vorher
  }
  return(si)
}


funk_h100_ei_juettner <- function(alter, bon, bon_als_ekl = FALSE) {
  if (length(bon) != length(alter) & length(bon) > 1 & length(alter) > 1) {
    stop("Die \u00fcbergebenen Vektoren sind unterschiedlich lang!")
  }
  if (bon_als_ekl == TRUE) {
    si = funk_ekl2si(110, bon); ekl = funk_si2ekl_intern(110, si)
  } else {
    si = bon; ekl = funk_si2ekl_intern(110, si)
  }
  a = 0.92561 - 0.01578 * ekl
  b = 0.33513
  c = 0.79826
  return(65^a * (si / 65^a)^(exp(b / ((c-1) * alter^(c-1)) - b / ((c-1) * 100^(c-1)))))
}


funk_hg_ei_juettner <- function(alter, bon, bon_als_ekl = FALSE) {
  if (length(bon) != length(alter) & length(bon) > 1 & length(alter) > 1) {
    stop("Die \u00fcbergebenen Vektoren sind unterschiedlich lang!")
  }
  si = if (bon_als_ekl == TRUE) funk_ekl2si(110, bon) else bon
  hg.100 = 0.15295 + 0.96484 * si
  a = 0.81757 * exp(0.00417 * si)
  b = 0.44395
  c = 0.86702
  return(65^a * (hg.100 / 65^a)^(exp(b / ((c-1) * alter^(c-1)) - b / ((c-1) * 100^(c-1)))))
}


funk_h100_bu_schober <- function(alter, bon, bon_als_ekl = FALSE, hoss = FALSE) {
  if (length(bon) != length(alter) & length(bon) > 1 & length(alter) > 1) {
    stop("Die \u00fcbergebenen Vektoren sind unterschiedlich lang!")
  }
  if (bon_als_ekl == TRUE) {
    si = funk_ekl2si(211, bon); ekl = funk_si2ekl_intern(211, si)
  } else {
    si = bon; ekl = funk_si2ekl_intern(211, si)
  }
  if (hoss == TRUE) {
    a = 709.67192 * exp(0.55335 * ekl)
    b = 1.59965 + 0.12318 * ekl
    return(si * (alter^b / 100^b) * ((a + 100^b) / (a + alter^b)))
  } else {
    a = 0.97812 - 0.03621 * ekl
    b = 0.64567 * exp(-0.08788 * ekl)
    c = 0.90530 * exp(-0.05427 * ekl)
    return(65^a * (si / 65^a)^(exp(b / ((c-1) * alter^(c-1)) - b / ((c-1) * 100^(c-1)))))
  }
}


funk_hg_bu_schober <- function(alter, bon, bon_als_ekl = FALSE) {
  if (length(bon) != length(alter) & length(bon) > 1 & length(alter) > 1) {
    stop("Die \u00fcbergebenen Vektoren sind unterschiedlich lang!")
  }
  si = if (bon_als_ekl == TRUE) funk_ekl2si(211, bon) else bon
  hg.100 = -0.44875 + 0.96094 * si
  a = 0.62637 + 0.01058 * si
  b = 1.35085 * exp(0.00492 * si)
  c = 0.86060 * exp(0.00794 * si)
  return(65^a * (hg.100 / 65^a)^(exp(b / ((c-1) * alter^(c-1)) - b / ((c-1) * 100^(c-1)))))
}


funk_h100_fi_wiedemann <- function(alter, bon, bon_als_ekl = FALSE, hoss = FALSE) {
  if (length(bon) != length(alter) & length(bon) > 1 & length(alter) > 1) {
    stop("Die \u00fcbergebenen Vektoren sind unterschiedlich lang!")
  }
  if (bon_als_ekl == TRUE) {
    si = funk_ekl2si(511, bon); ekl = funk_si2ekl_intern(511, si)
  } else {
    si = bon; ekl = funk_si2ekl_intern(511, si)
  }
  if (hoss == TRUE) {
    a = 604.74833 * exp(0.50204 * ekl)
    b = 1.71066 + 0.07275 * ekl
    return(si * (alter^b / 100^b) * ((a + 100^b) / (a + alter^b)))
  } else {
    a = 0.91439 - 0.01607 * ekl
    b = 0.21512
    c = 0.51492 * exp(0.01636 * ekl)
    return(65^a * (si / 65^a)^(exp(b / ((c-1) * alter^(c-1)) - b / ((c-1) * 100^(c-1)))))
  }
}


funk_hg_fi_wiedemann <- function(alter, bon, bon_als_ekl = FALSE) {
  if (length(bon) != length(alter) & length(bon) > 1 & length(alter) > 1) {
    stop("Die \u00fcbergebenen Vektoren sind unterschiedlich lang!")
  }
  si = if (bon_als_ekl == TRUE) funk_ekl2si(511, bon) else bon
  hg.100 = -0.74618 + 0.99110 * si
  a = 0.72662 + 0.00469 * si
  b = 0.03840 + 0.00538 * si
  c = 0.36404 + 0.00485 * si
  return(65^a * (hg.100 / 65^a)^(exp(b / ((c-1) * alter^(c-1)) - b / ((c-1) * 100^(c-1)))))
}


funk_h100_dgl_bergel <- function(alter, bon, bon_als_ekl = FALSE, hoss = FALSE) {
  if (length(bon) != length(alter) & length(bon) > 1 & length(alter) > 1) {
    stop("Die \u00fcbergebenen Vektoren sind unterschiedlich lang!")
  }
  if (bon_als_ekl == TRUE) {
    si = funk_ekl2si(611, bon); ekl = funk_si2ekl_intern(611, si)
  } else {
    si = bon; ekl = funk_si2ekl_intern(611, si)
  }
  if (hoss == TRUE) {
    a = 198.39097 * exp(0.50463 * ekl)
    b = 1.36972 + 0.14338 * ekl
    return(si * (alter^b / 100^b) * ((a + 100^b) / (a + alter^b)))
  } else {
    a = -5.07385 * exp(0.45376 * ekl)
    b = -0.42897
    c = 1.72352 * exp(0.04812 * ekl)
    return(65^a * (si / 65^a)^(exp(b / ((c-1) * alter^(c-1)) - b / ((c-1) * 100^(c-1)))))
  }
}


funk_hg_dgl_bergel <- function(alter, bon, bon_als_ekl = FALSE) {
  if (length(bon) != length(alter) & length(bon) > 1 & length(alter) > 1) {
    stop("Die \u00fcbergebenen Vektoren sind unterschiedlich lang!")
  }
  si = if (bon_als_ekl == TRUE) funk_ekl2si(611, bon) else bon
  hg.100 = -1.28622 + 0.97104 * si
  a = 0.59593 + 0.00970 * si
  b = 1.93894 - 0.02086 * si
  c = 0.97071 + 0.00230 * si
  return(65^a * (hg.100 / 65^a)^(exp(b / ((c-1) * alter^(c-1)) - b / ((c-1) * 100^(c-1)))))
}


funk_h100_ki_wiedemann <- function(alter, bon, bon_als_ekl = FALSE, hoss = FALSE) {
  if (length(bon) != length(alter) & length(bon) > 1 & length(alter) > 1) {
    stop("Die \u00fcbergebenen Vektoren sind unterschiedlich lang!")
  }
  if (bon_als_ekl == TRUE) {
    si = funk_ekl2si(711, bon); ekl = funk_si2ekl_intern(711, si)
  } else {
    si = bon; ekl = funk_si2ekl_intern(711, si)
  }
  if (hoss == TRUE) {
    a = 188.08060 * exp(0.07021 * ekl)
    b = 1.37336
    return(si * (alter^b / 100^b) * ((a + 100^b) / (a + alter^b)))
  } else {
    a = 38.65338 - 4.36063 * ekl
    b = 0.02019
    return(a * (si/a)^(log(1 - exp(-b * alter))/log(1 - exp(-b * 100))))
  }
}


funk_hg_ki_wiedemann <- function(alter, bon, bon_als_ekl = FALSE) {
  if (length(bon) != length(alter) & length(bon) > 1 & length(alter) > 1) {
    stop("Die \u00fcbergebenen Vektoren sind unterschiedlich lang!")
  }
  si = if (bon_als_ekl == TRUE) funk_ekl2si(711, bon) else bon
  hg.100 = -1.34798 + 1.00400 * si
  tw = 52.52210 - 12.26742 * log(si)
  c = 1.57690 * exp(-0.00753 * si)
  return(hg.100 * (1 - exp(-log(c) * alter / tw))^c / (1 - exp(-log(c) * 100 / tw))^c)
}
