
# Neue Ertragstafel für das Paket aufbereiten
#
# Die Tafeln liegen als Excel-Dateien vor. Diese wurden baumartenweise in
# CSV-Dateien überführt (data-raw/tafeln_roh). Diese müssen noch in
# eine für das Paket geeignete Struktur gebracht werden (s.u.)
#
# Quelle:
# Albert M., Nagel J., Schmidt M., Nagel R.-V., Spellmann H. (2021):
# Eine neue Generation von Ertragstafeln für Eiche, Buche, Fichte, Douglasie
# und Kiefer [Datensatz]. Version 1.0. Zenodo.
# https://doi.org/10.5281/zenodo.6343906


# bisherigen internen Datensatz laden
# ------------------------------------------------------------------------------
load("R/sysdata.rda")


# Aus Excel gerettete CSVs einlesen
# ------------------------------------------------------------------------------
et_ei  <- read.csv("data-raw/tafeln_roh/et_ei.csv")
et_bu  <- read.csv("data-raw/tafeln_roh/et_bu.csv")
et_fi  <- read.csv("data-raw/tafeln_roh/et_fi.csv")
et_dgl <- read.csv("data-raw/tafeln_roh/et_dgl.csv")
et_ki  <- read.csv("data-raw/tafeln_roh/et_ki.csv")


# Aufbereiten
# ------------------------------------------------------------------------------
info <- list(
  Tafelname="",
  Baumart="",
  WissName="",
  ArtCodeNds="",
  Autor="Albert M., Nagel J., Schmidt M., Nagel R.-V., Spellmann H.",
  Jahr="2021",
  Quelle="Albert M., Nagel J., Schmidt M., Nagel R.-V., Spellmann H. (2021): Eine neue Generation von Ertragstafeln für Eiche, Buche, Fichte, Douglasie und Kiefer [Datensatz]. Version 1.0. Zenodo. https://doi.org/10.5281/zenodo.6343906",
  Region="Nordwestdeutschland"
)

info_ei <- info
info_ei$Tafelname <- "Eiche gestaffelte Durchforstung"
info_ei$Baumart <- "Eiche"
info_ei$ArtCodeNds <- "110"
info_ei$WissName <- "Quercus"

info_bu <- info
info_bu$Tafelname <- "Buche gestaffelte Durchforstung"
info_bu$Baumart <- "Buche"
info_bu$ArtCodeNds <- "211"
info_bu$WissName <- "Fagus sylvatica"

info_fi <- info
info_fi$Tafelname <- "Fichte gestaffelte Durchforstung"
info_fi$Baumart <- "Fichte"
info_fi$ArtCodeNds <- "511"
info_fi$WissName <- "Picea abies"

info_dgl <- info
info_dgl$Tafelname <- "Douglasie gestaffelte Durchforstung"
info_dgl$Baumart <- "Douglasie"
info_dgl$ArtCodeNds <- "611"
info_dgl$WissName <- "Pseudotsuga menziesii"

info_ki <- info
info_ki$Tafelname <- "Kiefer gestaffelte Durchforstung"
info_ki$Baumart <- "Kiefer"
info_ki$ArtCodeNds <- "711"
info_ki$WissName <- "Pinus sylvestris"


et <- list(
  '110' = list(info=info_ei,  tafel=et_ei),
  '211' = list(info=info_bu,  tafel=et_bu),
  '511' = list(info=info_fi,  tafel=et_fi),
  '611' = list(info=info_dgl, tafel=et_dgl),
  '711' = list(info=info_ki,  tafel=et_ki)
)



df <- unlist(info_ei)
df <- rbind(df, unlist(info_bu))
df <- rbind(df, unlist(info_fi))
df <- rbind(df, unlist(info_dgl))
df <- rbind(df, unlist(info_ki))
df <- as.data.frame(df, row.names = 1:nrow(df))
liste <- df[,c( "Baumart", "ArtCodeNds", "Tafelname", "Jahr", "Region")]

rm(df, et_bu, et_dgl, et_ei, et_fi, et_ki, info, info_bu, info_dgl, info_ei, info_fi, info_ki)


# als internen Datensatz speichern
# ------------------------------------------------------------------------------
#
# der interne Datensatz enthält:
#  - absbon_{max,min}_{funk,klas}
#  - art_tabelle
#  - et
#  - liste
#  - max_alter_klass

save(absbon_min_funk, absbon_min_klas, absbon_max_funk, absbon_max_klas,
     art_tabelle, et, liste, max_alter_klass,
     file="R/sysdata.rda", compress="bzip2", version=3)
