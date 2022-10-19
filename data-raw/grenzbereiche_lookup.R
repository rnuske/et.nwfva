#===============================================================================
#
#  Erzeuge Lookup Table mit den Grenzen für Alters- & Bonitätsbereich
#
#  Umsetzung hier über named vectors. Lookup dann über absbon_max_klas['211']
#===============================================================================

arten <- c(110, 111, 112, 211, 511, 611, 711)


# Grenzen für Altersbereich der klassischen Interpolation
#-------------------------------------------------------------------------------
# maximaler Altersextrapolationsbereich gemäß M. Albert

#                    Ei   SEi  TEi  Bu   Fi   Dgl  Ki
max_alter_klass <- c(220, 220, 220, 180, 160, 160, 160)
names(max_alter_klass) <- arten


# Grenzen für absolute Bonitäten
#-------------------------------------------------------------------------------

# Die Umrechnung von ekl zu si erfolgt getrennt nach funkt und klass, da
# im Nadelholz bei den guten Bonitäten die Höhen im Alter 100 sehr
# unterschiedlich extrapoliert werden. klass überschätzt dort extrem.
# Die Umrechnung von ekl zu si soll aber innerhalb einer Methode konsistent sein.


absbon_min_funk <- et.nwfva:::funk_ekl2si(art=arten, ekl=7)
names(absbon_min_funk) <- arten

absbon_min_klas = et.nwfva:::klas_ekl2si(art=arten, ekl=4)
names(absbon_min_klas) <- arten

absbon_max_funk <- et.nwfva:::funk_ekl2si(art=arten, ekl=-3)
names(absbon_max_funk) <- arten

absbon_max_klas <-  suppressWarnings(et.nwfva:::klas_ekl2si(art=arten, ekl=-2))
names(absbon_max_klas) <- arten



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
