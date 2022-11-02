#===============================================================================
#
#  Teste Bonitätsumrechnung mit et_bontrans()
#
#===============================================================================
#  Sowohl funktionale als auch klassische Methode.


norm <-data.frame(
  Art = rep(c("ei", "bu", "fi", "dgl", "ki"), times=c(5, 5, 5, 4, 5)),
  H100 = c(33, 30, 27, 24, 21, 40.5, 36.5, 32.5, 28.5, 24.5, 43, 39, 35, 31, 27,
           50, 45, 40, 35, 37, 33, 29, 25, 21),
  Ekl = c(-1, 0, 1, 2, 3, -1, 0, 1, 2, 3, -1, 0, 1, 2, 3, 0, 1, 2, 3, -1, 0, 1, 2, 3))


#===============================================================================
#  Testet die Eingabe Validierung
#===============================================================================
expect_silent(et_bontrans("Ei", bon=32))
expect_error(et_bontrans("Ei",  bon="0"))
expect_error(et_bontrans("Ei",  bon="a"))
expect_error(et_bontrans("Ei",  bon=TRUE))


#===============================================================================
#  Testet die funktionalisierte Inter-/Extrapolation
#===============================================================================
res <- data.frame(Art=character(nrow(norm)), H100=numeric(nrow(norm)),  Ekl=numeric(nrow(norm)))

# Treffer  Ekl <-> SI
for(i in seq_len(nrow(norm))){
  res[i, 'Art']  <- norm[i, 'Art']
  res[i, 'Ekl']  <- et_bontrans(art=norm[i, 'Art'], bon=norm[i, 'H100'],
                                richtung="abs_zu_rel", meth="funk")
  res[i, 'H100'] <- et_bontrans(art=norm[i, 'Art'], bon=norm[i, 'Ekl'],
                                richtung="rel_zu_abs", meth="funk")
}
expect_equivalent(norm, res)


# Interpolation
expect_equal(et_bontrans('ei', bon=-0.7, richtung="rel_zu_abs", meth="funk"), 32.1)
expect_equal(et_bontrans('ei', bon=32.1, richtung="abs_zu_rel", meth="funk"), -0.7)

expect_equal(et_bontrans('ki', bon=2.4, richtung="rel_zu_abs", meth="funk"), 23.4)
expect_equal(et_bontrans('ki', bon=23.4, richtung="abs_zu_rel", meth="funk"), 2.4)


# Extrapolation
expect_equal(et_bontrans('fi', bon=-1.7, richtung="rel_zu_abs", meth="funk"), 45.8)
expect_equal(et_bontrans('fi', bon=48.5, richtung="abs_zu_rel", meth="funk"), -2.4)

expect_equal(et_bontrans('bu', bon=2.7, richtung="rel_zu_abs", meth="funk"), 25.7)
expect_equal(et_bontrans('bu', bon=25.7, richtung="abs_zu_rel", meth="funk"), 2.7)


# Vektorisiertes
expect_equal(et_bontrans('ki', bon=c(0.3, 1.1,2.4), richtung="rel_zu_abs", meth="funk"),
             c(31.8, 28.6, 23.4))
expect_equal(et_bontrans(c('bu', 'ki', 'fi'), bon=2.4, richtung="rel_zu_abs", meth="funk"),
             c(26.9, 23.4, 29.4))
expect_equal(et_bontrans(c('bu', 'ki', 'fi'), bon=c(0.1, 1.2, 2.3), richtung="rel_zu_abs", meth="funk"),
             c(36.1, 28.2, 29.8))

expect_equal(et_bontrans('ki', bon=c(23.4, 25.1, 27.0), richtung="abs_zu_rel", meth="funk"),
             c(2.4, 2, 1.5))
expect_equal(et_bontrans(c('ei', 'ki', 'dgl'), bon=26.6, richtung="abs_zu_rel", meth="funk"),
             c(1.1, 1.6, 4.7))
expect_equal(et_bontrans(c('ei', 'ki', 'dgl'), bon=c(26.6, 28.2, 35.6), richtung="abs_zu_rel", meth="funk"),
             c(1.1, 1.2, 2.9))


# Außerhalb des Bonitätsbereiches
expect_true(is.na(et_bontrans('dgl', bon=-3.3, richtung="rel_zu_abs", meth="funk")))
expect_true(is.na(et_bontrans('dgl', bon=75, richtung="abs_zu_rel", meth="funk")))




#===============================================================================
#  Testet die klassische Inter-/Extrapolation mittels Dreisatz
#===============================================================================

# Treffer  Ekl <-> SI

# Die tabellierten Werte der Ertragstafel enden für das Nadelholz in den guten
# Ertragsklassen schon vor dem Alter 100. Das führt dazu, dass auch die Werte
# für die Bonitätsumrechnung extrapolierte werden müssen. Die klassische Methode
# der Extrapolation mittels Dreisatz zeigt hier zum Teil leider Abweichungen von
# den bei der Konstruktion der Tafeln vorgegebenen Zusammenhänge von H100 und
# Ekl.
# Dies wird besonders sichtbar bei der Umwandlung rel_zu_abs, da dort nach
# Rundung auf eine Nachkommastelle durch Werte im Zehnerbereich (Ekl liegt im
# Einerbereich) ein Stelle mehr zur Verfügung steht.
# Deswegen wird der Extrapolationsbereich (Fi: -1, 0, Dgl: 0, Ki: -1 Ekl) nicht
# getestet.

norm <- norm[!(norm$Art == 'fi' & norm$Ekl %in% -1:0), ]
norm <- norm[!(norm$Art == 'dgl' & norm$Ekl == 0), ]
norm <- norm[!(norm$Art == 'ki' & norm$Ekl == -1), ]

res <- data.frame(Art=character(nrow(norm)), H100=numeric(nrow(norm)),  Ekl=numeric(nrow(norm)))

for(i in seq_len(nrow(norm))){
  res[i, 'Art']  <- norm[i, 'Art']
  res[i, 'Ekl']  <- et_bontrans(art=norm[i, 'Art'], bon=norm[i, 'H100'],
                                richtung="abs_zu_rel", meth="klass")
  res[i, 'H100'] <- et_bontrans(art=norm[i, 'Art'], bon=norm[i, 'Ekl'],
                                richtung="rel_zu_abs", meth="klass")
}
expect_equivalent(norm, res)


# Interpolation
expect_equal(et_bontrans('ei', bon=-0.7, richtung="rel_zu_abs", meth="klass"), 32.1)
expect_equal(et_bontrans('ei', bon=32.1, richtung="abs_zu_rel", meth="klass"), -0.7)

expect_equal(et_bontrans('ki', bon=2.4, richtung="rel_zu_abs", meth="klass"), 23.4)
expect_equal(et_bontrans('ki', bon=23.4, richtung="abs_zu_rel", meth="klass"), 2.4)


# Extrapolation
expect_equal(et_bontrans('fi', bon=-1.7, richtung="rel_zu_abs", meth="klass"), 48.5)
expect_equal(et_bontrans('fi', bon=48.5, richtung="abs_zu_rel", meth="klass"), -1.7)

expect_equal(et_bontrans('bu', bon=2.7, richtung="rel_zu_abs", meth="klass"), 25.7)
expect_equal(et_bontrans('bu', bon=25.7, richtung="abs_zu_rel", meth="klass"), 2.7)


# Vektorisiertes
expect_equal(et_bontrans('ki', bon=c(0.3, 1.1,2.4), richtung="rel_zu_abs", meth="klass"),
             c(31.8, 28.6, 23.4))
expect_equal(et_bontrans(c('bu', 'ki', 'fi'), bon=2.4, richtung="rel_zu_abs", meth="klass"),
             c(26.9, 23.4, 29.4))
expect_equal(et_bontrans(c('bu', 'ki', 'fi'), bon=c(0.1, 1.2, 2.3), richtung="rel_zu_abs", meth="klass"),
             c(36.1, 28.2, 29.8))

expect_equal(et_bontrans('ki', bon=c(23.4, 25.1, 27.0), richtung="abs_zu_rel", meth="klass"),
             c(2.4, 2, 1.5))
expect_equal(et_bontrans(c('ei', 'ki', 'dgl'), bon=26.6, richtung="abs_zu_rel", meth="klass"),
             c(1.1, 1.6, NA))
expect_equal(et_bontrans(c('ei', 'ki', 'dgl'), bon=c(26.6, 28.2, 35.6), richtung="abs_zu_rel", meth="klass"),
             c(1.1, 1.2, 2.9))


# Außerhalb des Bonitätsbereiches
expect_true(is.na(et_bontrans('dgl', bon=-2.7, richtung="rel_zu_abs", meth="klass")))
expect_true(is.na(et_bontrans('dgl', bon=65, richtung="abs_zu_rel", meth="klass")))
