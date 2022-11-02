#===============================================================================
#
#  Teste Bestimmung der Bestandeshöhen mit et_hoehe()
#
#===============================================================================

#===============================================================================
#  Testet die Eingabe Validierung
#===============================================================================
expect_silent(et_hoehe(511, alter=80, bon=1.3))
expect_error(et_hoehe(511, alter="80", bon=1.3))
expect_error(et_hoehe(511, alter=TRUE, bon=1.3))
expect_error(et_hoehe(511, alter=80, bon="1.3"))
expect_error(et_hoehe(511, alter=80, bon=TRUE))


#===============================================================================
#  Testet die funktionalisierte Inter-/Extrapolation
#===============================================================================

# Teste Höhen-Ekl-Kombinationen, die bei der Tafelkonstruktion verwendet wurden
norm <-data.frame(
  Art = rep(c("ei", "bu", "fi", "dgl", "ki"), times=c(5, 5, 5, 4, 5)),
  H100 = c(33, 30, 27, 24, 21, 40.5, 36.5, 32.5, 28.5, 24.5, 43, 39, 35, 31, 27,
           50, 45, 40, 35, 37, 33, 29, 25, 21),
  Ekl = c(-1, 0, 1, 2, 3, -1, 0, 1, 2, 3, -1, 0, 1, 2, 3, 0, 1, 2, 3, -1, 0, 1, 2, 3))

ekl2h100  <- numeric(nrow(norm))
h1002h100 <- numeric(nrow(norm))

for(i in seq_len(nrow(norm))){
  ekl2h100[i] <- et_hoehe(norm[i, 'Art'], alter=100, bon=norm[i, 'Ekl'],
                           bon_typ='rel', hoehe_typ="ober", meth='funk')
  h1002h100[i] <- et_hoehe(norm[i, 'Art'], alter=100, bon=norm[i, 'H100'],
                           bon_typ='abs', hoehe_typ="ober", meth='funk')
}

expect_equal(norm$H100, ekl2h100)
expect_equal(norm$H100, h1002h100)

# Interpolation
expect_equal(et_hoehe('fi', alter=86, bon=2.3, bon_typ='rel', hoehe_typ='ober', meth='funk'),
             27.5)
expect_equal(et_hoehe('ei', alter=136, bon=0.6, bon_typ='rel', hoehe_typ='mittel', meth='funk'),
             30.9)
expect_equal(et_hoehe('bu', alter=76, bon=30.9, bon_typ='abs', hoehe_typ='ober', meth='funk'),
             26.1)
expect_equal(et_hoehe('ki', alter=81, bon=30.6, bon_typ='abs', hoehe_typ='mittel', meth='funk'),
             26.9)


# Extrapolation
# Bonität
expect_equal(et_hoehe('fi', alter=71, bon=-1.5, bon_typ='rel', hoehe_typ='mittel', meth='funk'),
             39.2)
expect_equal(et_hoehe('bu', alter=103, bon=41.7, bon_typ='abs', hoehe_typ='mittel', meth='funk'),
             40.2)
expect_equal(et_hoehe('ei', alter=133, bon=4.5, bon_typ='rel', hoehe_typ='ober', meth='funk'),
             19.6)
expect_equal(et_hoehe('dgl', alter=87, bon=22.0, bon_typ='abs', hoehe_typ='ober', meth='funk'),
             20.8)
# Alter
expect_equal(et_hoehe('dgl', alter=13, bon=2.4, bon_typ='rel', hoehe_typ='ober', meth='funk'),
             3.7)
expect_equal(et_hoehe('ki', alter=12, bon=25, bon_typ='abs', hoehe_typ='ober', meth='funk'),
             4.7)
expect_equal(et_hoehe('ei', alter=240, bon=1.3, bon_typ='rel', hoehe_typ='mittel', meth='funk'),
             34.4)
expect_equal(et_hoehe('fi', alter=140, bon=34.6, bon_typ='abs', hoehe_typ='mittel', meth='funk'),
             37.1)



# Vektorisiertes
expect_equal(et_hoehe(c('ei', 'bu', 'dgl'), alter=54, bon=1.3, bon_typ='rel',
                      hoehe_typ='ober', meth='funk'),
             c(18.9, 20.0, 31.9))
expect_equal(et_hoehe('fi', alter=seq(100,140,10), bon=34.6, bon_typ='abs',
                      hoehe_typ='mittel', meth='funk'),
             c(33.5, 34.7, 35.7, 36.4, 37.1))
expect_equal(et_hoehe('bu', alter=76, bon=seq(30.9,40,2), bon_typ='abs',
                      hoehe_typ='ober', meth='funk'),
             c(26.1, 28, 29.8, 31.7, 33.6))


# Außerhalb des Alters- & Bonitätsbereiches
# Alter > 0
expect_true(is.na(out <- et_hoehe('ei', alter=-1, bon=1.3, bon_typ='rel',
                                  hoehe_typ='mittel', meth='funk'))
            && !is.nan(out))
expect_true(is.na(out <- et_hoehe('fi', alter=-10, bon=34.6, bon_typ='abs',
                                  hoehe_typ='ober', meth='funk'))
            && !is.nan(out))

# Ertragsklassen -3 bis 7
expect_true(is.na(out <- et_hoehe('fi', alter=71, bon=-3.5, bon_typ='rel',
                                  hoehe_typ='mittel', meth='funk'))
            && !is.nan(out))
expect_true(is.na(out <- et_hoehe('bu', alter=103, bon=49.1, bon_typ='abs',
                                  hoehe_typ='mittel', meth='funk'))
            && !is.nan(out))
expect_true(is.na(out <- et_hoehe('dgl', alter=13, bon=7.5, bon_typ='rel',
                                  hoehe_typ='ober', meth='funk'))
            && !is.nan(out))
expect_true(is.na(out <- et_hoehe('ki', alter=12, bon=3, bon_typ='abs',
                                  hoehe_typ='ober', meth='funk'))
            && !is.nan(out))



#===============================================================================
#  Testet die klassische Inter-/Extrapolation mittels Dreisatz
#===============================================================================

# Teste Höhen-Ekl-Kombinationen, die bei der Tafelkonstruktion verwendet wurden
# Extrapolationsbereich (Fi: -1, 0, Dgl: 0, Ki: -1 Ekl) nicht getestet, weil
# dort bei klassischer Methode Abweichungen von der Norm (s. test_bontrans()).
norm <- norm[!(norm$Art == 'fi' & norm$Ekl %in% -1:0), ]
norm <- norm[!(norm$Art == 'dgl' & norm$Ekl == 0), ]
norm <- norm[!(norm$Art == 'ki' & norm$Ekl == -1), ]

ekl2h100  <- numeric(nrow(norm))
h1002h100 <- numeric(nrow(norm))

for(i in seq_len(nrow(norm))){
  ekl2h100[i] <- et_hoehe(norm[i, 'Art'], alter=100, bon=norm[i, 'Ekl'],
                          bon_typ='rel', hoehe_typ="ober", meth='klass')
  h1002h100[i] <- et_hoehe(norm[i, 'Art'], alter=100, bon=norm[i, 'H100'],
                           bon_typ='abs', hoehe_typ="ober", meth='klass')
}

expect_equal(norm$H100, ekl2h100)
expect_equal(norm$H100, h1002h100)


# Interpolation
expect_equal(et_hoehe('fi', alter=86, bon=2.3, bon_typ='rel', hoehe_typ='ober', meth='klass'),
             27.5)
expect_equal(et_hoehe('ei', alter=136, bon=0.6, bon_typ='rel', hoehe_typ='mittel', meth='klass'),
             30.8)
expect_equal(et_hoehe('bu', alter=76, bon=30.9, bon_typ='abs', hoehe_typ='ober', meth='klass'),
             26.2)
expect_equal(et_hoehe('ki', alter=81, bon=30.6, bon_typ='abs', hoehe_typ='mittel', meth='klass'),
             26.9)


# Extrapolation
# Bonität
expect_equal(et_hoehe('fi', alter=71, bon=-1.5, bon_typ='rel', hoehe_typ='mittel', meth='klass'),
             39.2)
expect_equal(et_hoehe('bu', alter=103, bon=41.7, bon_typ='abs', hoehe_typ='mittel', meth='klass'),
             40.3)
expect_equal(et_hoehe('ei', alter=133, bon=3.5, bon_typ='rel', hoehe_typ='ober', meth='klass'),
             22.6)
expect_equal(et_hoehe('dgl', alter=87, bon=31.1, bon_typ='abs', hoehe_typ='ober', meth='klass'),
             29.3)
# Alter
expect_equal(et_hoehe('dgl', alter=13, bon=2.4, bon_typ='rel', hoehe_typ='ober', meth='klass'),
             3.9)
expect_equal(et_hoehe('ki', alter=12, bon=25, bon_typ='abs', hoehe_typ='ober', meth='klass'),
             5.7)
expect_equal(et_hoehe('ei', alter=217, bon=1.3, bon_typ='rel', hoehe_typ='mittel', meth='klass'),
             34.0)
expect_equal(et_hoehe('fi', alter=142, bon=34.6, bon_typ='abs', hoehe_typ='mittel', meth='klass'),
             37.9)



# Vektorisiertes
expect_equal(et_hoehe(c('ei', 'bu', 'dgl'), alter=54, bon=1.3, bon_typ='rel',
                      hoehe_typ='ober', meth='klass'),
             c(18.9, 20.0, 31.9))
expect_equal(
  suppressMessages(et_hoehe('fi', alter=seq(100,140,10), bon=34.6, bon_typ='abs',
                      hoehe_typ='mittel', meth='klass')),
  c(33.5, 34.7, 35.7, 36.7, 37.7))
expect_equal(et_hoehe('bu', alter=76, bon=seq(30.9,40,2), bon_typ='abs',
                      hoehe_typ='ober', meth='klass'),
             c(26.2, 28, 29.9, 31.7, 33.6))


# Außerhalb des Alters- & Bonitätsbereiches
# zwischen 5 und max. zulässigem Alter (Ei 220, Bu 180 und Fi, Dgl, Ki 160)
expect_true(is.na(out <- et_hoehe('ei', alter=4, bon=1.3, bon_typ='rel',
                                  hoehe_typ='mittel', meth='klass'))
            && !is.nan(out))
expect_true(is.na(out <- et_hoehe('fi', alter=-1, bon=34.6, bon_typ='abs',
                                  hoehe_typ='mittel', meth='klass'))
            && !is.nan(out))
expect_true(is.na(out <- et_hoehe('ei', alter=225, bon=1.3, bon_typ='rel',
                                  hoehe_typ='mittel', meth='klass'))
            && !is.nan(out))
expect_true(is.na(out <- et_hoehe('fi', alter=180, bon=34.6, bon_typ='abs',
                                  hoehe_typ='mittel', meth='klass'))
            && !is.nan(out))

# Ertragsklassen -2 bis 4
expect_true(is.na(out <- et_hoehe('fi', alter=71, bon=-2.5, bon_typ='rel',
                                  hoehe_typ='mittel', meth='klass'))
            && !is.nan(out))
expect_true(is.na(out <- et_hoehe('bu', alter=103, bon=44.9, bon_typ='abs',
                                  hoehe_typ='mittel', meth='klass'))
            && !is.nan(out))
expect_true(is.na(out <- et_hoehe('dgl', alter=13, bon=4.5, bon_typ='rel',
                                  hoehe_typ='ober', meth='klass'))
            && !is.nan(out))
expect_true(is.na(out <- et_hoehe('ki', alter=12, bon=16.6, bon_typ='abs',
                                  hoehe_typ='ober', meth='klass'))
            && !is.nan(out))
