#===============================================================================
#
#  Teste Ertragstafel Interpolation mit et_tafel()
#
#===============================================================================
#  Es gibt zur Zeit nur die klassische Methode der Interpolation

#===============================================================================
#  Testet die Eingabe Validierung
#===============================================================================
expect_silent(et_tafel(211, alter=NULL, bon=NULL))
expect_silent(et_tafel(211, alter=45, bon=NULL))
expect_silent(et_tafel(211, alter=NULL, bon=1.2))
expect_silent(et_tafel(211, alter=45, bon=1.2))

expect_error(et_tafel(211,  alter="0", bon=NULL))
expect_error(et_tafel(211,  alter="a", bon=NULL))
expect_error(et_tafel(211,  alter=TRUE, bon=NULL))
expect_error(et_tafel(211,  alter=NULL, bon="2"))
expect_error(et_tafel(211,  alter=NULL, bon="b"))
expect_error(et_tafel(211,  alter=NULL, bon=TRUE))
expect_error(et_tafel(211,  alter="0", bon="1"))
expect_error(et_tafel(211,  alter="a", bon="b"))
expect_error(et_tafel(211,  alter=TRUE, bon=TRUE))


#===============================================================================
#  Testet die klassische Inter-/Extrapolation mittels Dreisatz
#===============================================================================

# Gebe ganze Tafel aus (ca 100 Zeilen) -----------------------------------------
# zieht nur die entsprechende Tafel aus dem Datensatz, keine Berechnung
wahre_buche <- et.nwfva:::et[["211"]]$tafel

expect_equal(et_tafel(211), wahre_buche)
expect_equal(et_tafel('Bu'), wahre_buche)
expect_equal(et_tafel('Fagus syl'), wahre_buche)


# Gebe Zeilen zu bestimmten Alter aus ------------------------------------------
# nur Inter-/Extrapolation des Alters

# Vergleichsdaten aus der Tafel für Überprüfung der Alterstreffer
ki <- et.nwfva:::et[["711"]]$tafel

# ausschließlich Treffer (nur Selektion von Zeilen aus dem Datensatz)
expect_equivalent(et_tafel(711, alter=50), ki[ki$Alter == 50, ])
expect_equivalent(et_tafel(711, alter=90), ki[ki$Alter == 90, ])

# enthält Treffer und Interpoliertes
expect_equivalent(sum(
  is.element(et_tafel(711, alter=100)$Ekl, ki[ki$Alter == 100, ]$Ekl)),
  nrow(ki[ki$Alter == 100, ]))
expect_equivalent(sum(
  is.element(et_tafel(711, alter=25)$Ekl, ki[ki$Alter == 25, ]$Ekl)),
  nrow(ki[ki$Alter == 25, ]))

# Interpolation des Alters
expect_equal(et_tafel(711, alter=53),
             structure(list(
               Ekl = c(-1, 0, 1, 2, 3),
               Alter = c(53, 53, 53, 53, 53),
               N = c(391, 531, 753, 1148, 1797),
               Hg = c(26.2, 23, 19.8, 16.6, 13.5),
               H100 = c(27.4, 24.2, 21.1, 17.9, 14.7),
               G = c(33.8, 31.7, 29.1, 26, 22.1),
               Dg = c(32.1, 26.8, 21.8, 17.1, 13.2),
               Dw = c(36.4, 33.5, 30.2, 26.8, 22.9),
               V = c(352, 302.6, 252.2, 202.8, 154.6),
               N_aus = c(51, 85, 151, 214, 369),
               G_aus = c(3.9, 4.1, 4.2, 4.2, 4),
               Dg_aus = c(31.5, 24.9, 19, 15.9, 11.8),
               V_aus = c(44.6, 41.2, 37.8, 32.4, 27),
               iV = c(15.1, 13.7, 12, 10.3, 8.5),
               GWL = c(838.8, 715.2, 542.4, 397.6, 250.2),
               dGZ = c(15.8, 13.5, 10.2, 7.5, 4.7)),
               row.names = c(NA, 5L), class = "data.frame"))

# Extrapolation des Alters
expect_message(dat <- et_tafel(711, alter=16))
expect_equal(dat,
             structure(list(
               Ekl = c(-1, 0, 1, 2, 3),
               Alter = c(16, 16, 16, 16, 16),
               N = c(3156, 4370, 4844, 5659, 5879),
               Hg = c(9.7, 8.1, 6.9, 5.7, 4.8),
               H100 = c(11, 9.4, 8.2, 7, 6.1),
               G = c(18.4, 15.5, 13.6, 11.5, 9.9),
               Dg = c(7.7, 6.3, 4.8, 3.4, 2.8),
               Dw = c(10.9, 10.4, 9.9, 9.4, 9.2),
               V = c(79.4, 66.4, 53, 37.8, 30.4),
               N_aus = c(2480, 2928, 2616, 2435, 1895),
               G_aus = c(17.2, 13.9, 10.8, 7.5, 6.1),
               Dg_aus = c(8.3, 7, 5.4, 4.2, 2.3),
               V_aus = c(95.4, 68.6, 49, 32.2, 21.2),
               iV = c(28.3, 22, 17.7, 13.2, 10.1),
               GWL = c(103.6, 80.4, 15.6, 0, 0),
               dGZ = c(7.8, 6.1, 3.3, 1.9, 0)),
               row.names = c(NA, 5L), class = "data.frame"), tol=0.01)

expect_message(dat <- et_tafel(711, alter=125))
expect_equal(dat,
             structure(list(
               Ekl = c(-1, 0, 1, 2, 3),
               Alter = c(125, 125, 125, 125, 125),
               N = c(157, 232, 290, 368, 504),
               Hg = c(40, 34.1, 29.9, 25.6, 21.4),
               H100 = c(41.2, 35.3, 31.1, 26.8, 22.7),
               G = c(38.5, 36.1, 35, 33.5, 31),
               Dg = c(54.1, 45.3, 39.6, 34.2, 28.6),
               Dw = c(75.9, 66.4, 57.9, 49.6, 41.5),
               V = c(693, 572, 483, 396, 313),
               N_aus = c(0, 8, 9, 14, 22),
               G_aus = c(1.2, 1.4, 1.6, 1.9, 1.9),
               Dg_aus = c(63.6, 51.3, 46.5, 41.3, 33.2),
               V_aus = c(13, 22, 20, 21, 18),
               iV = c(6.2, 6.8, 6.4, 6, 5.3),
               GWL = c(1615, 1385, 1158, 951, 728),
               dGZ = c(12, 11.2, 9.3, 7.7, 5.7)),
               row.names = c(NA, 5L), class = "data.frame"), tol = 0.01)



# Gebe Zeilen zu bestimmter Ertragsklasse aus ----------------------------------

# Vergleichsdaten aus der Tafel für Überprüfung der Ertragsklassentreffer
fi <- et.nwfva:::et[["511"]]$tafel

# ausschließlich Treffer (nur Selektion von Zeilen aus dem Datensatz)
expect_equivalent(et_tafel(511, bon=-1),  fi[fi$Ekl == -1, ])
expect_equivalent(et_tafel(511, bon=2), fi[fi$Ekl == 2, ])
expect_equivalent(et_tafel(511, bon=27, bon_typ='abs'), fi[fi$Ekl == 3, ])

# Interpolation
expect_equal(head(et_tafel(511, bon=1.3), 3),
             structure(list(
               Ekl = c(1.3, 1.3, 1.3),
               Alter = c(30, 35, 40),
               N = c(1797, 1303, 1016),
               Hg = c(11.9, 14.5, 16.8),
               H100 = c(12.8, 15.4, 17.8),
               G = c(19.2, 23, 26.3),
               Dg = c(12, 15.4, 18.6),
               Dw = c(18.1, 21.8, 25.3),
               V = c(109, 155.7, 205.4),
               N_aus = c(802, 494, 287),
               G_aus = c(5.1, 4.6, 4.1),
               Dg_aus = c(9.1, 11, 14),
               V_aus = c(27.9, 29.9, 31.9),
               iV = c(14.1, 15.5, 16.2),
               GWL = c(179.6, 257.2, 338.1),
               dGZ = c(6, 7.4, 8.5)),
               row.names = c(NA, 3L), class = "data.frame"))
expect_equal(tail(et_tafel(511, bon=1.3), 3),
             structure(list(
               Ekl = c(1.3, 1.3, 1.3),
               Alter = c(100, 105, 110),
               N = c(394, 370, 347),
               Hg = c(32.7, 33.4, 33.9),
               H100 = c(33.8, 34.4, 35),
               G = c(48.8, 49.8, 50.9),
               Dg = c(40, 41.6, 43.4),
               Dw = c(52.5, 53.9, 55.2),
               V = c(697, 724.5, 750.3),
               N_aus = c(27, 25, 24),
               G_aus = c(2.9, 2.8, 2.8),
               Dg_aus = c(38, 39.3, 40.2),
               V_aus = c(39.1, 40.1, 40.1),
               iV = c(13.8, 13.5, 13.1),
               GWL = c(1265.1, 1332.7, 1397.6),
               dGZ = c(12.7, 12.7, 12.7)),
               row.names = 15:17, class = "data.frame"))

expect_equal(head(et_tafel(711, bon=35, bon_typ='abs'), 3),
             structure(list(
               Ekl = c(-0.5,-0.5,-0.5),
               Alter = c(20, 25, 30),
               N = c(2937, 1869, 1291),
               Hg = c(11, 13.6, 15.9),
               H100 = c(12.3, 14.9, 17.2),
               G = c(19.6, 23.1, 25.7),
               Dg = c(9.5, 12.5, 15.7),
               Dw = c(13.4, 16.9, 20.4),
               V = c(104.1, 143.5, 180.3),
               N_aus = c(1983, 1067, 578),
               G_aus = c(13.4, 10.8, 8.7),
               Dg_aus = c(9.3, 11.4, 14),
               V_aus = c(77.5, 72.6, 65.7),
               iV = c(23.8, 22.4, 20.6),
               GWL = c(181, 293, 395.7),
               dGZ = c(9.1, 11.7, 13.2)),
               row.names = c(NA, 3L), class = "data.frame"))
expect_equal(tail(et_tafel(711, bon=35, bon_typ='abs'), 3),
             structure(list(
               Ekl = c(-0.5,-0.5,-0.5),
               Alter = c(80, 85, 90),
               N = c(296, 282, 268),
               Hg = c(30.8, 31.6, 32.4),
               H100 = c(32, 32.8, 33.6),
               G = c(35.9, 36.2, 36.4),
               Dg = c(39.2, 40.5, 41.8),
               Dw = c(49.7, 52.2, 54.7),
               V = c(458.9, 479.3, 499.7),
               N_aus = c(19, 16, 13),
               G_aus = c(2.3, 2.1, 2),
               Dg_aus = c(40.1, 42.4, 44.6),
               V_aus = c(30, 28.5, 27),
               iV = c(10.4, 9.8, 9.4),
               GWL = c(1092.9, 1142.3, 1189.2),
               dGZ = c(13.7, 13.5, 13.2)),
               row.names = 13:15, class = "data.frame"))

# Extrapolation
expect_equal(head(et_tafel(511, bon=-1.5), 3),
             structure(list(
               Ekl = c(-1.5,-1.5,-1.5),
               Alter = c(20, 25, 30),
               N = c(978, 588, 494),
               Hg = c(14.1, 18.2, 22),
               H100 = c(15.1, 19.1, 23),
               G = c(24.1, 28.8, 33.1),
               Dg = c(16.3, 22.9, 27.8),
               Dw = c(19.5, 24.8, 29.8),
               V = c(173.5, 248, 320.5),
               N_aus = c(978, 390, 94),
               G_aus = c(5.8, 5.7, 5.3),
               Dg_aus = c(8.6, 12.7, 20.9),
               V_aus = c(37, 47.5, 53),
               iV = c(21.8, 24.3, 25.2),
               GWL = c(232, 352.5, 480),
               dGZ = c(11.7, 14.1, 16)),
               row.names = c(NA, 3L), class = "data.frame"))
expect_equal(tail(et_tafel(511, bon=-1.5), 3),
             structure(list(
               Ekl = c(-1.5,-1.5,-1.5),
               Alter = c(65, 70, 75),
               N = c(302, 296, 290),
               Hg = c(37.8, 39, 40),
               H100 = c(38.9, 40.1, 41.2),
               G = c(52.2, 54.3, 56.2),
               Dg = c(46, 47.8, 49.2),
               Dw = c(56.5, 59.2, 61.9),
               V = c(783.5, 838, 893),
               N_aus = c(8, 6, 6),
               G_aus = c(3.2, 3.1, 2.8),
               Dg_aus = c(56.4, 61.3, 64.8),
               V_aus = c(52, 52, 50.5),
               iV = c(22.2, 21.3, 20.7),
               GWL = c(1323.5, 1430.5, 1534),
               dGZ = c(20.3, 20.4, 20.4)),
               row.names = 10:12,class = "data.frame"))


# Gebe Zeile mit bestimmter Ekl und Alter aus (1 Zeile) ------------------------
# Im Grunde dasselbe wie bei der Bonitierung:
# erst das Alter, dann die Ertragsklasse {Treffer, Inter-/Extrapolation}

# Vergleichsdaten aus der Tafel für Überprüfung der Alterstreffer
ei <- et.nwfva:::et[["110"]]$tafel

# Treffer Alter & Ertragsklasse
# nur Selektion einer Zeile aus dem Datensatz
expect_equivalent(et_tafel('ei', alter=80, bon=1),
                  ei[ei$Ekl == 1 & ei$Alter == 80, ])
expect_equivalent(et_tafel('ei', alter=105, bon=33, bon_typ='abs'),
                  ei[ei$Ekl == -1 & ei$Alter == 105, ])

# Treffer Alter & Interpolation Ekl
expect_equal(et_tafel('ei', alter=60, bon=-0.5),
             structure(list(Ekl=-0.5, Alter=60, N=417, Hg=24.1, H100=25.4,
                            G=21.9, Dg=25.9, Dw=35.5, V=285, N_aus=60, G_aus=3,
                            Dg_aus=25.2, V_aus=38, iV=12.1, GWL=608.5, dGZ=10.1),
                       row.names=1L, class="data.frame"))

# Treffer Alter & Extrapolation Ekl
expect_equal(et_tafel('ei', alter=60, bon=-1.7),
             structure(list(Ekl=-1.7, Alter=60, N=364, Hg=27.5, H100=28.9,
                            G=23.7, Dg=28.6, Dw=41.7, V=349.8, N_aus=50, G_aus=3.5,
                            Dg_aus=29.7, V_aus=50, iV=14.7, GWL=813.7, dGZ=13.6),
                       row.names=1L, class="data.frame"))

# Interpolation Alter & Treffer Ekl
expect_equal(et_tafel('ei', alter=42, bon=1),
             structure(list(Ekl=1, Alter=42, N=883, Hg=15.5, H100=16.8,
                            G=16.6, Dg=15.5, Dw=19.9, V=124, N_aus=193, G_aus=2.6,
                            Dg_aus=13.1, V_aus=20.4, iV=8.9, GWL=213, dGZ=5.1),
                       row.names=1L, class="data.frame"))

# Extrapolation Alter & Treffer Ekl
expect_message(werte <- et_tafel('ei', alter=200, bon=1))
expect_equal(werte,
             structure(list(Ekl=1, Alter=200, N=68, Hg=33.8, H100=33.8,
                            G=28.5, Dg=72, Dw=85.6, V=491, N_aus=0, G_aus=1.5,
                            Dg_aus=69.5, V_aus=26, iV=5.8, GWL=1397, dGZ=7.1),
                       row.names=1L, class="data.frame"))

# Extrapolation Alter & Extrapolation Ekl
expect_message(werte <- et_tafel('ei', alter=200, bon=-1.3))
expect_equal(werte,
             structure(list(Ekl=-1.3, Alter=200, N=56, Hg=40.4, H100=41.1,
                            G=31.3, Dg=81, Dw=106.5, V=594.2, N_aus=4, G_aus=0.8,
                            Dg_aus=76.4, V_aus=26.6, iV=6.9, GWL=2065.7, dGZ=10.2),
                       row.names=1L, class="data.frame"))
expect_message(werte <- et_tafel('ei', alter=190, bon=35.1, bon_typ='abs'))
expect_equal(werte,
             structure(list(Ekl=-1.7, Alter=190, N=62, Hg=40.9, H100=41.7,
                            G=31.9, Dg=78.9, Dw=105.6, V=601.8, N_aus=4, G_aus=1,
                            Dg_aus=75.8, V_aus=29.4, iV=7.3, GWL=2109.1, dGZ=11),
                       row.names=1L, class="data.frame"))



# Überschreitung des Extrapolationsbereiches -----------------------------------

# außerhalb des Bonitätsbereichs [-2,4]
expect_error(et_tafel(611, alter=50, bon=-2.3))
expect_error(et_tafel(611, alter=50, bon=5))
expect_error(et_tafel(711, alter=50, bon=43, bon_typ='abs'))
expect_error(et_tafel(711, alter=50, bon=15.4, bon_typ='abs'))

# außerhalb des Altersbereichs [5,160]
expect_error(et_tafel(611, alter=4, bon=1))
expect_error(et_tafel(611, alter=161, bon=1))
