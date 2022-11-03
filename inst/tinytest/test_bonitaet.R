#===============================================================================
#
#  Teste Bonitierung mit et_bonitaet()
#
#===============================================================================
#  Sowohl funktionale als auch klassische Methode.
#
#  Unterstützung bei der Konstruktion von Testfällen bieten die Bonitätsfächer
#  aus data-raw/bonitaetsfaecher_fuer_tests.R


#===============================================================================
#  Testet die Eingabe Validierung
#===============================================================================
expect_error(et_bonitaet()) # verpflichtend art, alter, hoehe
expect_error(et_bonitaet('fi'))
expect_error(et_bonitaet('fi', alter=50))
expect_error(et_bonitaet('fi', hoehe=25))
expect_error(et_bonitaet(art='fi', alter=50, hoehe=25, hoehe_typ="spitz"))
expect_error(et_bonitaet(art='fi', alter=50, hoehe=25, methode="modern"))
expect_error(et_bonitaet(art='fi', alter=50, hoehe=25, bon_typ="si"))

expect_silent(et_bonitaet(211, alter=80, hoehe=35))
expect_error(et_bonitaet(211, alter="80", hoehe=35))
expect_error(et_bonitaet(211, alter=80, hoehe="35"))
expect_error(et_bonitaet(211, alter=80, hoehe=TRUE))

expect_silent(et_bonitaet(211, alter=80, hoehe=35, kapp_na=TRUE))
expect_error(et_bonitaet(211, alter=80, hoehe=35, kapp_na=NULL))
expect_error(et_bonitaet(211, alter=80, hoehe=35, kapp_na=NA))
expect_error(et_bonitaet(211, alter=80, hoehe=35, kapp_na=1))
expect_error(et_bonitaet(211, alter=80, hoehe=35, kapp_na="a"))
expect_error(et_bonitaet(211, alter=80, hoehe=35, kapp_na=c(TRUE, TRUE, FALSE)))



#===============================================================================
#  Testet die funktionalisierte Inter-/Extrapolation (funk_bonitieren)
#===============================================================================

# Im Bereich der Ertragstafel
# ------------------------------------------------------------------------------
# Treffer der Oberhöhen
expect_equal(et_bonitaet(211, hoehe=40.5, alter=100, hoehe_typ="ober"), -1, tol=0.01)
expect_equal(et_bonitaet(211, hoehe=26.1, alter=70, hoehe_typ="ober"), 1, tol=0.01)
expect_equal(et_bonitaet(211, hoehe=29.0, alter=140, hoehe_typ="ober"), 3, tol=0.01)

# Treffer der Mittelhöhen (+/- ein Zentimeter)
expect_equal(et_bonitaet(211, hoehe=38.5, hoehe_typ="m", alter=100), -1, tol=0.01)
expect_equal(et_bonitaet(211, hoehe=23.9, hoehe_typ="m", alter=70), 1, tol=0.01)
expect_equal(et_bonitaet(211, hoehe=27.9, hoehe_typ="m", alter=140), 3, tol=0.01)

# Interpolation
expect_equal(et_bonitaet(211, hoehe=24, hoehe_typ="m", alter=50), -1.3, tol=0.01)
expect_equal(et_bonitaet(211, hoehe=27, hoehe_typ="m", alter=80), 0.9, tol=0.01)
expect_equal(et_bonitaet(211, hoehe=28, hoehe_typ="m", alter=120), 2.5, tol=0.01)


# Extrapolation der Ertragsklasse
# ------------------------------------------------------------------------------
expect_equal(et_bonitaet(211, hoehe=29, hoehe_typ="m", alter=50), -2.8, tol=0.01)
expect_equal(et_bonitaet(211, hoehe=41, hoehe_typ="m", alter=90), -2.2, tol=0.01)
expect_equal(et_bonitaet(211, hoehe=43, hoehe_typ="m", alter=110), -1.7, tol=0.01)

expect_equal(et_bonitaet(211, hoehe=15, hoehe_typ="m", alter=140), 6.3, tol=0.01)
expect_equal(et_bonitaet(211, hoehe=18, hoehe_typ="m", alter=100), 4.3, tol=0.01)
expect_equal(et_bonitaet(211, hoehe=12, hoehe_typ="m", alter=60), 3.6, tol=0.01)


# Extrapolation in Richtung junger Alter
# ------------------------------------------------------------------------------
# Ekl -1 beginnt bei 25

# ohne Hossfeld
expect_equal(et_bonitaet(211, hoehe=4.5, hoehe_typ="ober", alter=20),
             1.3, tol=0.01)
expect_equal(et_bonitaet(211, hoehe=2, hoehe_typ="ober", alter=10),
             -1.1, tol=0.01)
expect_equal(et_bonitaet(211, hoehe=0.1, hoehe_typ="ober", alter=5),
             -0.3, tol=0.01)
expect_equal(et_bonitaet(211, hoehe=0.01, hoehe_typ="ober", alter=3),
             -1.1, tol=0.01)

# mit Hossfeld
expect_equal(et_bonitaet(211, hoehe=5, hoehe_typ="ober", alter=20, hoss=TRUE),
             1.5, tol=0.01)
expect_equal(et_bonitaet(211, hoehe=2, hoehe_typ="ober", alter=10, hoss=TRUE),
             0.9, tol=0.01)
expect_equal(et_bonitaet(211, hoehe=0.1, hoehe_typ="ober", alter=5, hoss=TRUE),
             4.4, tol=0.01)
expect_equal(et_bonitaet(211, hoehe=0.01, hoehe_typ="ober", alter=3, hoss=TRUE),
             6.1, tol=0.01)


# Extrapolation in Richtung hoher Alter
# ------------------------------------------------------------------------------
# Ekl 3 endet bei 150
expect_equal(et_bonitaet(211, hoehe=42, hoehe_typ="m", alter=150), -0.1, tol=0.01)
expect_equal(et_bonitaet(211, hoehe=48, hoehe_typ="m", alter=160), -1.2, tol=0.01)
expect_equal(et_bonitaet(211, hoehe=38, hoehe_typ="m", alter=180), 1.4, tol=0.01)


# Absolute Bonitäten
# ------------------------------------------------------------------------------
# Das ist nur eine nachträgliche Umrechnung der relativen Ertragsklassen
# in Oberhöhen im Alter 100
expect_equal(et_bonitaet(211, 70, 26.1, "ober","abs"), 32.5, tol=0.01)
expect_equal(et_bonitaet(211, 42, 14.4, "ober","abs"), 30.1, tol=0.01)
expect_equal(et_bonitaet(211, 15, 6, "ober","abs"), 42.4, tol=0.01)


# Fehler und Kappung
# ------------------------------------------------------------------------------
# Eingabetests
# Vektoren unterschiedlich lang
expect_error(et_bonitaet("fi", c(80, 100), c(39, 35, 31)))
expect_error(et_bonitaet(c("fi", "bu"), 100, c(39, 35, 31)))
expect_error(et_bonitaet(c("fi", "bu"), 100:103, 35))

# negative Alter
expect_true(is.na(et_bonitaet(211, hoehe=30, hoehe_typ="m", alter=-5)))
expect_true(is.na(et_bonitaet(211, hoehe=30, hoehe_typ="o", alter=-5)))
expect_true(is.na(et_bonitaet(211, hoehe=30, hoehe_typ="m", bon_typ='rel', alter=-5)))
expect_true(is.na(et_bonitaet(211, hoehe=30, hoehe_typ="o", bon_typ='rel', alter=-5)))

# negative Höhen
et_bonitaet(211, hoehe=-3, hoehe_typ="m", alter=180)

# unbekannte Baumart
expect_error(et_bonitaet(art="xyz", 100, 35))

# Kappungen
expect_warning(bons <- et_bonitaet("fi", 100, c(55, 39, 35, 31, 27, 8),
                                   hoehe_typ="m", methode="funk", kapp_na=TRUE))
expect_equal(bons, c(NA, -0.3, 0.7, 1.7, 2.8, NA), tol=0.01)

expect_warning(bons <- et_bonitaet("fi", 100, c(55, 39, 35, 31, 27, 8),
                                   hoehe_typ="m", methode="funk", kapp_na=FALSE))
expect_equal(bons, c(-3.0, -0.3, 0.7, 1.7, 2.8, 7.0), tol=0.01)

expect_warning(bons <- et_bonitaet("fi", 100, c(55, 39, 35, 31, 27, 8),
                                   hoehe_typ="m", meth="funk", bon_typ="abs", kapp_na=TRUE))
expect_equal(bons, c(NA, 40.1, 36.1, 32.0, 28.0, NA), tol=0.01)



#===============================================================================
#  Testet die klassische Inter-/Extrapolation mittels Dreisatz (klas_bonitieren)
#===============================================================================

# Klare Interpolation des Alters
# ==============================================================================
# klassische, einfache Fälle
# in der Altersspanne 45-115 sind alle Ekl vorhanden

# Alters- & Höhentreffer [Abkürzung]
expect_equal(et_bonitaet(211, 70, 26.1, "ober","relativ","klass"), 1, tol=0.01)
# Toleranztest Höhe
expect_equal(et_bonitaet(211, 70, 26.099, "ober","relativ","klass"), 1, tol=0.01)
# Alterstreffer [Abkürzung]
expect_equal(et_bonitaet(211, 55, 24.8, "ober","relativ","klass"), 0, tol=0.01)
# Altersinterpolation
expect_equal(et_bonitaet(211, 51, 24, "ober","relativ","klass"), -0.2, tol=0.01)
expect_equal(et_bonitaet(211, 103, 23, "ober","relativ","klass"), 3.5, tol=0.01)

# Alter für benötigte Ekl können interpoliert werden,
# Andere nicht-relevante Ekl müssten extrapoliert werden.
# Sollte nicht stattfinden, weil vorher in Code abgefangen. Nur im Debugging sichtbar.
# Alterstreffer
expect_equal(et_bonitaet(211, 40, 15, "ober","relativ","klass"), 1.1, tol=0.01)
expect_equal(et_bonitaet(211, 140, 37, "ober","relativ","klass"), 1.2, tol=0.01)
# Interpolation
expect_equal(et_bonitaet(211, 42, 14.4, "ober","relativ","klass"), 1.6, tol=0.01)
expect_equal(et_bonitaet(211, 133, 35.2, "ober","relativ","klass"), 1.4, tol=0.01)


# Extrapolation des Alters
# ==============================================================================

# Extrapolation nach unten
# ------------------------------------------------------------------------------
# mind. eine Alter für benötigte Ekl muss extrapoliert werden
# Alterstreffer & Extrapolation
expect_equal(et_bonitaet(211, 30, 12.5, "ober","relativ","klass"), 0.2, tol=0.01)
# Interpolation & Extrapolation
expect_equal(et_bonitaet(211, 33, 14.2, "ober","relativ","klass"), 0.1, tol=1e-1)
expect_equal(et_bonitaet(211, 42, 11.4, "ober","relativ","klass"), 2.6, tol=0.01)
# Alter aller Ekl müssen extrapoliert werden (< 25a)
expect_equal(et_bonitaet(211, 15, 6, "ober","relativ","klass"), -1.2, tol=0.01)
expect_equal(et_bonitaet(211, 32, 8, "ober","relativ","klass"), 2.4, tol=0.01)

# Extrapolation nach oben
# ------------------------------------------------------------------------------
# mind. eine Alter für benötigte Ekl muss extrapoliert werden (>115)
# Alterstreffer & Extrapolation
expect_equal(et_bonitaet(211, 140, 40, "ober","relativ","klass"), 0.5, tol=0.01)
# Interpolation & Extrapolation
expect_equal(et_bonitaet(211, 131, 39, "ober","relativ","klass"), 0.5, tol=0.01)
expect_equal(et_bonitaet(211, 147, 36.5, "ober","relativ","klass"), 1.4, tol=0.01)
# Alter aller Ekl müssen extrapoliert werden (>150) unterhalb Alterskappung (<180)
expect_equal(et_bonitaet(211, 154, 32, "ober","relativ","klass"), 2.5, tol=0.01)
expect_equal(et_bonitaet(211, 162, 44, "ober","relativ","klass"), 0.3, tol=0.01)

# Absolute Bonitäten
# ------------------------------------------------------------------------------
# Das ist nur eine nachträgliche Umrechnung der relativen Ertragsklassen
# in Oberhöhen im Alter 100
expect_equal(et_bonitaet(211, 70, 26.1, "ober","abs","klass"), 32.5, tol=0.01)
expect_equal(et_bonitaet(211, 42, 14.4, "ober","abs","klass"), 30.2, tol=0.01)
expect_equal(et_bonitaet(211, 15, 6, "ober","abs","klass"), 41.1, tol=0.01)


# Fehler und Kappung
# ------------------------------------------------------------------------------
# Eingabetests

# Alter außerhalb des Altersintervalls (Bu: 5-180)
expect_warning(bon <- et_bonitaet('bu', hoehe=10, hoehe_typ="m", alter=3, meth="klass"))
expect_true(is.na(bon))
expect_warning(bon <- et_bonitaet(211, 190, 50, "ober","relativ","klass"))
expect_true(is.na(bon))

# Ekl Kappung
expect_warning(bons <- et_bonitaet("fi", 100, c(55, 39, 35, 31, 27, 8),
                                   hoehe_typ="m", meth="klass", kapp_na=TRUE))
expect_equal(bons, c(NA, -0.2, 0.7, 1.7, 2.8, NA), tol=0.01)

expect_warning(bons <- et_bonitaet("fi", 100, c(55, 39, 35, 31, 27, 8),
                                   hoehe_typ="m",meth="klass", kapp_na=FALSE))
expect_equal(bons, c(-2.0, -0.2, 0.7, 1.7, 2.8, 4.0), tol=0.01)

expect_warning(bons <- et_bonitaet("fi", 100, c(55, 39, 35, 31, 27, 8),
                                   hoehe_typ="m",meth="klass", bon_typ="abs", kapp_na=TRUE))
expect_equal(bons, c(NA, 40.1, 36.1, 32.0, 28.0, NA), tol=0.01)
