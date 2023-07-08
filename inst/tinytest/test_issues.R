#===============================================================================
#
#  Testfälle für gemeldete Issues
#
#===============================================================================


#===============================================================================
#  #3 NA handling in et_hoehe() und et_bonitaet()
#===============================================================================


expect_equal(et_bonitaet(art=211, alter=c(100, 150), hoehe=c(30, 35)),
             c(1.6, 1.8))

expect_equal(et_bonitaet(art=211, alter=c(100, 150), hoehe=c(30, NA)),
             c(1.6, NA))
expect_equal(et_bonitaet(art=211, alter=c(NA, 150), hoehe=c(30, 35)),
             c(NA, 1.8))


expect_equal(et_hoehe(art=211, alter=c(100, 100), bon=c(1, 2)), c(32.5, 28.5))

expect_equal(et_hoehe(art=211, alter=c(100, 100), bon=c(1, NA)), c(32.5, NA))
expect_equal(et_hoehe(art=211, alter=c(NA, 100), bon=c(1, 2)), c(NA, 28.5))
