#===============================================================================
#
# Grafiken als Hilfestellung für die Konstruktion von Testfällen in test_bonitaet.R
# Wird bei der Ausführung von Tests nicht benötigt.
#
# Ertragsklassenfächer für Buche
#
#===============================================================================


yt <- et_tafel(211)


opa <- par(mar=c(2.5,2.4,0.5,0.5), xaxs='i', yaxs='i')

# Oberhöhenbonitätsfächer
plot(0,0, xlim=c(22,153), ylim=c(10,44), type='n', xlab='Alter', ylab='H100')
for(i in c(-1, 0, 1, 2, 3)){
  df <- yt[yt$Ek == i,  ]
  lines(df$Alter, df$H100)
}

abline(v=seq(25, 150, 5), col='grey', lty='dotted')
abline(h=seq(8, 45, 1), col='grey', lty='dotted')
mtext(seq(25,45,5), side=1, line=-1.05, at=seq(25,45,5), cex=0.75)
mtext(seq(115,150,5), side=3, line=-0.95, at=seq(115,150,5), cex=0.75)
mtext(seq(30,43,1), side=4, line=-1.05, at=seq(30,43,1), cex=0.75, las=1)
text(x=c(94.2, 104.6, 115.7, 126.0, 134.2),
     y=c(40.2, 38.0, 35.5, 32.5, 29.0),
     labels=-1:3, adj=c(0.5,0.5), col="darkred")
mtext("Oberhöhenbonitätsfächer", side=3, line=-1.2, adj=0.02, cex=1.2)


# Mittelhöhenbonitätsfächer
plot(0,0, xlim=c(22,153), ylim=c(7,42), type='n', xlab='alter', ylab='hg')
for(i in c(-1, 0, 1, 2, 3)){
  df <- yt[yt$Ek == i,  ]
  lines(df$Alter, df$Hg)
}

abline(v=seq(20, 155, 5), col='grey', lty='dotted')
abline(h=seq(6, 45, 1), col='grey', lty='dotted')
mtext(seq(25,45,5), side=1, line=-1.05, at=seq(25,45,5), cex=0.75)
mtext(seq(115,150,5), side=3, line=-0.95, at=seq(115,150,5), cex=0.75)
mtext(seq(28,42,1), side=4, line=-1.05, at=seq(28,42,1), cex=0.75, las=1)
text(x=c(91.9, 105.6, 119.7, 129.6, 140.2),
     y=c(37.2, 36.0, 34.2, 31.4, 28.3),
     labels=-1:3, adj=c(0.5,0.5), col="darkred")
mtext("Mittelhöhenbonitätsfächer", side=3, line=-1.2, adj=0.02, cex=1.2)

par(opa)
