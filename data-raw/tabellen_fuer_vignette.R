#===============================================================================
#
#  Erzeugung von HTML Quellcode für die Tabellen in der Vignette Nutzerhinweise
#
#  Hauptmotivation ist die Vermeidung einer unnötigen Abhängigkeit von einem
#  Tabellenerzeugungspaket wie kableExtra, gt o.Ä., die jeweils viele weitere
#  Pakete hinter sich herziehen.
#  Eine dynamische Erzeugung der Tabellen ist auch nicht notwendig, da sie sich
#  nicht ändern und nicht aus einem bestehenden Datenobjekt generiert werden.
#
#===============================================================================

library(knitr)
library(kableExtra)


#-------------------------------------------------------------------------------
# Tabelle 1
#-------------------------------------------------------------------------------

df <- data.frame(
  mb=c('463 (101)', '241 (61)', '278 (61)', '478 (290)', '142 (32)'),
  a_min=c(17, 25, 19, 15, 16),
  a_mit=c(74, 63, 42, 40, 43),
  a_max=c(170, 110, 99, 77, 96),
  h_q5=c(19.4, 26.7, 30.1, 35.1, 24.9),
  h_mit=c(27.3, 33.1, 37.9, 41.7, 30.6),
  h_q95=c(36.4, 40.4, 42.2, 47.8, 35.7),
  stringsAsFactors = FALSE
)
rownames(df) <- c("Eiche", "Buche", "Fichte", "Douglasie", "Kiefer")
names(df) <- c("Modellbestände", "Min.", "Mittelwert", "Max.", "5%-Quantil",
               "Mittelwert", "95%-Quantil")
df

options(kableExtra_view_html=F)

kable(df, align='c',
      caption="Datengrundlage für die Ertragstafelerstellung für die fünf Hauptbaumarten: Anzahl der generierten Modellbestände mit ihren Alters- und Oberhöhenbonitätskennwerten. Die Anzahl ergibt
sich aus der in Klammern angegebenen Anzahl von Versuchsparzellen multipliziert mit den verwendeten Aufnahmen abzüglich ausgesonderter Startkonstellationen.") |>
  kable_styling() |> column_spec(1, bold=TRUE) |>
  add_header_above(c(" " = 1, "Anzahl"=1, "Alter zu Beginn der Projektion"=3,
                     "Oberhöhenbonität h100"=3))



#-------------------------------------------------------------------------------
# Tabelle 2
#-------------------------------------------------------------------------------

# zwei nebeneinander (verworfen)
lh <- data.frame(H100=c(33,30,27,24,21, 40.5,36.5,32.5,28.5,24.5),
                 Ekl=c('-I', '0', 'I', 'II', 'III', '-I', '0', 'I', 'II', 'III'))
nh <- data.frame(H100=c(43,39,35,31,27, 50,45,40,35, 37,33,29,25,21),
                 Ekl=c('-I', '0', 'I', 'II', 'III', '0', 'I', 'II', 'III',
                       '-I', '0', 'I', 'II', 'III'))

kables( list(
    kable(lh, col.names=c("H₁₀₀ [m]", "Ekl."), align="lcc", booktabs=TRUE) |>
      pack_rows(index=c("Eiche"=5, "Buche"=5))|> kable_styling(),
    kable(nh, col.names=c("H₁₀₀ [m]", "Ekl."), align="lcc", booktabs=TRUE) |>
      pack_rows(index=c("Fichte"=5, "Douglasie"=4,"Kiefer"=5))|> kable_styling()
    ),
  caption="Die absoluten Oberhöhenbonitäten in den Ertragstafeln und die korrespondierenden relativen Ertragsklassen (gerundet) nach Schober (1995).")


# nebeneinander in einer großen Tabelle (verworfen)
ei <- data.frame(H100=c(33,30,27,24,21),
                 Ekl=c('-I', '0', 'I', 'II', 'III'))
bu <- data.frame(H100=c(40.5,36.5,32.5,28.5,24.5),
                 Ekl=c('-I', '0', 'I', 'II', 'III'))
fi <- data.frame(H100=c(43,39,35,31,27),
                 Ekl=c('-I', '0', 'I', 'II', 'III'))
dgl <- data.frame(H100=c(NA, 50,45,40,35),
                  Ekl=c(NA, '0', 'I', 'II', 'III'))
ki <- data.frame(H100=c(37,33,29,25,21),
                 Ekl=c('-I', '0', 'I', 'II', 'III'))
df <- cbind(ei, bu, fi, dgl, ki)
names(df) <- rep(c("H100", "Ekl."), 5)

kable(df, caption="Die absoluten Oberhöhenbonitäten in den Ertragstafeln und die korrespondierenden relativen Ertragsklassen (gerundet) nach Schober (1995).") |>
  kable_paper() |>
  add_header_above(c("Eiche"=2,"Buche"=2,"Fiche"=2,"Douglasie"=2,"Kiefer"=2))


# 5 einzelne Tabellen nebeneinander (ausgewählt)

options(kableExtra_view_html=F)     # FALSE => Gib Quellcode aus!
options(knitr.kable.NA = '&nbsp;')  # unsichtbares Zeichen das Platz einnimmt

kables(list(
  kable(ei, align='c') |> add_header_above(c("Eiche"=2)),
  kable(bu, align='c') |> add_header_above(c("Buche"=2)),
  kable(fi, align='c') |> add_header_above(c("Fichte"=2)),
  kable(dgl, align='c') |> add_header_above(c("Douglasie"=2)),
  kable(ki, align='c') |> add_header_above(c("Kiefer"=2))
),  caption="Die absoluten Oberhöhenbonitäten in den Ertragstafeln und die korrespondierenden relativen Ertragsklassen (gerundet) nach Schober (1995).") |> kable_paper()


# Nacharbeiten:
# H100 -> H<sub>100</sub>
