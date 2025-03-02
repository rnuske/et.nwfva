
# et.nwfva 0.2.0
* Wechsel der Lizenz von GPL zu MIT, um Nutzung in proprietärer Software zu ermöglichen. Zustimmung der Ko-Autoren in Issue #1.
* `NA` in Parametern von et_bonitaet() und et_hoehe() ermöglichen (Danke an Jan Schick für Pull Request)
* Fix für Bonitäten genau zwischen zwei Ertragsklassen in et_tafel()
* Eingabe von bon in et_tafel() wird vor der Berechnung auf Genauigkeit der Ausgabe (eine Nachkommastelle) gerundet, um konsistente Zeilen in der Ausgabe zu erreichen.

# et.nwfva 0.1.1
* Bessere Überprüfung der Eingabe und Warnungen (Danke für Hinweise an Jan Schick)

# et.nwfva 0.1.0
* Erste Version. Enthält die neuen Ertragstafeln. Die Funktionen zur Bonitierung, Bestandeshöhenberechnung und Umrechnung von Bonitäten verwenden entweder funktionale Bonitätsfächer oder den klassischen Dreisatz. Die Inter- & Extrapolation von Tafelwerten ist nur mittels Dreisatz implementiert.
