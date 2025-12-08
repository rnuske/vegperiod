# Vegetation Start Method "Menzel"

The method "Menzel" implements the algorithm described in Menzel (1997).
The method is parameterized for 10 common tree species ( *Larix
decidua*, *Picea abies (frueh)*, *Picea abies (spaet)*, *Picea abies
(noerdl.)*, *Picea omorika*, *Pinus sylvestris*, *Betula pubescens*,
*Quercus robur*, *Quercus petraea*, *Fagus sylvatica*). It needs
previous year's chill days.

## Calculations

     - counting chill days in Nov & Dec of previous year if previous year available else estimate previous year's chill days
     - count chill days in current year
     - determine critical temperature per days using regression equation fitted by Menzel 1997
     - calculate HeatSum starting from Feb
     - vegetation start if HeatSum >= criticalTemp

## References

Menzel, A. (1997) Phänologie von Waldbäumen unter sich ändernden
Klimabedingungen - Auswertung der Beobachtungen in den Internationalen
Phänologischen Gärten und Möglichkeiten der Modellierung von Phänodaten.
Forstliche Forschungsberichte München.
