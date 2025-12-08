# Vegetation End Method "vonWilpert"

The method `vonWilpert` is based on von Wilpert (1990). It was
originally developed for "Picea abies" in the Black Forest but is
commonly used for all tree species throughout Germany. As usual, the
rules regarding the soilmatrix are neglected in this implementation.

## Calculations

Orthodox are 3 criteria: **short day**, **temperature** and **drought
criterion** we consider -as usual- only short day and temperature
citerion

### Temperature criterion:

- 7 day moving average of daily mean temperatures at least 5 consecutive
  days under 10°C

- if afterwards more than 5 consecutive days 7 day moving average over
  10°C vegetation period gets restarted

### Short day criterion

- last day of the vegetation period is DOY 279 (5th of October in leap
  years)

## References

von Wilpert, K. (1990) Die Jahrringstruktur von Fichten in Abhängigkeit
vom Bodenwasserhaushalt auf Pseudogley und Parabraunerde: Ein
Methodenkonzept zur Erfassung standortsspezifischer
Wasserstreßdispostion. *Freiburger Bodenkundliche Abhandlungen*. Pages
106–108.
