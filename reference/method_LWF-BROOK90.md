# Vegetation End Method "LWF-BROOK90"

The method `LWF-BROOK90` is -for the sake of convenience- a
reimplementation of the LWF-BROOK90 VBA (version 3.4) variant of
"vonWilpert" (Hammel and Kennel 2001). Their interpretation of von
Wilpert (1990) and the somewhat lower precision of VBA was mimicked.

## Calculations

    - starting search at June 1st
    - propose end if 7-day moving average temperature is below 10°C
      on 5 consecutive days
    - restart search for end if there is a warm periode (7-day moving average
      temperature above 10°C for 5 consecutive days
    - nevertheless the vegetation periode stops latest at DOY 279

## References

Hammel, K. and Kennel, M. (2001) Charakterisierung und Analyse der
Wasserverfügbarkeit und des Wasserhaushalts von Waldstandorten in Bayern
mit dem Simulationsmodell BROOK90. *Forstliche Forschungsberichte
München*.

von Wilpert, K. (1990) Die Jahrringstruktur von Fichten in Abhängigkeit
vom Bodenwasserhaushalt auf Pseudogley und Parabraunerde: Ein
Methodenkonzept zur Erfassung standortsspezifischer
Wasserstreßdispostion. *Freiburger Bodenkundliche Abhandlungen*.
