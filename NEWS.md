# Changes in version 0.1.1

## Bug fix

- The `RenouvTList` function no longer worked casting the error
  "`fitJit not found`".

- In the `autoplot` method for the class `Renouv`, the grid of periods
  used was chosen as starting at `1`. It now starts at `1 / lambda` 
  as it should, where `lambda` is the rate `coef(objet)["rate"]`.
  

## Enhancements

- The `autoplot` method for the `"Renouv"` class now provides better
  legend "guides" and allows the user to change the colours,
  linetypes, ...

- The `coef` method of the `"RenouvTList"` class now has a `sd` formal
  argument which makes it work as the former `coSd` method which is
  now deprecated.
  
- The `RenouvTList` function/creator now has a `start.par.ini`
  argument which can be a matrix or a list of lists. This can be used
  to give initial values for the parameters that differ across
  thresholds as required with the `EGPD3` distribution for the
  excesses.

- In the `autoplot` method for the class `Renouv`, the confidence
  intervals are identified in the legend by the linetype of their
  borders, not by the fill.
  
- In the `autoplot` method for the class `Renouv`, the historical
  block names that could have been specified at the creation time
  through the names of the `MAX.data` or `OTS.data` are now duely
  shown.

