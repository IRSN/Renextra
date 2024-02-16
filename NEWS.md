# Changes in (pending) version 0.1.1

## Bug fix

- In the `autoplot` method for the class `Renouv`, the grid of periods
  used was chosen as starting at `1`. It now starts at `1 / lambda` 
  as it should, where `lambda` is the rate `coef(objet)["rate"]`.
  

## Enhancements

- In the `autoplot` method for the class `Renouv`, the confidence
  intervals are identified in the legend by the linetype of their
  borders, not by the fill.
  
- In the `autoplot` method for the class `Renouv`, the historical
  block names that could have been specified at the creation time
  through the names of the `MAX.data` or `OTS.data` are now duely
  shown.
