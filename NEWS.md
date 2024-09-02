
# Changes in version 0.1.2

## Enhancements

- When the `autoplot` method is used for an object with class
  `"coef.RenouvTList"` the number of exceedances is now displayed on a
  second (top) horizontal axis. This is only possible when the object
  was created using `lambda = FALSE`.

- The autoplot methods for the classes `"Renouv"` and `"RenouvTList"`
  both have a `xlim` argument. The motivation is that when using the
  standard `+ xlim()` on a ggplot object with a log scale on the
  x-axis, the log-scale is lost.

# Changes in (pending) version 0.1.1

## Bug fix

- In the `autoplot` method for the class `Renouv`, the grid of periods
  used was chosen as starting at `1`. It now starts at `1 / lambda` as
  it should, where `lambda` is the rate `coef(objet)["rate"]`.
  
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

