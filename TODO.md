
## Renextra TODO list

- [x] `autoplot.Rendata`: show the threshold as horizontal segments.

- [x] `autoplot.Renouv`: make the use of `groupNames` consistent with
  `plot.Renouv`.

- [ ] `autoplot.Renouv`: allow the user to change the graphical
  elements (colours, symbol shapes, ...) via suitable args such as
  `quant.col`, `conf.lty` and so on.

- [ ] `autoplot.Renouv`: show the quantile line in the legend (colour
  and linetype).

- [ ] Change the labels of the legends (such as `"Level"`) or at least allow
  the use to change them via suitable arguments. Note that due to the use of 
  **ggnewscale** it is not possible to change these in the usual way 
  `labs(linetype = "Conf")` 

- [ ] Discard the `coSd` method (and the related definition of a new
      generic), and use simply `coef(object, sd = TRUE)` instead. It
      should be mentioned in the documentation that with `sd = TRUE`
      the returned object is a list of two matrices.

- [ ] Use the graphical parameters defined with `Renext::RLpar` with
      the `autoplot` and `autolayer` methods? If so the arguments of
      `autoplot.Renouv` devoted to the graphical parameters should be
      discarded.
