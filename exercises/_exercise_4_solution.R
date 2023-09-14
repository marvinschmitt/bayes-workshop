library(brms)

icecream2 = read.csv("data/icecream2.csv")


mlm_1 = brm(
  units ~ temp + (1 | location),
  data = icecream2,
  control = list(adapt_delta = 0.99)
)

mlm_2 = brm(
  units ~ temp + (1 | location),
  family = poisson(),
  data = icecream2,
  control = list(adapt_delta = 0.99,
                 max_treedepth=15)
)

mlm_3 = brm(
  units ~ temp + (temp | location),
  data = icecream2,
  control = list(adapt_delta = 0.99,
                 max_treedepth=15)
)

mlm_4 = brm(
  units ~ 1 + temp + (1 + temp | location),
  data = icecream2,
  control = list(adapt_delta = 0.99,
                 max_treedepth=15)
)


loo_1 = loo(mlm_1)
loo_2 = loo(mlm_2)
loo_3 = loo(mlm_3)
loo_4 = loo(mlm_4)
loo_compare(loo_1, loo_2, loo_3, loo_4)



p = posterior_samples(mlm_1)
