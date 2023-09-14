library(brms)
library(tidyverse)

movies = read.csv("data/movies.csv")


# Cross Table
movies %>%
  group_by(genre) %>%
  summarize(avg_rating = mean(rating, na.rm = TRUE))


# frequentist: equal variances
t.test(rating ~ genre, data = movies, var.equal=TRUE)

# frequentist: unequal variances
t.test(rating ~ genre, data = movies, var.equal=FALSE)


# Bayesian
lm_means_sd = brm(
  bf(rating ~ 0 + genre, sigma ~ 0 + genre),
  data = movies
)

post_samples = posterior_samples(lm_means_sd) %>%
  mutate_at(vars(contains("sigma")), funs(exp)) %>%
  mutate(mean_diff = b_genreAction - b_genreComedy,
         var_action = b_sigma_genreAction ** 2,
         var_comedy = b_sigma_genreComedy ** 2) %>%
  mutate(pooled_sd = sqrt(0.5 * (var_action + var_comedy))) %>%
  mutate(d = mean_diff / pooled_sd)

post_samples$d

CI = quantile(post_samples$d, c(0.025, 0.975))

ROPE = c(-0.3, +0.3)

ggplot(post_samples, aes(x = d)) +
  geom_histogram(aes(y=..density..), bins=50, fill="white", color="black") +
  geom_density(size=2) +
  geom_vline(xintercept = ROPE, color="red", size=2, label="ROPE") +
  geom_vline(xintercept = CI, color = "blue", size=1.5, linetype="dashed", label="CI") +
  scale_y_continuous(expand=c(0, 0)) +
  scale_x_continuous(limits=c(-1, 1))



in_rope = between(post_samples$d, ROPE[1], ROPE[2]) %>%
  mean()
1 - in_rope

(abs(post_samples$d) > 0.6) %>%
  mean()
