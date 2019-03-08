library(tidyverse)

F <- function(x, theta) pnorm(x, theta, 1)

inverse <- function(f) {
    fout <- function(y) uniroot(function(x) f(x) - y, c(-100, 100))$root
    function(y) (quietly(fout)(y))$result
}
THETA <- 1
n <- 2
LL <- qchisq(0.05, 2*n)
UL <- qchisq(0.95, 2*n)
df <- tibble(X = map(1:10000, ~rnorm(n, THETA, 1))) %>%
    mutate(
        Q1 = map(X, ~function(theta) -2 * sum(log(F(.x, theta)))),
        Q2 = map(X, ~function(theta) -2 * sum(log(1 - F(.x, theta)))),
        LIQ1 = map_dbl(Q1, ~inverse(.x)(LL)),
        UIQ1 = map_dbl(Q1, ~inverse(.x)(UL)),
        LIQ2 = map_dbl(Q2, ~inverse(.x)(UL)),
        UIQ2 = map_dbl(Q2, ~inverse(.x)(LL)),
        Q1CI_contains = LIQ1 < THETA & THETA <  UIQ1,
        Q2CI_contains = LIQ2 < THETA & THETA < UIQ2,
        Q1_length = UIQ1 - LIQ1,
        Q2_length = UIQ2 - LIQ2
    )

df %>%  summarize(
            Q1_coverage = sum(Q1CI_contains) / nrow(.),
            Q1_length_mean = mean(Q1_length),
            Q1_length_range = max(Q1_length) - min(Q1_length),
            Q2_coverage = sum(Q2CI_contains) / nrow(.),
            Q2_length_mean = mean(Q2_length),
            Q2_length_range = max(Q2_length) - min(Q2_length),
            )

with(df, ks.test(Q1_length, Q2_length))
with(df, ks.test(LIQ1, LIQ2))
with(df, ks.test(UIQ1, UIQ2))

df %>% select(LIQ1, LIQ2, UIQ1, UIQ2) %>%
    gather(which, value) %>%
    ggplot() +
    geom_density(aes(value, color = which, fill = which), alpha = 0.3)



