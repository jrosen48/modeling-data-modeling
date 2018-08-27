library(LMest)
library(tidyverse)
library(janitor)
library(poLCA)

f <- "obs-segment_units1-7_2013-2014.csv"

d <- read_csv(f)

d <- clean_names(d)

recode_vals <- function(x) {
    if_else
}

ds <- d %>% 
    dplyr::select(teacher_condition, f_groups:i_variability_stats, -s_engagement) %>% 
    dplyr::filter(teacher_condition == "1 Treatment") %>% 
    mutate_all(replace_na, 0)

data("RLMSdat", package = "LMest")

RLMSdat %>% str()
out <- aggr_data(RLMSdat)
yv <- out$freq
S <- 1 - out$data_dis
mod1 <- est_lm_basic(S, yv, k = 3, mod = 1, start = 0, out_se = TRUE)

out <- aggr_data(ds[1:100, 2:6] %>% t() %>% as_tibble())

#out <- aggr_data(ds[, 2:6])
yv <- out$freq
S <- 1 - out$data_dis
mod1 <- est_lm_basic(S, yv, k = 3, mod = 1, start = 0, out_se = TRUE)

mod1$Psi %>% 
    as_tibble() %>% 
    setNames(c("c1", "c2", "c3")) 
    mutate(codes = 0:1) %>% 
    gather(key, val, -codes) %>% 
    mutate(key = as.factor(key)) %>% 
    ggplot(aes(x = codes, y = val, color = key, group = key)) +
    geom_point() +
    geom_line()
