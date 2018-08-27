library(tidyverse)
library(janitor)
library(poLCA)

f <- "obs-segment_units1-7_2013-2014.csv"

d <- read_csv(f)

d <- clean_names(d)

recode_vals <- function(x) {
    if_else
}

d %>% count(`Teacher::TeacherID`, `Teacher::Condition`) %>% arrange(`Teacher::Condition`)

d %>% count(SegNum, `Teacher::TeacherID`, `ClassObservation::Unit`)

add_one <- function(x) {
    x + 1
}

ds <- d %>% 
    dplyr::select(sInvented, sProcedural, sConceptual, tInitSelect, tCompare, tDiscussQ, tConnectBigIdeas, tConnectOthers, tPressExplain) %>% 
    map_df(replace_na, 0) %>% 
    map_df(add_one)

f <- cbind(sInvented, sProcedural, sConceptual, tInitSelect, tCompare, tDiscussQ, tConnectBigIdeas, tConnectOthers, tPressExplain) ~ 1


od <- map(1:9, poLCA, formula = f, data = ds, maxiter = 5000) %>% 
    map_df(broom::glance)

od %>% 
    mutate(n_classes = 1:9) %>% 
    ggplot(aes(x = n_classes, y = BIC)) +
    geom_point() +
    geom_line() +
    scale_x_continuous(breaks = 1:9, labels = 1:9) +
    theme_bw()