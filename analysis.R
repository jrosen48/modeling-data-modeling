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






# dd <- d %>% 
#     dplyr::select(teacher_condition, teacher_teacher_id, f_groups:i_variability_stats, -s_engagement, 
#                   class_observation_date, class_observation_unit) %>% 
#     dplyr::filter(teacher_condition == "1 Treatment") %>% 
#     mutate_all(replace_na, 1) %>% 
#     mutate(class_observation_unit = as.factor(class_observation_unit)) %>% 
#     count(class_observation_date, class_observation_unit, teacher_teacher_id)
# 
# p1 <- dd %>% 
#     mutate(class_observation_date = lubridate::mdy(class_observation_date)) %>% 
#     ggplot(aes(x = class_observation_date, y = n)) +
#     geom_point(aes(color = class_observation_unit)) +
#     geom_line(color = "gray") +
#     ylab("N segments") +
#     theme_bw() +
#     facet_wrap(~teacher_teacher_id)
# 
# p1
# 
# du <- d %>% 
#     dplyr::select(teacher_condition, f_groups:i_variability_stats, -s_engagement, 
#                   class_observation_unit) %>% 
#     dplyr::filter(teacher_condition == "1 Treatment") %>% 
#     mutate_all(replace_na, 1) %>% 
#     count(class_observation_unit)
# 
# p2 <- du %>% 
#     ggplot(aes(x = class_observation_unit, y = n)) +
#     geom_point() +
#     geom_line() +
#     ylab("N segments") +
#     theme_bw()
# 
# ds <- d %>% 
#     dplyr::select(teacher_condition, f_groups:i_variability_stats, -s_engagement, 
#                   class_observation_unit) %>% 
#     dplyr::filter(teacher_condition == "1 Treatment") %>% 
#     mutate_all(replace_na, 1)
# 
# ds %>% 
#     filter(class_observation_unit == 1)
# 
# f <- cbind(f_groups, f_seat, s_invented, s_conceptual, s_procedural) ~ 1
# 
# M0 <- poLCA(f,
#             ds[, 2:6],
#             nclass=2, 
#             graphs=T, 
#             na.rm = F) # log-likelihood: -254.8961, BIC = 531.63, AIC = 499.96
# 
# M0
# 
# library(MplusAutomation)
# 
# data <- read.table("http://statmodel.com/usersguide/chap8/ex8.13.dat")[,c(1:10)]
# names(data) <- c("u11", "u12", "u13", "u14", "u15", "u21", "u22", "u23", "u24", "u25")
# createMixtures(
#     classes = 2,
#     filename_stem = "dating",
#     model_overall = "c2 ON c1;",
#     model_class_specific = c(
#         "[u11$1] (a{C});  [u12$1] (b{C});  [u13$1] (c{C});  [u14$1] (d{C});  [u15$1] (e{C});",
#         "[u21$1] (a{C});  [u22$1] (b{C});  [u23$1] (c{C});  [u24$1] (d{C});  [u25$1] (e{C});"
#     ),
#     rdata = data,
#     OUTPUT = "standardized",
#     ANALYSIS = "PROCESSORS IS 2;  LRTSTARTS (0 0 40 20);  PARAMETERIZATION = PROBABILITY;",
#     VARIABLE = "CATEGORICAL = u11-u15 u21-u25;"
# )
# 
# runModels("dating_2_class.inp")
# m <- readModels()
# plotMixtures(m, variables = c("u11", "u12"), parameter = c("Means", "Thresholds"))
# m
