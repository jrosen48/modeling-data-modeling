#analysis.r

library(tidyverse)
library(poLCA)

f <- "obs-segment_units1-7_2013-2014.csv"

d <- read_csv(f)

add_one <- function(x) {
    x + 1
}

dlta <- d %>% 
    dplyr::select(sInvented, sProcedural, sConceptual, tInitSelect, tCompare, tDiscussQ, tConnectBigIdeas, tConnectOthers, tPressExplain) %>% 
    map_df(replace_na, 0)

?MplusAutomation::createMixtures()

ds <- d %>% 
    dplyr::select(fGroups,
                  sInvented, sProcedural, sConceptual, tInitSelect, tCompare, tDiscussQ, tConnectBigIdeas, tConnectOthers, tPressExplain) %>% 
    map_df(replace_na, 0) %>% 
    map_df(add_one)

f <- cbind(sInvented, sProcedural, sConceptual, tInitSelect, tCompare, tDiscussQ, tConnectBigIdeas, tConnectOthers, tPressExplain) ~ 1

# od <- map(1:9, poLCA, formula = f, data = ds, maxiter = 5000, verbose = FALSE, graphs = FALSE) %>% 
#     map_df(broom::glance)
# 
# od %>% 
#     mutate(n_classes = 1:9) %>% 
#     ggplot(aes(x = n_classes, y = BIC)) +
#     geom_point() +
#     geom_line() +
#     scale_x_continuous(breaks = 1:9, labels = 1:9) +
#     theme_bw()

f <- cbind(sInvented, sProcedural, sConceptual, tInitSelect, tCompare, tDiscussQ, tConnectBigIdeas, tConnectOthers, tPressExplain) ~ 1

m3 <- poLCA(f, ds, nclass = 3, maxiter = 5000, graphs = TRUE)
m4 <- poLCA(f, ds, nclass = 4, maxiter = 5000, graphs = TRUE)
m5 <- poLCA(f, ds, nclass = 5, maxiter = 5000, graphs = TRUE)

fc <- cbind(sInvented, sProcedural, sConceptual, tInitSelect, tCompare, tDiscussQ, tConnectBigIdeas, tConnectOthers, tPressExplain) ~ fGroups
m4c <- poLCA(fc, ds, nclass = 4, maxiter = 5000, graphs = TRUE)
m4c

ds$m3_class <- m3$predclass
ds$m4_class <- m4$predclass
ds$m5_class <- m5$predclass

ds$segment <- d$SegNum
ds$teacher <- as.factor(d$`Teacher::TeacherID`)

ds %>% 
    mutate(yval = 1) %>% 
    ggplot(aes(x = segment,y =yval, color = as.factor(m3_class))) + geom_point() + geom_line() + facet_wrap(~teacher, ncol = 5) +
    scale_y_continuous(labels = 1:3, breaks = 1:3)
ggtitle("3 class/profile solution")

ds %>% 
    mutate(yval = 1) %>% 
    ggplot(aes(x = segment,y =yval, color = as.factor(m4_class))) + geom_point() + geom_line() +facet_wrap(~teacher, ncol = 5) +
    scale_y_continuous(labels = 1:3, breaks = 1:3)
ggtitle("4 class/profile solution")

ds %>% 
    mutate(yval = 1) %>% 
    ggplot(aes(x = segment,y =yval, color = as.factor(m5_class))) + geom_point() + geom_line() + facet_wrap(~teacher, ncol = 5) +
    scale_y_continuous(labels = 1:3, breaks = 1:3)
ggtitle("5 class/profile solution")
