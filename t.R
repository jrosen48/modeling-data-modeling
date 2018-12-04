ds %>% 
    dplyr::select(sInvented, sProcedural, sConceptual,
                  tInitSelect, tCompare, tDiscussQ, 
                  tConnectBigIdeas, tConnectOthers, tPressExplain,
                  class_assignment = class, C1_prob, C2_prob, C3_prob, C4_prob) %>% 
    gather(class, prob, C1_prob, C2_prob, C3_prob, C4_prob) %>% 
    mutate_if(is.integer, replace_na, 0) %>% 
    unite(response_pattern, sInvented, sProcedural, sConceptual,
          tInitSelect, tCompare, tDiscussQ, 
          tConnectBigIdeas, tConnectOthers, tPressExplain, sep = "-") %>% 
    group_by(class, response_pattern) %>% 
    summarize(mean_post_prob = mean(prob),
              n = n()) %>% 
    ungroup() %>% 
    spread(class, mean_post_prob) %>% 
    mutate_if(is.double, round, 3) %>% 
    arrange(desc(n)) %>% 
    write_csv("response-patterns-probability.csv")
    
ds %>% 
    dplyr::select(sInvented, sProcedural, sConceptual,
                  tInitSelect, tCompare, tDiscussQ, 
                  tConnectBigIdeas, tConnectOthers, tPressExplain,
                  class) %>% 
    mutate_if(is.integer, replace_na, 0) %>% 
    unite(response_pattern, sInvented, sProcedural, sConceptual,
          tInitSelect, tCompare, tDiscussQ, 
          tConnectBigIdeas, tConnectOthers, tPressExplain, sep = "-") %>% 
    count(response_pattern) %>% 
    mutate(overall_prop = n / sum(n)) %>% 
    write_csv("response-patterns-overall.csv")

ds %>% 
    dplyr::select(sInvented, sProcedural, sConceptual,
                  tInitSelect, tCompare, tDiscussQ, 
                  tConnectBigIdeas, tConnectOthers, tPressExplain,
                  class) %>% 
    mutate_if(is.integer, replace_na, 0) %>% 
    unite(response_pattern, sInvented, sProcedural, sConceptual,
          tInitSelect, tCompare, tDiscussQ, 
          tConnectBigIdeas, tConnectOthers, tPressExplain, sep = "-") %>% 
    count(class, response_pattern) %>% 
    group_by(class) %>% 
    mutate(within_class_prop = n / sum(n)) %>% 
    ungroup() %>% 
    mutate(overall_prop = n / sum(n)) %>% 
    arrange(class, desc(n)) %>% 
    filter(class == 2) %>% 
    slice(1:9) %>% 
    summarize(sum_wcp = sum(within_class_prop))
