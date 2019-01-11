## Functions

create_db <- function(varname,  pre_post, full_db) {
  
  db_new <- full_db %>% select(ID, Condicion, starts_with(varname)) 
  
  db_new <- db_new %>% select(ID, Condicion, ends_with(pre_post)) %>%
    mutate(Condicion = factor(Condicion, levels = c(0, 1), labels = c("Control", "FB"))) %>%
  rename(Condition = Condicion, 
         "0" = 3, 
         "1" = 4,
         "2" = 5,
         "3" = 6,
         "4" = 7,
         "5" = 8) %>%
    gather(3:8, key = "Time", value = "Values")
  
  return(db_new)
  
}

fit_gamm <- function(full_db, priors, package_name) {
  
  if(package_name == "brms") {
  mod <- brm(Values ~ Condition + s(Time, by = Condition, k = 5), 
                     data = full_db,
                     family = gaussian(), 
                     prior = priors,
                     #cores=4, 
                     #chains=4, 
                     #sample_prior = "only", 
                     seed = 1234,
                     control = list(adapt_delta = 0.9))
  } else if(package_name == "rstanarm") {
    mod <- stan_gamm4(Values ~ Condition + s(Time, by = Condition, k = 5),
                    #random = ~(1 | senior),
                    data = full_db, 
                    family = gaussian, 
                    #QR = TRUE,
                    chains = 4, 
                    cores = 4, 
                    seed = 1234
    )
  }
  
  
  return(mod)
  
} 







