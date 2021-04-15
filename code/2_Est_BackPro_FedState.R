library(tidyverse)
library(lubridate)
library(rstan)
options(mc.cores = 4)

dat = readRDS("../data/divi_all.rds")
dat_redu = dat %>% select(date=daten_stand,
                          state,
                          covid_cases=faelle_covid_aktuell) %>%
  filter(date>=ymd("2020-09-01"),
         state!="Deutschland")

dat_redu_berlin_brandeburg = dat_redu %>% filter(state %in% c("Berlin", "Brandenburg")) %>%
  group_by(date) %>% 
  summarise(covid_cases=sum(covid_cases)) %>%
  mutate(state="Berlin & Brandenburg")

dat_redu_niedersachsen_bremen = dat_redu %>% filter(state %in% c("Niedersachsen", "Bremen")) %>%
  group_by(date) %>% 
  summarise(covid_cases=sum(covid_cases)) %>%
  mutate(state="Bremen & Niedersachsen")

dat_redu_hamburg_schleswig = dat_redu %>% filter(state %in% c("Hamburg", "Schleswig-Holstein")) %>%
  group_by(date) %>% 
  summarise(covid_cases=sum(covid_cases)) %>%
  mutate(state="Hamburg & Schleswig-Holstein")

dat_redu = dat_redu %>%
  filter(!(state %in% c("Berlin", "Bremen", "Brandenburg", "Niedersachsen", "Hamburg", "Schleswig-Holstein"))) %>%
  rbind(dat_redu_berlin_brandeburg) %>%
  rbind(dat_redu_hamburg_schleswig) %>%
  rbind(dat_redu_niedersachsen_bremen)
states = dat_redu %>% pull(state) %>% unique()
stay_dist = readRDS("./stay_dist_rki.rds")
rev_surv_s = rev(c(1-cumsum(stay_dist$pmf)))

mod_icu_new_smooth=stan_model("./stan_models/mod_icu_new_smooth_logl_comment.stan")

est_new_icu_state = function(state_sel) {
  print(as.character(state_sel))
  dat_state = dat_redu %>% filter(state==state_sel) %>%
    arrange(date)
  icu_occu = dat_state %>% pull(covid_cases)
  
  res_icu = sampling(mod_icu_new_smooth, 
                     data = list(T=length(icu_occu),
                                 icu_occu=icu_occu,
                                 S=length(rev_surv_s)-1,
                                 rev_surv_s=rev_surv_s,
                                 lambda_1_e=log(max(icu_occu[1]/6,1))), 
                     iter=2000, chains=4)

  lambda_post = exp(extract(res_icu)$log_lambda)
  posterior = t(apply(lambda_post, MARGIN = 2, function(x) c(med = median(x), 
                                                             q025=as.numeric(quantile(x, 0.025)), 
                                                             q975=as.numeric(quantile(x, .975))))) %>%
    as_tibble() %>% 
    mutate(date = seq(min(dat_state$date)-length(rev_surv_s)+1, max(dat_state$date), by = "1 day"),
           t=1:n(),
           state=state_sel) %>% right_join(dat_state %>% filter(state==state_sel)) 
}
# Estimate Backpro per federal state
res_list = lapply(states, function(x) est_new_icu_state(state=x))
# Combine posterior
res = do.call(rbind, res_list)
res = res %>% 
  select(date, state, med, q025, q975, bestand=covid_cases)

write_tsv(res, path = paste0("../results/", max(res$date), "_backpro_fed_state.csv"))

q(save = "no")
