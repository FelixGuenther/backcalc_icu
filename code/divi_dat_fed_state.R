# Load required packages
library(tidyverse)
library(lubridate)
library(rstan)
options(mc.cores = 4)
# Read DIVI data on occupied beds per federal state
dat = readRDS("../data/divi_federal_states.rds")
# Reduce to days starting from Sept, 1st
dat_redu = dat %>% select(date=daten_stand,
                          state,
                          covid_cases=faelle_covid_aktuell) %>%
  filter(date>=ymd("2020-09-01"),
         state!="Deutschland")

# Combine Berlin and Brandenburg
dat_redu_berlin_brandeburg = dat_redu %>% filter(state %in% c("Berlin", "Brandenburg")) %>%
  group_by(date) %>% 
  summarise(covid_cases=sum(covid_cases)) %>%
  mutate(state="Berlin-Brandenburg")

dat_redu = dat_redu %>%
  filter(!(state %in% c("Berlin", "Brandenburg"))) %>%
  rbind(dat_redu_berlin_brandeburg)
# Which states in data
states = dat_redu %>% pull(state) %>% unique()
#' Load assumed distribution of ICU-LOS
stay_dist = readRDS("./stay_dist_rki.rds")
#' Obtain survival probability and revert the vector -> c(P(LOS>=40), ..., P(LOS>=0))
rev_surv_s = rev(c(1-cumsum(stay_dist$pmf)))
#' Load Stan model for back-calculation
mod_icu_new_smooth=stan_model("./stan_models/mod_icu_new_smooth.stan")

# Function to perform estimation of admission time-series per state
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
                                 lambda_1_e=icu_occu[1]/length(rev_surv_s)), 
                     iter=2000, chains=4)
  print(warnings())
  
  lambda_post = extract(res_icu)$lambda
  posterior = t(apply(lambda_post, MARGIN = 2, function(x) c(med = median(x), 
                                                             q025=as.numeric(quantile(x, 0.025)), 
                                                             q975=as.numeric(quantile(x, .975))))) %>%
    as_tibble() %>% 
    mutate(date = seq(min(dat_state$date)-length(rev_surv_s)+1, max(dat_state$date), by = "1 day"),
           t=1:n(),
           state=state_sel) %>% right_join(dat_state %>% filter(state==state_sel)) 
}

# Perform estimation for each state
res_list = lapply(states, function(x) est_new_icu_state(state=x))
# Get posterior over all federal states
res = do.call(rbind, res_list)

# Plot estimated expected number of ICU admissions per day
theme_set(new = theme_bw())
fig = res %>% filter(date>=ymd("2020-09-01"), state!="Bremen") %>% 
  mutate(med=med,
         q025=q025,
         q975=q975) %>%
  ggplot() +
  geom_vline(aes(xintercept=date), 
             data = tibble(date=c(ymd("2021-02-01"),
                                  ymd("2021-01-01"),
                                  ymd("2020-12-01"),
                                  ymd("2020-11-01"),
                                  ymd("2020-10-01"),
                                  ymd("2020-09-01"))), 
             lty=1, col="lightgrey") +
  geom_line(aes(date, med)) +
  geom_ribbon(aes(date, ymin=q025, max=q975), alpha=.3) +
  xlab("Day") + ylab("Number ICU admission") +
  # geom_line(aes(date, covid_cases)) +
  facet_wrap(~state)

ggsave(fig, filename = "../results/new_icu_state.png", width=8, height = 6)  
