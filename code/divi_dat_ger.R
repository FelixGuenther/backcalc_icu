# Load equired packages
library(tidyverse)
library(lubridate)
library(rstan)
library(zoo)
options(mc.cores = 4)

#' Load German data
dat = read_tsv("../data/divi_ger_reports.csv") %>% filter(date<ymd("2021-02-09"))
#' Load assumed distribution of ICU-LOS
stay_dist = readRDS("./stay_dist_rki.rds")
#' Obtain survival probability and revert the vector -> c(P(LOS>=40), ..., P(LOS>=0))
rev_surv_s = rev(c(1-cumsum(stay_dist$pmf)))

#' Load Stan model for back-calculation
mod_icu_new_smooth=stan_model("./stan_models/mod_icu_new_smooth.stan")
#' Perform MCMC-Sampling (Estimation)
#' Requires a data list with entries:
#' T: Length of time-series of daily number of occupied beds
#' icu_occu: time-series of daily number of occupied beds
#' S: Maximum LOS
#' rev_surv_s: Vector of LOS 'survival probabilities' as defined above
#' lambda_1_e: mean of prior for expected number of (unobserved) now ICU admissions 
#' on day 1

res_icu = sampling(mod_icu_new_smooth, 
                   data = list(T=length(dat$icu),
                               icu_occu=dat$icu,
                               S=length(rev_surv_s)-1,
                               rev_surv_s=rev_surv_s,
                               lambda_1_e=dat$icu[1]/length(rev_surv_s)), 
                   iter=2000, 
                   chains=4, 
                   seed=2534)
print(warnings())
# Get posterior and compare to true data
lambda_post = extract(res_icu)$lambda
sd_first_order_post = extract(res_icu)$sd_first_order
quantile(sd_first_order_post, probs = c(0.025, 0.5, 0.975))

posterior = t(apply(lambda_post, MARGIN = 2, function(x) c(med = median(x), 
                                                           q025=as.numeric(quantile(x, 0.025)), 
                                                           q975=as.numeric(quantile(x, .975))))) %>%
  as_tibble() %>% mutate(dist="Estimated",
                         date = seq(min(dat$date)-length(rev_surv_s)+1, max(dat$date), by = "1 day")) %>%
  full_join(dat) %>% filter(date>=ymd("2020-10-23")) 

# Observed data of occupied beds
theme_set(theme_bw())
plot_icu = posterior %>% ggplot() + 
  geom_line(aes(date, icu), lty=2) + 
  ylim(c(0,6000)) + ylab("ICU Occupied beds") +
  coord_cartesian(xlim=c(ymd("2020-10-23", ymd("2021-02-08"))))

# True and estimated number of ICU admissions
plot_icu_new = ggplot(posterior) +
  geom_line(aes(date, med, col = dist)) +
  geom_ribbon(aes(date, ymin=q025, ymax=q975, fill=dist), alpha=.2) +
  geom_line(aes(date, icu_new), col = "green", lty=2, lwd=0.2) +
  geom_smooth(aes(date, icu_new), col = "green", se = FALSE, lty=2) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  scale_color_brewer(type="qual", palette = 2) +
  scale_fill_brewer(type="qual", palette = 2) +
  ylim(0,1600) + ylab("ICU admission (GER)") +
  coord_cartesian(xlim=c(ymd("2020-10-23", ymd("2021-02-08"))))



fig_icu_ger = ggpubr::ggarrange(plot_icu, plot_icu_new, nrow=2, vjust = T)
ggsave(fig_icu_ger, filename = "../results/divi_ger_eval.png", width=6, height = 5)
