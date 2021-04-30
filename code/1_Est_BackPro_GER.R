library(tidyverse)
library(lubridate)
library(rstan)
library(zoo)
options(mc.cores = 4)
day = ymd("2021-04-27")
dat = read_tsv(paste0("../data/", day, "_divi_ger_reports.csv"))

stay_dist = readRDS("./stay_dist_rki.rds")
rev_surv_s = rev(c(1-cumsum(stay_dist$pmf)))

mod_icu_logl_smooth=stan_model("./stan_models/mod_icu_new_smooth_logl_comment.stan")


res_icu_ger = sampling(mod_icu_logl_smooth, 
                   data = list(T=length(dat$icu),
                               icu_occu=dat$icu,
                               S=length(rev_surv_s)-1,
                               rev_surv_s=rev_surv_s,
                               lambda_1_e=log(dat$icu[1]/6)), 
                   iter=2000, 
                   chains=4, 
                   verbose=TRUE)


res_icu_new_post = extract(res_icu_ger)
mod_res = tibble(date=seq(min(dat$date)-(length(rev_surv_s)-1), max(dat$date), by="1 day"),
                 new_icu_med = apply(exp(res_icu_new_post$log_lambda),2,median),
                 new_icu_ci_lwr = apply(exp(res_icu_new_post$log_lambda),2,function(x) quantile(x, 0.025)),
                 new_icu_ci_upr = apply(exp(res_icu_new_post$log_lambda),2,function(x) quantile(x, 0.975)))
write_tsv(mod_res, path = paste0("../results/",day, "_backpro_ger_res.csv"))
