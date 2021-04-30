library(tidyverse)
library(lubridate)
library(zoo)
theme_set(theme_bw())
date = ymd("2021-04-27")

dat_ger = read_tsv(paste0("../data/", date, "_divi_ger_reports.csv"))
dat_ger_res = read_tsv(paste0("../results/", date, "_backpro_ger_res.csv"))
dat_fed = read_tsv(paste0("../results/", date, "_backpro_fed_state.csv"))

# Estimate smooth model for daily new admissions in Germany as a whole
bel_smooth = mgcv::gam(icu_new ~ s(t), family = "poisson", data = dat_ger %>% mutate(t=1:n()))

dat_ger_sm = dat_ger %>% select(date) %>%
  mutate(icu_new_smooth_ex = exp(predict(bel_smooth, newdata = tibble(t=1:n()))),
         icu_new_smooth_ex_lwr = exp(log(icu_new_smooth_ex) - 
                                       2*predict(bel_smooth, newdata = tibble(t=1:n()),
                                                 se.fit = T)[[2]]),
         icu_new_smooth_ex_upr = exp(log(icu_new_smooth_ex) + 
                                       2*predict(bel_smooth, newdata = tibble(t=1:n()),
                                                 se.fit = T)[[2]]))

dat_ger_sm %>% rename(est=icu_new_smooth_ex, q025=icu_new_smooth_ex_lwr, q975=icu_new_smooth_ex_upr) %>%
  write_tsv(paste0("../results/", date, "_ger_new_sm.csv"))
# Create Plot for Germany
plot_bel = dat_ger %>% ggplot() + geom_line(aes(date, icu)) + ylab("Belegung")
plot_new = dat_ger %>% select(date, icu=icu_new) %>%
  mutate(icu_lwr=NA, icu_upr=NA, type="Beobachtet") %>%
  rbind(dat_ger_res %>% select(date, icu=new_icu_med, icu_lwr=new_icu_ci_lwr,
                               icu_upr=new_icu_ci_upr) %>% mutate(type="Aus Belegung")) %>%
  rbind(dat_ger_sm %>% select(date, icu=icu_new_smooth_ex, icu_lwr=icu_new_smooth_ex_lwr,
                              icu_upr = icu_new_smooth_ex_upr) %>% mutate(type="Aus beobachteten\nNeuaufnahmen")) %>%
  filter(date>=ymd("2020-10-23")) %>%
  ggplot() + 
  geom_line(aes(date, icu, col=type, lty=type)) +
  geom_ribbon(aes(date, ymin=icu_lwr, ymax=icu_upr, fill = type), alpha=.2) +
  scale_linetype_manual(values = c("Aus beobachteten\nNeuaufnahmen"=1, "Aus Belegung"=1, "Beobachtet"=2)) +
  scale_color_manual(values = c("Aus beobachteten\nNeuaufnahmen"="darkgreen", "Aus Belegung"="purple", "Beobachtet"="black")) +
  scale_fill_manual(values = c("Aus beobachteten\nNeuaufnahmen"="darkgreen", "Aus Belegung"="purple", "Beobachtet"="black")) +
  ylab("Neuaufnahmen") + theme(legend.position = "bottom", legend.title = element_blank()) +
  xlab("Datum") + guides(linetype = FALSE, fill=FALSE)

ggpubr::ggarrange(plot_bel, plot_new, ncol=2, labels = "AUTO", common.legend = T, legend = "bottom")

# Perform adjustment for federal states
# Derive correction factor lambda_t
dat_ger = dat_ger %>% left_join(dat_ger_res) %>% left_join(dat_ger_sm) %>%
  mutate(lambda_t = icu_new_smooth_ex_lwr/new_icu_med)

# Adjust estimates for federal states
dat_fed = dat_fed %>% right_join(dat_ger %>% select(date, lambda_t)) %>%
  mutate(med_adj=med*lambda_t,
         q025_adj = q025*lambda_t,
         q975_adj = q975*lambda_t)

# Plot results
dat_fed %>% select(date, state, med, q025, q975) %>%
  mutate(type = "unadj") %>%
  rbind(dat_fed %>% select(date, state, med=med_adj, q025=q025_adj, q975=q975_adj) %>%
          mutate(type = "adj")) %>%
  mutate(type=factor(type, levels = rev(c("adj", "unadj")), labels = rev(c("Model Belegung adjusted",
                                                                  "Model Belegung")))) %>%
  ggplot() +
  geom_vline(aes(xintercept=date), 
             data = tibble(date=c(ymd("2021-03-01"),
                                  ymd("2021-02-01"),
                                  ymd("2021-01-01"),
                                  ymd("2020-12-01"),
                                  ymd("2020-11-01"),
                                  ymd("2020-10-01"),
                                  ymd("2020-09-01"))), 
             lty=1, col="lightgrey") +
  geom_line(aes(date, med, col = type)) +
  geom_ribbon(aes(date, ymin=q025, max=q975, fill = type), alpha=.3) +
  facet_wrap(~state, scales = "free_y") +
  ylab("Neuaufnahmen") +
  theme(legend.position = "bottom", legend.title = element_blank())

