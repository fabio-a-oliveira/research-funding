# Housekeeping -----------------------------------------------------------------

if (! "tidyverse" %in% installed.packages()) {install.packages("tidyverse")}
if (! "ggplot2" %in% installed.packages()) {install.packages("ggplot2")}
if (! "dslabs" %in% installed.packages()) {install.packages("dslabs")}

library(tidyverse)
library(dslabs)
library(ggplot2)

options(digits = 3)

data("research_funding_rates")

# Raw data ---------------------------------------------------------------------

names(research_funding_rates)
research_funding_rates$discipline

# Confidence intervals ---------------------------------------------------------

combined_results <- 
  research_funding_rates %>%
  summarize(grants = sum(awards_total),
            applications = sum(applications_total),
            success_rate = grants/applications,
            standard_error_estimate = 
              sqrt(success_rate*(1-success_rate)/applications),
            ci_lower = qnorm(0.025,success_rate,standard_error_estimate),
            ci_upper = qnorm(0.975,success_rate,standard_error_estimate))

combined_results_by_gender <-
  research_funding_rates %>%
  mutate(Men = paste(awards_men,applications_men,
                     success_rates_men),
         Women = paste(awards_women,applications_women,
                       success_rates_women)) %>%
  select(discipline,Men,Women) %>%
  gather("gender","data",Men:Women) %>%
  mutate(grants = 
           sapply(data,function(d){unlist(strsplit(d,split=" "))}[1]),
         applications = 
           sapply(data,function(d){unlist(strsplit(d,split=" "))}[2]),
         success_rate = 
           sapply(data,function(d){unlist(strsplit(d,split=" "))}[3])) %>%
  mutate(gender = as.factor(gender),
         grants = as.numeric(grants),
         applications = as.numeric(applications),
         success_rate = as.numeric(success_rate)/100) %>%
  select(-data) %>%
  group_by(gender) %>%
  summarize(grants = sum(grants),
            applications = sum(applications),
            success_rate = grants/applications,
            standard_error_estimate = 
              sqrt(success_rate*(1-success_rate)/applications),
            ci_lower = qnorm(0.025,success_rate,standard_error_estimate),
            ci_upper = qnorm(0.975,success_rate,standard_error_estimate))

# Visualization of differences between gender ----------------------------------

combined_results_by_gender %>%
  add_case(gender="Combined",
           grants=combined_results$grants,
           applications=combined_results$applications,
           success_rate=combined_results$success_rate,
           standard_error_estimate=combined_results$standard_error_estimate,
           ci_lower=combined_results$ci_lower,
           ci_upper=combined_results$ci_upper) %>%
  ggplot(aes(x=success_rate,y=gender)) +
  geom_vline(xintercept=combined_results$success_rate,
             color='red',size=2) +
  geom_vline(xintercept=combined_results$success_rate,
             color='red',size=2) +
  geom_point(size=5) +
  geom_errorbar(aes(xmin=ci_lower,xmax=ci_upper),
                size=2, width=.5) +
  ggtitle("Investigation of gender bias in research funding") +
  xlab("Gender bias \n (95% confidence interval of the probability of receiving
       a grant according to gender)") +
  ylab("Gender") +
  scale_x_continuous(labels = scales::percent)


# Analysis ---------------------------------------------------------------------

totals <- research_funding_rates %>%
  select(-discipline) %>%
  summarize_all(funs(sum)) %>%
  summarize(yes_men = awards_men,
            no_men = applications_men - awards_men,
            yes_women = awards_women,
            no_women = applications_women - awards_women)

two_by_two <- 
  data.frame(gender = c("men","women"),
             yes = c(totals$yes_men,totals$yes_women), 
             no = c(totals$no_men,totals$no_women))

two_by_two %>%
  select(-gender) %>%
  chisq.test()

odds_ratio <-
  (totals$yes_men / totals$no_men) / 
  (totals$yes_women / totals$no_women)
odds_ratio


# Monte Carlo simulation -------------------------------------------------------

N <- 10000

simulation <- replicate(N,{
  results_men <- 
    sample(c(1,0),
           combined_results_by_gender$applications
           [combined_results_by_gender$gender=="Men"],
           prob = c(combined_results$success_rate,
                    1-combined_results$success_rate),
           replace = TRUE) %>%
    sum()
  results_women <- 
    sample(c(1,0),
           combined_results_by_gender$applications
           [combined_results_by_gender$gender=="Women"],
           prob = c(combined_results$success_rate,
                    1-combined_results$success_rate),
           replace = TRUE) %>%
    sum()
  c(results_men,results_women)
})

monte_carlo_results <-
  data.frame(grants_men = simulation[1,],
             grants_women = simulation[2,],
             applications_men = combined_results_by_gender$applications
             [combined_results_by_gender$gender=="Men"],
             applications_women = combined_results_by_gender$applications
             [combined_results_by_gender$gender=="Women"]) %>%
  mutate(success_rate_men = grants_men / applications_men,
         success_rate_women = grants_women / applications_women,
         spread = success_rate_men - success_rate_women)

monte_carlo_results$spread %>% quantile(c(0.025,0.975))

actual_spread <-
  combined_results_by_gender$success_rate[combined_results_by_gender$gender == "Men"] - 
  combined_results_by_gender$success_rate[combined_results_by_gender$gender == "Women"]

standard_error_estimate <-
  sqrt(
  combined_results_by_gender$standard_error_estimate[combined_results_by_gender$gender == "Men"]^2 +
  combined_results_by_gender$standard_error_estimate[combined_results_by_gender$gender == "Women"]^2)
  
mean(monte_carlo_results$spread >= actual_spread | monte_carlo_results$spread <= -actual_spread)

# Analysis of the spread per discipline-----------------------------------------

totals_per_discipline <-
  research_funding_rates %>%
  mutate(success_rates_men = success_rates_men/100,
         success_rates_women = success_rates_women/100) %>%
  mutate(se_men = sqrt(success_rates_men*(1-success_rates_men)/applications_men),
         se_women = sqrt(success_rates_women*(1-success_rates_women)/applications_women),
         spread = success_rates_men - success_rates_women,
         se_spread = sqrt(se_men^2 + se_women^2)) %>%
  select(discipline,success_rates_men,success_rates_women,se_men,se_women,spread,se_spread) %>%
  add_case(discipline = "Combined",
           spread = actual_spread,
           se_spread = standard_error_estimate) %>%
  mutate(p_null = pnorm(0,spread,se_spread),
         ci_lower = qnorm(0.025,spread,se_spread),
         ci_upper = qnorm(0.975,spread,se_spread),
         include_zero = 0>qnorm(0.025,spread,se_spread) & 0<qnorm(0.975,spread,se_spread),
         favorite = ifelse(spread>0,"Men","Women")) %>%
  arrange(spread)

totals_per_discipline %>%
  select(discipline,spread,ci_lower,ci_upper,favorite)

# Plot results -----------------------------------------------------------------
  
totals_per_discipline %>%
  ggplot(aes(x=spread,y=discipline)) +
  geom_point(size=5) +
  labs(title = "Gender bias in research funding",
       subtitle = "based on data from the research_funding_rates database contained in the dslabs library") +
  xlab("Gender bias \n (95% confidence interval of the difference in probability of receiving a grant according to gender)") +
  ylab("Discipline") +
  scale_y_discrete(limits = c("Technical sciences",
                              "Social sciences",
                              "Physics",
                              "Physical sciences",
                              "Medical sciences",
                              "Interdisciplinary",
                              "Humanities",
                              "Earth/life sciences",
                              "Chemical sciences",
                              "Combined")) +
  scale_x_continuous(breaks = seq(-.40,.40,.1), 
                     minor_breaks=seq(-.40,.40,.01),
                     labels = scales::percent) +
  coord_cartesian(xlim=c(-.35,.35),
                  ylim=c(1.1,9.7)) +
  geom_vline(xintercept = combined_spread, color = 'green', size = 18, alpha = 0.2) +
  geom_vline(xintercept = .20, color = 'blue', size = 108, alpha = 0.2) +
  geom_vline(xintercept = -.20, color = 'red', size = 108, alpha = 0.2) +
  geom_vline(xintercept=0,color='red', size = 2) +
  geom_errorbar(aes(xmin = spread-1.96*se_spread, xmax = spread+1.96*se_spread),
              size=2,width=.5) +
  geom_errorbar(aes(y = "Combined",
                    x = combined_spread,
                    xmin = qnorm(0.025,combined_spread,combined_se),
                    xmax = qnorm(0.975,combined_spread,combined_se)),
                size=2,width=.5,color='blue') +
  geom_point(aes(y = "Combined",
                 x = combined_spread),
             size=5, color='blue')

# Proportion of applications per discipline ------------------------------------

gender_ratio <-
  research_funding_rates %>%
    mutate(spread = success_rates_men/100 - success_rates_women/100,
           men_women_ratio = applications_men/applications_women,
           men_proportion = applications_men/(applications_men+applications_women),
           prevalence = ifelse(men_proportion>.5,"Men","Women")) %>%
    select(discipline,spread,men_women_ratio,men_proportion,prevalence) %>%
    arrange(-men_proportion)

gender_ratio

gender_ratio %>%
  ggplot(aes(x=men_proportion,y=spread)) +
  geom_ribbon(inherit.aes = FALSE,
              mapping = aes(x=seq(-1,2,length.out=9),
                            ymax=.2,
                            ymin=0),
              alpha = 0.2, fill =  'blue') +
  geom_ribbon(inherit.aes = FALSE,
              mapping = aes(x=seq(-1,2,length.out=9),
                            ymax=0,
                            ymin=-.2),
              alpha = 0.2, fill =  'red') +
  geom_hline(yintercept = 0, color='red',size=2) +
  geom_label(aes(label=discipline),
             nudge_x=.05,nudge_y=0,size=3) +
  geom_point(size=3) +
  scale_x_continuous(labels=scales::percent) +
  scale_y_continuous(labels=scales::percent) +
  labs(title = "Gender bias vs gender prevalence in funding requests",
       subtitle = "based on data from the research_funding_rates database contained in the dslabs library") +
  xlab("Proportion of requests from male researchers") +
  ylab("Observed difference in accepted requests") +
  coord_cartesian(xlim=c(.45,.95),
                  ylim=c(-.12,.12))

# Analysis of the spread -------------------------------------------------------

totals_per_gender <- data.frame(gender = c("Male","Female"),
                                success_rate = c(totals$yes_men/(totals$yes_men + totals$no_men),
                                                 totals$yes_women/(totals$yes_women + totals$no_women)),
                                sample_size = c(totals$yes_men + totals$no_men, totals$yes_women + totals$no_women)) %>%
  mutate(se = sqrt(success_rate*(1-success_rate)/sample_size))

combined_spread <- totals_per_gender$success_rate[totals_per_gender$gender=="Male"] - totals_per_gender$success_rate[totals_per_gender$gender=="Female"]
combined_se <- sqrt(totals_per_gender$se[totals_per_gender$gender=="Male"]^2 + totals_per_gender$se[totals_per_gender$gender=="Female"]^2)

# Plausibility of results under null hypothesis --------------------------------

p_null_hypothesis <- pnorm(0,combined_spread,combined_se)
p_null_hypothesis

# Trash ------------------------------------------------------------------------
  
# research_funding_rates %>%
#   mutate(winner = ifelse(success_rates_men>success_rates_women,"Men","Women")) %>%
#   group_by(winner)%>%
#   summarize(mean(success_rates_total))
# 
# 
# a <- paste(research_funding_rates$discipline, sep = " / ")
# a
# paste(a)
# a
# 
# paste(levels(research_funding_rates$discipline))
# levels(as.factor(research_funding_rates$discipline)) %>% paste()
# 
# 
# 
# mutate(discipline = factor(discipline,levels=c("Combined",
#                                                "Chemical sciences",
#                                                "Earth/life sciences",
#                                                "Humanities",
#                                                "Interdisciplinary",
#                                                "Medical sciences",
#                                                "Physical sciences",
#                                                "Physics",
#                                                "Social sciences",
#                                                "Technical sciences")))


# stocks <- tibble(
#   time = as.Date('2009-01-01') + 0:9,
#   X = rnorm(10, 0, 1),
#   Y = rnorm(10, 0, 2),
#   Z = rnorm(10, 0, 4)
# )
# 
# gather(stocks, "stock", "price", -time)





# confidence_intervals <-
#   data.frame(gender = c("Male","Female"),
#              ci_lower = 
#                c(quantile(monte_carlo_results$success_rate_men,0.025),
#                  quantile(monte_carlo_results$success_rate_women,0.025)),
#              ci_upper = 
#                c(quantile(monte_carlo_results$success_rate_men,0.975),
#                  quantile(monte_carlo_results$success_rate_women,0.975)))