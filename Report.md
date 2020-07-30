Report - Gender Bias in Research Funding
================

## Introduction

### Motivation

This analysis is motivated by topics presented in the course of the Data
Science Professional Certificate, a set of MOOCs presented by HarvardX
and hosted on the edX distance learning platform.

**This report is not focused on presenting any novel insight on the data
or the subject, but rather on applying principles from this introductory
set of courses.** The original paper presenting this data contains
additional data and levels of nuance and which are not addressed here.

In particular, this report hinges on the statistical analysis tools
presented in [course \#4 (Inference and
Modeling)](https://courses.edx.org/courses/course-v1:HarvardX+PH125.4x+2T2020/course/)
and explores the R Markdown and Git features presented in [course \#5
(Productivity
Tools)](https://courses.edx.org/courses/course-v1:HarvardX+PH125.5x+2T2020/course/).
It also uses tools explored in [course \#2 (Data Visualization with
ggplot2)](https://courses.edx.org/courses/course-v1:HarvardX+PH125.2x+1T2020/course/).

### Dataset

The data under analysis in this report was taken from a [2015 PNAS
paper](https://www.pnas.org/content/112/40/12349) analyzing success
rates from research funding agencies in the Netherlands. The raw data is
contained in the *dslabs* R package, a library with numerous datasets
compiled for teaching purposes. Further information is available in
[this page](https://CRAN.R-project.org/package=dslabs).

### Instructions

Given the simplicity of the analysis, the entire code is reproduced in
the markdown file, without need for any additional script or saved data.
The code is also entirely echoed in this report.

If you are interested in reproducing this analysis, the
*ResearchFundingGenderBias.R* script available in the repository
performs the same tasks.

## Analysis

### Housekeeping

The following code loads (and installs, if not yet installed) the
libraries required for this analysis. It also loads the
*research\_funding\_rates* data from the *dslabs* package.

``` r
if (! "tidyverse" %in% installed.packages()) {install.packages("tidyverse")}
if (! "ggplot2" %in% installed.packages()) {install.packages("ggplot2")}
if (! "dslabs" %in% installed.packages()) {install.packages("dslabs")}

library(tidyverse)
library(dslabs)
library(ggplot2)

data("research_funding_rates")
```

### Raw data

First, we examine the raw data from the *dslabs* package:

``` r
# variables contained in the dataset
names(research_funding_rates)
##  [1] "discipline"          "applications_total"  "applications_men"   
##  [4] "applications_women"  "awards_total"        "awards_men"         
##  [7] "awards_women"        "success_rates_total" "success_rates_men"  
## [10] "success_rates_women"

# disciplines for which data is available
research_funding_rates$discipline
## [1] "Chemical sciences"   "Physical sciences"   "Physics"            
## [4] "Humanities"          "Technical sciences"  "Interdisciplinary"  
## [7] "Earth/life sciences" "Social sciences"     "Medical sciences"

# total number of applications
sum(research_funding_rates$applications_total)
## [1] 2823

# total number of grants
sum(research_funding_rates$awards_total)
## [1] 467

# total success rate
sum(research_funding_rates$awards_total) / sum(research_funding_rates$applications_total)
## [1] 0.165
```

The dataset includes, for each of the 9 disciplines, the number of
applications, grants received and success rates. It also includes the
same data discriminated by gender.

### Analysis of the overall success rates

Based on the success rates and number of samples for both men and women,
we can calculate a 95% confidence interval for the probability of
receiving a grant. This analysis is based on the premise that the
observed data is a sample distributed according to an underlying
probability which is unknown.

``` r
# summary statistics on the data
combined_results <- 
  research_funding_rates %>%
  summarize(grants = sum(awards_total),
            applications = sum(applications_total),
            success_rate = grants/applications,
            standard_error_estimate = sqrt(success_rate*(1-success_rate)/applications),
            ci_lower = qnorm(0.025,success_rate,standard_error_estimate),
            ci_upper = qnorm(0.975,success_rate,standard_error_estimate))

combined_results
##   grants applications success_rate standard_error_estimate ci_lower ci_upper
## 1    467         2823        0.165                 0.00699    0.152    0.179
```

From this we can see that there is an overall 15.2-17.9% chance (with
95% confidence)of obtaining a grant, across different disciplines and
genders.

We can also repeat this analysis, with data segregated between men and
women:

``` r
# create distinct observations for men and women, group data by gender and calculate summary for each group
combined_results_by_gender <-
  research_funding_rates %>%
  mutate(Men = paste(awards_men,applications_men,success_rates_men),
         Women = paste(awards_women,applications_women,success_rates_women)) %>%
  select(discipline,Men,Women) %>%
  gather("gender","data",Men:Women) %>%
  mutate(grants = sapply(data,function(d){unlist(strsplit(d,split=" "))}[1]),
         applications = sapply(data,function(d){unlist(strsplit(d,split=" "))}[2]),
         success_rate = sapply(data,function(d){unlist(strsplit(d,split=" "))}[3])) %>%
  mutate(gender = as.factor(gender),
         grants = as.numeric(grants),
         applications = as.numeric(applications),
         success_rate = as.numeric(success_rate)/100) %>%
  select(-data) %>%
  group_by(gender) %>%
  summarize(grants = sum(grants),
            applications = sum(applications),
            success_rate = grants/applications,
            standard_error_estimate = sqrt(success_rate*(1-success_rate)/applications),
            ci_lower = qnorm(0.025,success_rate,standard_error_estimate),
            ci_upper = qnorm(0.975,success_rate,standard_error_estimate))

combined_results_by_gender
## # A tibble: 2 x 7
##   gender grants applications success_rate standard_error_esti~ ci_lower ci_upper
##   <fct>   <dbl>        <dbl>        <dbl>                <dbl>    <dbl>    <dbl>
## 1 Men       290         1635        0.177              0.00945    0.159    0.196
## 2 Women     177         1188        0.149              0.0103     0.129    0.169
```

The data shows that there is a difference of approximately 2.8% in the
probability of obtaining a grant between men and women, which is
similarly reflected in the confidence intervals. A visualization helps
in evaluating the magnitudes:

``` r
# add observation with combined results and plot observed values and error bars
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
  xlab("Success rate in obtaining a grant \n (95% confidence interval of the probability of receiving a grant according to gender)") +
  ylab("Gender") +
  scale_x_continuous(labels = scales::percent)
```

![](Report_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

Despite confirming that there is a difference in the probability of
receiving a grant between men and women, the graph shows that both
confidence intervals have a significant overlap. Additionally, they both
include the total observed success rate, depicted as the red vertical
line in the figure.

Nevertheless, in order to evaluate the relevance of the observed
discrepancy, it is useful to perform a chi-squared test on the data as
well as calculate the odds ratio:

``` r
# create object with total yay/nay for each gender
totals <- research_funding_rates %>%
  select(-discipline) %>%
  summarize_all(sum) %>%
  summarize(yes_men = awards_men,
            no_men = applications_men - awards_men,
            yes_women = awards_women,
            no_women = applications_women - awards_women)

# format as two-by-two table used in chi-squared calculation
two_by_two <- 
  data.frame(gender = c("men","women"),
             yes = c(totals$yes_men,totals$yes_women), 
             no = c(totals$no_men,totals$no_women))

# perform chi-squared test
two_by_two %>%
  select(-gender) %>%
  chisq.test()
## 
##  Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  .
## X-squared = 4, df = 1, p-value = 0.05

# calculate odds ratio
odds_ratio <-
  (totals$yes_men / totals$no_men) / 
  (totals$yes_women / totals$no_women)
odds_ratio
## [1] 1.23
```

The test reveals that the associated p-value is approximately 0.051,
which indicates that, under the null hypothesis (no actual difference
between men an women), the probability of observing such a discrepancy
due to random variability is around 5.1%.

The odds ratio of 1.23, on the other hand, does not show a significant
difference among genders.

### Monte Carlo simulation

A Monte Carlo simulation is a convenient way of repeating an experiment
multiple times, under a certain set of assumptions, to determine if the
results fit our conclusions and the observed data.

Here, we perform 10000 iteractions under the following assumptions:

  - At each iteraction, a total of 1635 men and 1188 women are drawn,
    corresponding to the total amount of applications for each gender in
    the original data;
  - Each researcher will be randomly assigned a response of 1 (in case
    the application is granted) or 0 (in case the application is
    rejected), with a probability corresponding to the combined success
    rate observed in the data (with no gender bias);
  - After the simulation, the spread (difference in success rate between
    men and women) is calculated for each iteraction;
  - The 2.5% and 97.5% quantiles for the spread are calculated,
    indicating the range where 95% of the observations are contained;
  - The percentage of cases where the spread obtained was equal to or
    greater than the actual data in the dataset is calculated,
    indicating the likelihood that such a discrepancy is observed under
    the simulation assumptions.

<!-- end list -->

``` r
# set number of simulation iteractions
N <- 10000

# run simulation
simulation <- replicate(N,{
  results_men <- 
    sample(c(1,0),
           combined_results_by_gender$applications[combined_results_by_gender$gender=="Men"],
           prob = c(combined_results$success_rate,1-combined_results$success_rate),
           replace = TRUE) %>%
    sum()
  results_women <- 
    sample(c(1,0),
           combined_results_by_gender$applications[combined_results_by_gender$gender=="Women"],
           prob = c(combined_results$success_rate,1-combined_results$success_rate),
           replace = TRUE) %>%
    sum()
  c(results_men,results_women)
})

# gather results into a data frame
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

# calculate quantiles corresponding to the 95% range
monte_carlo_results$spread %>% quantile(c(0.025,0.975))
##    2.5%   97.5% 
## -0.0282  0.0279
```

The 2.5% and 97.5% quantiles are -0.028 and 0.028. The actual difference
observed in the data is slightly above the 97.5% quantile, at 2.8%.

In fact, we can see the percentage of iteractions which showed a spread
as big or bigger than the actual data:

``` r
# calculate the spread from the dataset
actual_spread <-
  combined_results_by_gender$success_rate[combined_results_by_gender$gender == "Men"] - 
  combined_results_by_gender$success_rate[combined_results_by_gender$gender == "Women"]

# estimate the standard error based on the number of observations
standard_error_estimate <-
  sqrt(
  combined_results_by_gender$standard_error_estimate[combined_results_by_gender$gender == "Men"]^2 +
  combined_results_by_gender$standard_error_estimate[combined_results_by_gender$gender == "Women"]^2)
  
# evaluate the simulation results for the percentage of cases with discrepancy matching the data
spread_extreme <- mean(monte_carlo_results$spread >= actual_spread | monte_carlo_results$spread <= -actual_spread)
spread_extreme
## [1] 0.0476
```

When comparing the results from the Monte Carlo simulation with the
actual spread observed in the dataset, we see that 4.8% of the
iteractions generated a discrepancy with a magnitude at least as large
as the one observed in the data, with no bias towards any of the genders
programmed into the simulation.

### Analysis of the discrepancies between men and women for each discipline

Another relevant point of analysis is whether there is any pattern in
the different success rates across different disciplines. Here we look
at the data for each particular discipline, arranged according to the
observed spread in success rates:

``` r
# calculate spread, estimated standard error and favored gender and arrange according to spread
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

# print relevant variables
totals_per_discipline %>%
  filter(discipline != "Combined") %>%
  select(discipline,spread,ci_lower,ci_upper,favorite)
##            discipline spread ci_lower ci_upper favorite
## 1   Interdisciplinary -0.104 -0.21396  0.00596    Women
## 2  Technical sciences -0.051 -0.16500  0.06300    Women
## 3          Humanities -0.050 -0.12517  0.02517    Women
## 4   Physical sciences -0.038 -0.18609  0.11009    Women
## 5   Chemical sciences  0.009 -0.15766  0.17566      Men
## 6     Social sciences  0.038 -0.00812  0.08412      Men
## 7             Physics  0.047 -0.24454  0.33854      Men
## 8    Medical sciences  0.076  0.01385  0.13815      Men
## 9 Earth/life sciences  0.101  0.01001  0.19199      Men
```

We see that, even though the total success rate favors male researchers,
this rate is actually more positive towards women in 4 disciplines.

The spread data per discipline, along with the calculated confidence
intervals, are represented in this figure:

``` r
# create and populate plot
totals_per_discipline %>%
  ggplot(aes(x=spread,y=discipline)) +
  geom_point(size=5) +
  labs(title = "Gender bias in research funding",
       subtitle = "based on data from the research_funding_rates database \r
       contained in the dslabs library") +
  xlab("Gender bias \n (95% confidence interval of the difference \r 
       in probability of receiving a grant according to gender)") +
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
  geom_vline(xintercept = actual_spread, color = 'green', size = 15, alpha = 0.2) +
  geom_vline(xintercept = .2, color = 'blue', size = 96, alpha = 0.2) +
  geom_vline(xintercept = -.2, color = 'red', size = 96, alpha = 0.2) +
  geom_vline(xintercept=0,color='red', size = 2) +
  geom_errorbar(aes(xmin = spread-1.96*se_spread, xmax = spread+1.96*se_spread),
              size=2,width=.5) +
  geom_errorbar(aes(y = "Combined",
                    x = actual_spread,
                    xmin = qnorm(0.025,actual_spread,standard_error_estimate),
                    xmax = qnorm(0.975,actual_spread,standard_error_estimate)),
                size=2,width=.5,color='blue') +
  geom_point(aes(y = "Combined",
                 x = actual_spread),
             size=5, color='blue')
```

![](Report_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

The figure demonstrates that the 95% confidence intervals for each
individual discipline are too large to make definitive statements about
one gender being systematically favored over the other. In fact, for 7
out of the 9 disciplines, the confidence interval includes the 0% spread
point, which indicates that the null hypothesis is plausible and that
the data does not permit a conclusive statement.

### Proportion of requests in different disciplines and relationship with spread

A final point to evaluate is whether there is any discernible pattern in
the proportion of men/women who requested grants across different
disciplines and if there a preference towards one gender in disciplines
where that gender is prevalent.

Here, we can use the proportion of applicants from each gender as a
proxy for the overall prevalence of a gender among the practitioners of
that discipline.

``` r
# calculate the ratio of men to women and the percentage of men for each discipline
gender_ratio <-
  research_funding_rates %>%
    mutate(spread = success_rates_men/100 - success_rates_women/100,
           men_women_ratio = applications_men/applications_women,
           men_proportion = applications_men/(applications_men+applications_women),
           prevalence = ifelse(men_proportion>.5,"Men","Women")) %>%
    select(discipline,spread,men_women_ratio,men_proportion,prevalence) %>%
    arrange(-men_proportion)

gender_ratio
##            discipline spread men_women_ratio men_proportion prevalence
## 1             Physics  0.047           7.444          0.882        Men
## 2   Physical sciences -0.038           3.462          0.776        Men
## 3  Technical sciences -0.051           3.048          0.753        Men
## 4   Chemical sciences  0.009           2.128          0.680        Men
## 5          Humanities -0.050           1.386          0.581        Men
## 6   Interdisciplinary -0.104           1.346          0.574        Men
## 7 Earth/life sciences  0.101           1.238          0.553        Men
## 8     Social sciences  0.038           1.039          0.510        Men
## 9    Medical sciences  0.076           0.942          0.485      Women
```

From the data, the only discipline where women outnumber men in number
of applications are the Medical sciences.

The figure below demonstrates the difference in funding success across
disciplines as a function of the proportion of male applicants.

``` r
# create and populate plot
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
       subtitle = "based on data from the research_funding_rates \r database contained in the dslabs library") +
  xlab("Proportion of requests from male researchers") +
  ylab("Observed difference in accepted requests") +
  coord_cartesian(xlim=c(.45,.95),
                  ylim=c(-.12,.12))
```

![](Report_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

We can see from the image that there does not appear to be any relation
between the difference in success rates according to the gender
prevalence in each particular discipline.

## Conclusion

The original paper concluded that:

> “results reveal gender bias favoring male applicants over female
> applicants”

From an evaluation restricted to the dataset used in this report, the
data does not seem to be sufficient to support this conclusion. The
dataset does show numbers that favors men, but not with a level of
certainty sufficient to affirm that there is a bias towards one gender.

Perhaps larger datasets, geographically spread and spanning several
years could be more conclusive.
