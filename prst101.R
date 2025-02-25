library(marinecs100b)
library(tidyverse)


# Frequency of occurence --------------------------------------------------
# P1 Filter cctd_meso to rows representing the predator with id 99112. What was
# species was the predator? How many prey items were in their diet sample? How
# many of those prey were mesopelagic or coastal pelagic fish species?

predator_99112 <- cctd_meso %>%
  filter(predator_id == 99112) %>%
  group_by(predator_common_name, prey_common_name) %>%
  summarize(any_meso = any(meso_prey),
            any_cpf = any(cpf_prey),
            .groups = "drop")
predator_99112
#predator: california sea lion
#5 prey
#1 is meso, 3 are cpf


# P2 Summarize cctd_meso at the predator level (i.e., each row in the summary
# should represent one predator), indicating whether the predator’s diet sample
# contained mesopelagic and/or coastal pelagic fish species. Call the summary
# columns any_meso and any_cpf.
predator <- cctd_meso %>%
  group_by(predator_common_name) %>%
  summarize(any_meso = any(meso_prey),
            any_cpf = any(cpf_prey),
            .groups = "drop")
predator
cctd_meso
# P3 Using the data frame you created in P2, create a species-level summary that
# contains columns for mesopelagic FO (meso_fo), coastal pelagic fish FO
# (cpf_fo), and predator sample size (n).
species_summary <- cctd_meso %>%
  group_by(predator_scientific_name) %>%
  summarize(meso_fo = mean(meso_prey),
            cpf_fo = mean(cpf_prey),
            n = n())
print(species_summary, n = 143)
species_summary
# P4 How many predator species had a mesopelagic FO greater than 0.5? Which of
# those predator species had the largest sample size?
species_summary_f <- cctd_meso %>%
  group_by(predator_scientific_name) %>%
  summarize(meso_fo = mean(meso_prey),
            cpf_fo = mean(cpf_prey),
            n = n()) %>%
  filter(meso_fo  > 0.5)
species_summary_f

#one predator species has a mesopelagic FO greater than 0.5
#northern right whale dolphin
#Sample size of 803

# Simulating samples ------------------------------------------------------

set.seed(123)

# P5 In the sample, Lissodelphis had a slightly higher mesopelagic FO than
# Delphinus. Do you think that relative order reflects the true difference in
# the population? Why or why not?
#Yes, because the sample sizes are large and the difference in frequency is not
#insignificant
# P6 Fill in the blanks below to simulate 1000 new samples. Keep the size and
# probabilities the same as the original sample. Of the 1000 simulated samples,
# what fraction show the wrong order (i.e., mesopelagic FO greater in Delphinus
# than Lissodelphis)?

lissodelphis_samples <- rbinom(1000,
                               size = 803,
                               prob = 0.547)
lissodelphis_fo <- lissodelphis_samples / 803
delphinus_samples <- rbinom(1000,
                              size = 2822,
                              prob = 0.472)
delphinus_fo <- delphinus_samples / 2822
wrong_order <- length(which(lissodelphis_fo < delphinus_fo))/100
wrong_order
#0
  # P7 How does your result in P6 influence your confidence in the sample result
  # that Lissodelphis consume mesopelagic prey more frequently than Delphinus?
#It makes me more confident

  # P8 If you were to get new samples of the same size for these two taxa, which
  # mesopelagic FO do you think would change more? Why?
#lissodelphis because it has the smaller sample size

  # P9 Generate 1000 new simulated samples for Histioteuthidae and Dosidicus
  # gigas, keeping the sample sizes and probabilities the same.
histioteuthidae_samples <- rbinom(1000,
                               size = 93,
                               prob = 0.355)
histioteuthidae_fo <- histioteuthidae_samples / 93
dosidicus_giga_samples <- rbinom(1000,
                            size = 2877,
                            prob = 0.339)
dosidicus_giga_fo <- dosidicus_giga_samples / 2877
  # P10 What’s the mean mesopelagic FO of the 1000 Histioteuthidae simulated
  # samples? How about Dosidicus gigas? How do these means compare to the original
  # sample?
mean(histioteuthidae_fo)
#0.3533226
mean(dosidicus_giga_fo)
#0.3390379
#very similar to original sample
  # P11 What’s the standard deviation of mesopelagic FO across simulated samples
  # for the two taxa?
sd(histioteuthidae_fo)
#0.05049549
sd(dosidicus_giga_fo)
#0.009041675
  # P12 How frequently did Histioteuthidae mesopelagic FO fall outside the range
  # 0.45 - 0.65? How about Dosidicus gigas?
(length(which(histioteuthidae_fo > 0.65)) + length(which(histioteuthidae_fo < 0.45)))/1000
#0.975
(length(which(dosidicus_giga_fo > 0.65)) + length(which(dosidicus_giga_fo < 0.45)))/1000
#1
  # P13 Based on your answers to P10-P12, what effect does sample size have on
  # sample accuracy?
#Increasing sample size means increasing sample accuracy
