library(marinecs100b)
library(tidyverse)


# Frequency of occurence --------------------------------------------------

predator_scale <- cctd_meso %>%
  group_by(predator_id, predator_scientific_name) %>%
  summarize(any_meso = any(meso_prey),
            any_cpf = any(cpf_prey),
            .groups = "drop")

species_scale <- predator_scale %>%
  group_by(predator_scientific_name) %>%
  summarize(fo_meso = mean(any_meso),
            fo_cpf = mean(any_cpf),
            n_predators = n())

species_scale %>%
  # Only retain predator species that ate mesopelagic fish
  filter(fo_meso > 0) %>%
  # Flip sign of coastal pelagic FO (for plotting purposes)
  mutate(fo_cpf = -fo_cpf,
         # Format name and sample size
         label = sprintf("%s (%d)", predator_scientific_name, n_predators),
         # Order labels by descending mesopelagic FO
         label = fct_reorder(label, fo_meso)) %>%
  # Rename fo_* columns to human-readable
  rename(`Coastal pelagic fish` = fo_cpf,
         `Mesopelagic fish` = fo_meso) %>%
  # Pivot frequency of occurence columns to long format
  pivot_longer(ends_with("Fish"), values_to = "fo") %>%
  # Create column chart
  ggplot(aes(x = fo, y = label, fill = name)) +
  geom_col(color = "grey10") +
  scale_x_continuous("Frequency of occurence", limits = c(-1, 1)) +
  labs(y = "Predator taxa") +
  scale_fill_manual("Prey type", values = c("grey70", "navy")) +
  theme_bw() +
  theme(axis.text.y = element_text(face = "italic"),
        legend.position = "inside",
        legend.position.inside = c(0.99, 0.01),
        legend.justification = c(1, 0))
ggsave("~/Downloads/meso_cpf_fo.png", width = 8, height = 6, units = "in")


# Simulate order ----------------------------------------------------------

# How sensitive is the order of these five species to sampling?
set.seed(123)

# Two species consumed mesopelagic fish more frequently in the CCTD sample than
# any other taxa: Lissodelphis borealis and Delphinus delphis. In the sample,
# Lissodelphis had a slightly higher mesopelagic FO than Delphinus. Do you think
# that relative order reflects the true difference in the population? Why or why
# not?

# Simulate sampling the same number of Lissodelphis borealis and Delphinus
# delphis stomachs as in the observed sample, keeping the probability of
# observing mesopelagic fish prey the same as the observed sample. Repeat 1000
# times. How frequently is the Delphinus mesopelagic fish FO *greater* than the
# Lissodelphis FO in the simulations (the reverse of the observed order in the
# sample)?
lissodelphis_meso <- rbinom(1000,
                            size = 56,
                            prob = 0.893)
lissodelphis_fo <- lissodelphis_meso / 56
delphinus_meso <- rbinom(1000,
                         size = 259,
                         prob = 0.857)
delphinus_fo <- delphinus_meso / 259
sum(delphinus_fo > lissodelphis_fo)

# How confident are you that the mesopelagic FO ranks in the observed sample
# reflect the population as a whole?

# Mesopelagic fish were found in the stomachs of Histioteuthidae and Dosidicus
# gigas at very similar frequencies - 0.553 compared to 0.522. The sample
# contained far more Dosidicus than Histioteuthidae, though - 1136 Dosidicus
# compared to 47 Histioteuthidae. If you were to get new samples of the same
# size for these two taxa, which FO do you think would change more?

# Simulate the Histioteuthidae and Dosidicus samples (keeping size and
# probability the same as the observed sample) 1000 times and calculate the FO.
histioteuthidae_random <- rbinom(1000, size = 47, prob = 0.553)
histioteuthidae_fo <- histioteuthidae_random / 47
dosidicus_random <- rbinom(1000, size = 1136, prob = 0.522)
dosidicus_fo <- dosidicus_random / 1136

# What's the mean FO across simulations for the two taxa? How does that compare
# to the original sample?

mean(histioteuthidae_fo)
mean(dosidicus_fo)

# What's the standard deviation of FO across simulations for the two taxa?

sd(histioteuthidae_fo)
sd(dosidicus_fo)

# How frequently did Histioteuthidae FO fall outside the range 0.45 - 0.65? How
# about Dosidicus?

sum(histioteuthidae_fo > 0.65)
sum(histioteuthidae_fo < 0.45)
sum(dosidicus_fo > 0.65)
sum(dosidicus_fo < 0.45)

# Based on the previous problems, describe the effect of sample size on the
# accuracy of a sample.
