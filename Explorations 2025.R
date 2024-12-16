setwd("~/Library/CloudStorage/OneDrive-TexasA&MUniversity/02 AGGIES Study/csv and R files")
source("1.5 Explorations OCD.R")
source("1.5 PB Explorations.R")

#install.packages("tidyverse")
library(tidyverse)  # For data manipulation and visualization
library(lmtest)     # For diagnostic tests

# Assuming "df" is your dataframe and "column_name" is the column you want to keep
df_ocs <- df_ocdex[, c("id", "currentsum", "cscreen")]
df_pcs <- df_pb_exp[, c("id", "care_average_p", "care_average_m", "protection_average_p", "protection_average_m")]

# Assuming "id" is the common column
combined_ocdpb_rough <- merge(df_pcs, df_ocs, by = "id", all = TRUE)

df_explorations <- na.omit(combined_ocdpb_rough)

# Extract relevant columns from each dataframe
#pb_subset <- df_pb_exp %>% select(care_average_p, care_average_m, protection_average_p, protection_average_m)
#df2_subset <- df_ocdex %>% select(currentsum)
formula <- currentsum ~ care_average_p + care_average_m + protection_average_p + protection_average_m

model <- lm(formula, data = df_explorations)

summary(model)
plot(model)
shapiro.test(model$residuals)
bptest(model)

library(ggplot2)

## Scatter Plot w/ Regression line for each parental bonding variable ####
ggplot(df_explorations, aes(x = care_average_p, y = currentsum)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Currentsum vs. Paternal Care Average",
       x = "Paternal Care Average",
       y = "Currentsum")


ggplot(df_explorations, aes(x = care_average_m, y = currentsum)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Currentsum vs. Maternal Care Average",
       x = "Maternal Care Average",
       y = "Currentsum")

ggplot(df_explorations, aes(x = protection_average_p, y = currentsum)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Currentsum vs. Paternal Overprotection Average",
       x = "Paternal Overprotection Average",
       y = "Currentsum")

ggplot(df_explorations, aes(x = protection_average_m, y = currentsum)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Currentsum vs. Maternal Overprotection Average",
       x = "Maternal Overprotection Average",
       y = "Currentsum")

## SEM ####
install.packages("lavaan")
library(lavaan)
