#install.packages("rlang")
#install.packages("tidyverse")
#install.packages("ggpubr")
#install.packages("ggsignif")
library(rlang)
library(tidyverse)
library(readxl)
library(rstatix)
library(ggsignif)
library(ggpubr)


Height_Data_Raw <- 
  read_excel("Oak Data Entry MCRS Field Trip.xlsx", 
             sheet = "Seedling Heights")

SPAD_Data_Raw <- 
  read_excel("Oak Data Entry MCRS Field Trip.xlsx", 
             sheet = "SPAD", 
             na = "NA")



SPAD_Data_Columns_Clean <- SPAD_Data_Raw |>
  select(
    Date, Tag_Num, Treatment, Block, QAQC_Dead_Alive,QAQC_Leaf1, QAQC_Leaf1, QAQC_Leaf2, QAQC_Leaf3, QAQC_Leaf4, QAQC_Leaf5, QAQC_Leaf6, QAQC_Num_leaves
  ) |>
  rename_with(
    ~ str_remove(., "QAQC_")
  ) |>
  filter(Dead_Alive == "Alive") |>
  separate_wider_regex(
    cols = Tag_Num,
    patterns = c(
      Tag_Number = "^[0-9]+",
      Sapling_AB     = ".*"      
    )
  ) |>
  mutate(Avg_Leaf_SPAD = rowMeans(across(c(Leaf1, Leaf2, Leaf3, Leaf4, Leaf5, Leaf6)), na.rm = TRUE)
  ) |>
  mutate(Date_Factor = as.factor(as_date(Date))
  )

nrow(SPAD_Data_Columns_Clean) 
glimpse(SPAD_Data_Columns_Clean)

view(SPAD_Data_Columns_Clean)

Height_Data_Columns_Clean <- Height_Data_Raw |>
  select(Date, Tag_Num, Treatment, Block, Sapling_AB_etc, QAQC_Dead_Alive, QAQC_Seedling_Ht_cm) |>
  rename_with(~ str_remove(., "QAQC_")) |>
  rename(Sapling_AB = Sapling_AB_etc, Tag_Number = Tag_Num) |>
  filter(Dead_Alive == "Alive") |>
  mutate(Date_Factor = as.factor(as_date(Date))
  )

Height_Data_Columns_Clean$Treatment[Height_Data_Columns_Clean$Treatment == "OA"] <- "OS"

nrow(Height_Data_Columns_Clean)
glimpse(Height_Data_Columns_Clean)

view(Height_Data_Columns_Clean)

SPAD_Height <- inner_join(SPAD_Data_Columns_Clean, Height_Data_Columns_Clean) 

view(SPAD_Height)

SPAD_Data_Columns_Clean |>
  ggplot() +
  aes(x = Treatment, y = Avg_Leaf_SPAD, fill = Date_Factor) +
  facet_wrap(~Block) +
  geom_boxplot()

linear_modelSPAD <- lm(Avg_Leaf_SPAD ~ Treatment * Date_Factor + Block, data = SPAD_Data_Columns_Clean)

plot(linear_modelSPAD)

plot(linear_modelSPAD, which = 2)

#doesn't look great, but will continue further tests 

#add block factor 



SPAD_Data_Columns_Clean |> rstatix::levene_test(Avg_Leaf_SPAD ~ Treatment * Date_Factor)

# p-value > 0.05 so it passes the test


model_SPAD_aov <- aov(Avg_Leaf_SPAD ~ Treatment * Date_Factor + Block, SPAD_Data_Columns_Clean)

summary(model_SPAD_aov)


TukeyHSD(model_SPAD_aov)


SPAD_summary <- SPAD_Data_Columns_Clean |> 
  group_by(Date_Factor, Treatment) |> 
  summarise(
    mean = mean(Avg_Leaf_SPAD), 
    se = sd(Avg_Leaf_SPAD)/ sqrt(length(Avg_Leaf_SPAD)))
SPAD_summary

ggplot(SPAD_summary) + 
  aes(x = Date_Factor, y = mean, fill = Treatment) + 
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), position = position_dodge(0.9), width = 0.2) +
 
  labs(y = "Average SPAD", fill = "Treatment") +
  theme_classic()

ggplot(SPAD_Height) +
  aes(x = Avg_Leaf_SPAD, y = Seedling_Ht_cm) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

hist(SPAD_Height$Avg_Leaf_SPAD)
hist(SPAD_Height$Seedling_Ht_cm)

#does not look to be a linear relationship




SPAD.lm <- lm(Seedling_Ht_cm ~ Avg_Leaf_SPAD, data = SPAD_Height)
plot(SPAD.lm)
plot(SPAD.lm, which = 1)

#is not homoscedastic

ggplot(SPAD_Height) +
  aes(x = log(Avg_Leaf_SPAD), y = Seedling_Ht_cm) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

ggplot(SPAD_Height) +
  aes(x = sqrt(Avg_Leaf_SPAD), y = Seedling_Ht_cm) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# looks as though there is no relationship, could add treatment as a categorical variable but the sample size is not large enough with the combined data that we have

SPAD_H.lm <- lm(Seedling_Ht_cm ~ Avg_Leaf_SPAD, data = SPAD_Height)
summary(SPAD_H.lm)

SPAD_H_log.lm <- lm(Seedling_Ht_cm ~ log(Avg_Leaf_SPAD), data = SPAD_Height)
summary(SPAD_H_log.lm)

SPAD_H_sqrt.lm <- lm(Seedling_Ht_cm ~ sqrt(Avg_Leaf_SPAD), data = SPAD_Height)
summary(SPAD_H_sqrt.lm)

#R-squared value is negative

#try a parabolic relationship, r-squared is lower

SPAD_H_I.lm <- lm(Seedling_Ht_cm ~ Avg_Leaf_SPAD + I(Avg_Leaf_SPAD^2), data = SPAD_Height)
summary(SPAD_H_I.lm)


write.csv(SPAD_Data_Columns_Clean, "ANOVAData", row.names = FALSE)
write.csv(SPAD_Height, "CombinedRegressionData", row.names = FALSE)

