library(ggplot2)
library(dplyr)
library(tidyr)
library(lmodel2)
library(smatr)
library(car)
library(ggpubr)
library(survminer)
library(gridExtra)
library(grid)
library(agricolae)
library(multcomp)
##### Figure 2: Growth curves of larvae #####
df_1 <- read.csv(file.choose())

meal_colors <- c("CF" = "#1f77b4", "FW" = "#ff7f0e", "OKA" = "#2ca02c", "BG" = "#d62728","BB"="black")

df_1_summary <- df_1 %>%
  group_by(Meal, day) %>%
  summarise(
    mean_lw = mean(lw),
    se_lw = sd(lw) / sqrt(n()),
    .groups = 'drop'
  )

# Plot growth curves with error bars
ggplot(df_1_summary, aes(x = day, y = mean_lw, color = Meal, group = Meal)) +
  geom_line(size = 1) +  
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin = mean_lw - se_lw, ymax = mean_lw + se_lw), width = 0.2) +  
  labs(x = "Day", y = "Individual Larval Weight (mg)") +
  scale_color_manual(values = meal_colors) +  
  scale_x_continuous(breaks = seq(0, max(df_1_summary$day), by = 2), limits = c(0, NA)) + 
  scale_y_continuous(limits = c(0, NA)) +  
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.title = element_blank(),
    legend.position = c(0.05, 0.95),  
    legend.justification = c("left", "top")  
  )


#####Figure 3: Adult body weight bar chart##### 
df_2 <- read.csv(file.choose())# Load the CSV file "adult weight.csv" into df_2
attach(df_2)

# QQ Plot for female body weight
qqPlot(lm(f_bw ~ meal, data.frame = df_2), simulate = TRUE, 
       main = "QQ Plot", labels = FALSE) 

# Bartlett's test for female body weight
bartlett.test(f_bw  ~ meal, data = df_2)

# QQ Plot for male body weight
qqPlot(lm(m_bw ~ meal, data.frame = df_2), simulate = TRUE, 
       main = "QQ Plot", labels = FALSE) 

# Bartlett's test for male body weight
bartlett.test(m_bw  ~ meal, data = df_2)

# Pivot data for analysis
df_2_long <- df_2 %>%
  pivot_longer(cols = c(f_bw, m_bw), names_to = "gender", values_to = "bw")

# Function to perform ANOVA and Tukey HSD for each gender
get_significance_letters <- function(data, gender_label) {
  data_gender <- data %>% filter(gender == gender_label)
  
  # ANOVA
  anova_result <- aov(bw ~ meal, data = data_gender)
  
  # Tukey HSD
  tukey_result <- HSD.test(anova_result, "meal", group = TRUE)
  
  # Return significance letters with proper column names
  return(tibble(meal = rownames(tukey_result$groups), 
                sig_letter = tukey_result$groups$groups,
                gender = gender_label))
}
# Get significance letters separately for female and male
female_letters <- get_significance_letters(df_2_long, "f_bw") %>%
  mutate(sig_letter = toupper(sig_letter)) # Capital letters for females
male_letters <- get_significance_letters(df_2_long, "m_bw") %>%
  mutate(sig_letter = tolower(sig_letter)) # Lowercase letters for males
significance_labels <- bind_rows(female_letters, male_letters)

df_2_summary <- df_2_long %>%
  group_by(meal, gender) %>%
  summarise(
    mean_bw = mean(bw, na.rm = TRUE),
    se_bw = sd(bw, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  left_join(significance_labels, by = c("meal", "gender")) %>%
  mutate(meal = factor(meal, levels = c("CF", "FW", "OKA", "BG"))) 

# Plot with significance letters and extended y-axis
ggplot(df_2_summary, aes(x = meal, y = mean_bw, fill = gender)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7, color = "black") +
  geom_errorbar(aes(ymin = mean_bw - se_bw, ymax = mean_bw + se_bw), 
                position = position_dodge(width = 0.8), width = 0.25) +
  labs(x = "Meal Type", y = "Adult Body Weight (mg)") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) + 
  scale_fill_manual(values = c("black", "white"), labels = c("Female", "Male")) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    axis.line = element_line(color = "black"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.title = element_blank(),
    legend.position = c(0.85, 0.85)
  ) +
  geom_text(aes(label = sig_letter, y = mean_bw + se_bw + 1), 
            position = position_dodge(width = 0.8), vjust = 0, size = 5)



####Figure 4: egg number and hatching rate#####
df_3 <- read.csv(file.choose())  ## Choose file "egg and hatch.csv"

# Prepare the data for plotting
df_3_long <- df_3 %>%
  pivot_longer(cols = c(egg_num, hatched_num), names_to = "egg_type", values_to = "egg_count") %>%
  group_by(meal, day, egg_type) %>%
  summarise(
    mean_egg_count = mean(egg_count, na.rm = TRUE),
    se_egg_count = sd(egg_count, na.rm = TRUE) / sqrt(n()),
    .groups = 'drop'
  ) %>%
  filter(mean_egg_count > 0)

# Define the colors for different meals
meal_colors <- c("CF" = "#1f77b4", "FW" = "#ff7f0e", "OKA" = "#2ca02c", "BG" = "#d62728")

# Define line types based on egg type
line_types <- c("egg_num" = "solid", "hatched_num" = "dashed")

# Plotting
ggplot(df_3_long, aes(x = day, y = mean_egg_count, color = meal, linetype = egg_type, group = interaction(meal, egg_type))) +
  geom_line(size = 1) +
  geom_errorbar(aes(ymin = mean_egg_count - se_egg_count, ymax = mean_egg_count + se_egg_count), 
                width = 0.2, position = position_dodge(0.1)) +
  labs(x = "Sampling Day", y = "Egg Number") +
  scale_x_continuous(breaks = seq(2, max(df_3$day), by = 2)) +
  scale_color_manual(values = meal_colors) +
  scale_linetype_manual(values = line_types, labels = c("Total Egg", "Hatched Egg")) +
  theme_minimal(base_size = 14) +
  theme(
    legend.title = element_blank(),
    legend.position = c(0.85, 0.85),
    legend.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )

####Table 2 analysis######
df_4<-read.csv(file.choose()) ##Choose file "egg and hatch.csv"
attach(df_4)
### 1) prepupal weight
qqPlot(lm(ppw ~ meal, data.frame = df_4), simulate = TRUE, 
       main = "QQ Plot", labels = FALSE) 
bartlett.test(ppw  ~ meal, data = df_4) 
fit <- aov(ppw~ meal) # equal variance satisfied
summary(fit)
TukeyHSD(fit)
sig <- LSD.test(fit,"meal",p.adj="bonferroni") 
sig

### 2) total egg weight at Day 5
qqPlot(lm(ew_d5 ~ meal, data.frame = df_4), simulate = TRUE, 
       main = "QQ Plot", labels = FALSE) 
bartlett.test(ew_d5  ~ meal, data = df_4) ### unequal variance, use kruskal test instead
fit <- aov(ew_d5~ meal) # equal variance satisfied
summary(fit)
TukeyHSD(fit)
sig <- LSD.test(fit,"meal",p.adj="bonferroni") 
sig

### 3) total egg weight at Day 10
qqPlot(lm(ew_d10 ~ meal, data.frame = df_4), simulate = TRUE, 
       main = "QQ Plot", labels = FALSE) 
bartlett.test(ew_d10  ~ meal, data = df_4) ### unequal variance, use kruskal test instead
fit <- aov(ew_d10~ meal) # equal variance satisfied
summary(fit)
TukeyHSD(fit)
sig <- LSD.test(fit,"meal",p.adj="bonferroni") 
sig

### 4) total egg weight at Day 10
qqPlot(lm(SDI_1 ~ meal, data.frame = df_4), simulate = TRUE, 
       main = "QQ Plot", labels = FALSE) 
bartlett.test(SDI_1  ~ meal, data = df_4) ### unequal variance, use kruskal test instead
fit <- aov(SDI_1~ meal) # equal variance satisfied
summary(fit)
TukeyHSD(fit)
sig <- LSD.test(fit,"meal",p.adj="bonferroni") 
sig
### 5) total egg weight at Day 10
qqPlot(lm(SDI_2 ~ meal, data.frame = df_4), simulate = TRUE, 
       main = "QQ Plot", labels = FALSE) 
bartlett.test(SDI_2  ~ meal, data = df_4) ### unequal variance, use kruskal test instead
fit <- aov(SDI_2~ meal) # equal variance satisfied
summary(fit)
TukeyHSD(fit)
sig <- LSD.test(fit,"meal",p.adj="bonferroni") 
sig