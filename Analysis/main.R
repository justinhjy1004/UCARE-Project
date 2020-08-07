# Utility Functions
source("Util/jolts_util.R")
source("Util/bed_util.R")

# Job Reallocation Analysis

# spread scale ID
t <- jr_training %>%
  select(-X1, -`Element Name`, -PCT_EMP) %>%
  ungroup() %>%
  spread(`Scale ID`, training_index)

# Industry Dummies
industryCategory <- factor(t$NAICS)

# Year Dummies
yearCategory <- factor(t$period)

# numericized year
t <- t %>%
  mutate(period.num = as.numeric(substr(as.character(period),1,4)) - 2003)

lmod <- lm(log(loss) ~  log(PT) + period.num*industryCategory, t)
summary(lmod)
plot(lmod)

lmod <- lm(loss ~ OJ + industryCategory, t)
summary(lmod)
plot(lmod)

lmod <- lm(loss ~ RW + industryCategory, t)
summary(lmod)
plot(lmod)

lmod <- lm(loss ~ RL + industryCategory*period.num, t)
summary(lmod)
plot(lmod)

lmod <- lm(adj_realloc ~ I(1/RW), t)
summary(lmod)

eqn_RW <- function(x){-5.311 + 97.062/x}
  
ggplot(data = t) +
  geom_point(mapping = aes(RW, adj_realloc, colour = NAICS)) +
  stat_function(fun = eqn_RW, colour = 'red', size = 1) +
  xlab('Required Level of Education') + 
  ylab('Job Reallocation Rate') + 
  ggtitle("Required Level of Education and Job Reallocation Rate") +
  theme_light()

# Worker Reallocation Analysis
p <- wr_training %>%
  select(-sep) %>%
  ungroup() %>%
  spread(`Scale ID`, training_index)

# Industry Dummies WR
industryCategoryWR <- factor(p$NAICS)

# Year Dummies WR
yearCategoryWR <- factor(p$period)

# numericized year
p <- p %>%
  mutate(period.num = as.numeric(substr(as.character(period),1,4)) - 2003)

lmod <- lm(realloc_rate ~ I(1/OJ^5) + period.num*industryCategoryWR, p)
summary(lmod)
plot(lmod)

lmod <- lm(realloc_rate ~ I(1/PT^5) + period.num*industryCategoryWR, p)
summary(lmod)
plot(lmod)

lmod <- lm(realloc_rate ~ I(1/RW^5) + period.num*industryCategoryWR, p)
summary(lmod)
plot(lmod)

lmod <- lm(realloc_rate ~ I(1/RL^5) + period.num*industryCategoryWR, p)
summary(lmod)
plot(lmod)

ggplot(data = p) +
  # geom_point(mapping = aes(RL, realloc_rate, colour = period.num)) +
  geom_point(mapping = aes(period, , colour = NAICS))
  xlab('Required Level of Education') + 
  ylab('Job Reallocation Rate') + 
  ggtitle("Required Level of Education and Job Reallocation Rate") +
  theme_light()

# Quits Analysis
q <- annualized_quits() %>%
  mutate(period.num = as.numeric(substr(as.character(period),1,4)) - 2003) %>%
  spread(`Scale ID`, training_index)

lmod <- lm(quits ~ I(1/PT^5) + industryCategoryWR*period.num, q)
summary(lmod)
plot(lmod)

lmod <- lm(quits ~ I(1/OJ^5) + industryCategoryWR*period.num, q)
summary(lmod)
plot(lmod)

lmod <- lm(quits ~ I(1/RW^5) + industryCategoryWR*period.num, q)
summary(lmod)
plot(lmod)

lmod <- lm(quits ~ RL + industryCategoryWR*period.num, q)
summary(lmod)
plot(lmod)

ggplot(data = q) +
  geom_point(mapping = aes(period, quits, colour = industry))

# Layoffs Analysis
l <- annualized_layoffs() %>%
  mutate(period.num = as.numeric(substr(as.character(period),1,4)) - 2003) %>%
  spread(`Scale ID`, training_index)

lmod <- lm(layoffs ~ I(1/PT^3) + industryCategoryWR*period.num, l)
summary(lmod)
plot(lmod)

lmod <- lm(layoffs ~ I(1/OJ^3) + industryCategoryWR*period.num, l)
summary(lmod)
plot(lmod)

lmod <- lm(layoffs ~ I(1/RW^3) + industryCategoryWR*period.num , l)
summary(lmod)
plot(lmod)

lmod <- lm(log(layoffs) ~ log(RL) + industryCategoryWR*period.num, l)
summary(lmod)
plot(lmod)

# Other Discharges Analysis
d <- annualized_other() %>%
  mutate(period.num = as.numeric(substr(as.character(period),1,4)) - 2003) %>%
  spread(`Scale ID`, training_index)

lmod <- lm(log(other) ~ log(PT) + industryCategoryWR*period.num, d)
summary(lmod)
plot(lmod)

lmod <- lm(log(other) ~ log(PT) + industryCategoryWR*period.num, d)
summary(lmod)
plot(lmod)

lmod <- lm(log(other) ~ log(PT) + industryCategoryWR*period.num, d)
summary(lmod)
plot(lmod)

lmod <- lm(log(layoffs) ~ log(RL) + industryCategoryWR*period.num, l)
summary(lmod)
plot(lmod)

lmod <- lm(log(adj_realloc) ~  log(PT) + industryCategory*period.num, t)
summary(lmod)
