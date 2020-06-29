# Utility Functions
source("Util/jolts_util.R")
source("Util/bed_util.R")

# Job Reallocation Analysis

# spread scale ID
jr_training <- jr_training %>%
  select(-X1, -`Element Name`, -PCT_EMP) %>%
  ungroup() %>%
  spread(`Scale ID`, training_index)

# Industry Dummies
industryCategory <- factor(jr_training$NAICS)

# Year Dummies
yearCategory <- factor(jr_training$period)

# numericized year
t <- jr_training %>%
  mutate(period.num = as.numeric(substr(as.character(period),1,4)) - 2003)

lmod <- lm(adj_realloc ~  PT + industryCategory*period.num, t)
summary(lmod)
plot(lmod)

lmod <- lm(adj_realloc ~ OJ + industryCategory*period.num, t)
summary(lmod)
plot(lmod)

lmod <- lm(adj_realloc ~ RW + period.num + industryCategory + industryCategory*period.num, t)
summary(lmod)
plot(lmod)

lmod <- lm(adj_realloc ~ I(1/RL) + period.num + industryCategory + industryCategory*period.num, t)
summary(lmod)
plot(lmod)

ggplot(data = t) +
  geom_point(mapping = aes(OJ, adj_realloc, )

# Worker Reallocation Analysis
p <- wr_training %>%
  select(-sep, -hires) %>%
  ungroup() %>%
  spread(`Scale ID`, training_index)

# Industry Dummies WR
industryCategoryWR <- factor(p$NAICS)

# Year Dummies WR
yearCategoryWR <- factor(p$period)

# numericized year
p <- p %>%
  mutate(period.num = as.numeric(substr(as.character(period),1,4)) - 2003)

lmod <- lm(realloc_rate ~ PT + industryCategoryWR*period.num, p)
summary(lmod)
plot(lmod)

lmod <- lm(realloc_rate ~ OJ + industryCategoryWR*period.num, p)
summary(lmod)
plot(lmod)

lmod <- lm(realloc_rate ~ RW + industryCategoryWR*period.num, p)
summary(lmod)
plot(lmod)

lmod <- lm(realloc_rate ~ RL + industryCategoryWR*period.num, p)
summary(lmod)
plot(lmod)


# Quits Analysis
q <- annualized_quits() %>%
  mutate(period.num = as.numeric(substr(as.character(period),1,4)) - 2003) %>%
  spread(`Scale ID`, training_index)

lmod <- lm(quits ~ I(1/PT) + industryCategoryWR*period.num, q)
summary(lmod)
plot(lmod)

lmod <- lm(quits ~ I(1/OJ) + industryCategoryWR*period.num, q)
summary(lmod)
plot(lmod)

lmod <- lm(quits ~ I(1/RW^2) + industryCategoryWR, q)
summary(lmod)
plot(lmod)

lmod <- lm(quits ~ I(1/RL^2) , q)
summary(lmod)
plot(lmod)

ggplot(data = q) +
  geom_point(mapping = aes(period, quits, colour = industry))

# Layoffs Analysis
l <- annualized_layoffs() %>%
  mutate(period.num = as.numeric(substr(as.character(period),1,4)) - 2003) %>%
  spread(`Scale ID`, training_index)

lmod <- lm(layoffs ~ industryCategoryWR*period.num, l)
summary(lmod)
plot(lmod)

lmod <- lm(layoffs ~ OJ + industryCategoryWR*period.num, l)
summary(lmod)
plot(lmod)

lmod <- lm(layoffs ~ RW , l)
summary(lmod)
plot(lmod)

lmod <- lm(layoffs ~ RL , l)
summary(lmod)
plot(lmod)

ggplot(data = l) +
  geom_point(mapping = aes(RW, layoffs, colour = industry))

