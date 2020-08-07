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

# Get Averages of TR 
t <- t %>% 
  group_by(NAICS) %>%
  mutate(avg_PT = mean(PT)) %>%
  mutate(avg_OJ = mean(OJ)) %>%
  mutate(avg_RW = mean(RW)) %>%
  mutate(avg_RL = mean(RL))

# derivation of gain, loss, adj_realloc, and TR
t <- t %>% 
  group_by(NAICS) %>%
  mutate(dgain = (gain - lag(gain))/lag(gain)) %>%
  mutate(dloss = (loss - lag(loss))/lag(loss)) %>%
  mutate(dadj_realloc = (adj_realloc - lag(adj_realloc))/lag(adj_realloc)) %>%
  mutate(dOJ = (OJ - lag(OJ))/lag(OJ)) %>%
  mutate(dPT = (PT - lag(PT))/lag(PT)) %>%
  mutate(dRW = (RW - lag(RW))/lag(RW)) %>%
  mutate(dRL = (RL - lag(RL))/lag(RL))

# Derivative for training indices
lmod <- lm(dRL ~ period.num, t)
summary(lmod)

lmod <- lm(dOJ ~ period.num, t)
summary(lmod)

lmod <- lm(dPT ~ period.num, t)
summary(lmod)

lmod <- lm(dRW ~ period.num, t)
summary(lmod)

# Derivative for reallocation rates

lmod <- lm(dgain ~ period.num, t)
summary(lmod)

lmod <- lm(dloss ~ period.num, t)
summary(lmod)

lmod <- lm(dadj_realloc ~ period.num, t)
summary(lmod)

# Relationship between ROC of job reallocation rate and training indices 
lmod <- lm(adj_realloc ~ dRW, t)
summary(lmod)
plot(lmod)

lmod <- lm(adj_realloc ~ dRL, t)
summary(lmod)

lmod <- lm(adj_realloc ~ dOJ, t)
summary(lmod)

lmod <- lm(adj_realloc ~ dPT, t)
summary(lmod)

ggplot(data = t) + 
  geom_point(mapping = aes(x = period.num, y = dRW, colour = factor(NAICS))) + 
  ggtitle("Rate of Change of RW Over Time")

ggplot(data = t) + 
  geom_point(mapping = aes(x = period.num, y = dadj_realloc, colour = factor(NAICS))) + 
  ggtitle("Rate of Change of Job Reallocation Rate Over Time")

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

# Quit Analysis
q <- annualized_quits() %>%
  mutate(period.num = as.numeric(substr(as.character(period),1,4)) - 2003) %>%
  spread(`Scale ID`, training_index)


# Layoffs Analysis
l <- annualized_layoffs() %>%
  mutate(period.num = as.numeric(substr(as.character(period),1,4)) - 2003) %>%
  spread(`Scale ID`, training_index)

# Other Discharges Analysis
d <- annualized_other() %>%
  mutate(period.num = as.numeric(substr(as.character(period),1,4)) - 2003) %>%
  spread(`Scale ID`, training_index)


