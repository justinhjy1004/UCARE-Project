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
  mutate(period.num = as.numeric(substr(as.character(period),1,4)))

lmod <- lm(adj_realloc ~ PT, t)
summary(lmod)
plot(lmod)

lmod <- lm(adj_realloc ~ PT + industryCategory, t)
summary(lmod)
plot(lmod)

lmod <- lm(adj_realloc ~ OJ + period.num, t)
summary(lmod)
plot(lmod)

lmod <- lm(adj_realloc ~ OJ + period.num + industryCategory, t)
summary(lmod)
plot(lmod)

# Worker Reallocation Analysis

#Scale
scale <- 'RL'
p <- wr_training %>%
  select(-sep, -hires) %>%
  ungroup() %>%
  spread(`Scale ID`, training_index)

# Industry Dummies WR
industryCategoryWR <- factor(p$NAICS)

# Year Dummies WR
yearCategoryWR <- factor(p$period)


lmod <- lm(realloc_rate ~ PT, p)
summary(lmod)
plot(lmod)

lmod <- lm(realloc_rate ~ PT + industryCategoryWR, p)
summary(lmod)
plot(lmod)

lmod <- lm(realloc_rate ~ PT + yearCategoryWR, p)
summary(lmod)
plot(lmod)

lmod <- lm(realloc_rate ~ PT + yearCategoryWR + industryCategoryWR, p)
summary(lmod)
plot(lmod)


ggplot(data = p) +
  geom_point(mapping = aes(x = OJ, y = realloc_rate, colour = NAICS)) 

  stat_function(fun = eqPT, col = 'red', size = 1) +
  theme_minimal() +
  xlab("On-Site/In-Plant Training") +
  ylab("Job Reallocation Rate") +
  ggtitle("On-Site/In-Plant and Job Reallocation Rate")

eqRL <- function(x){-50.525 + (581.981/(x+6.2)) - (5.690/(x-6.2))}
eqRW <- function(x){-36.3646 + (436.4665/(x+3.34)) + (0.9135/(x-3.34))}

eqOJ <- function(x){-6.464 + (246.194/(x^2))}
eqPT <- function(x){-10.041 + (235.654/(x^2))}


wr_training <- wr_training %>%
  spread(`Scale ID`, training_index)

wr_training

quits <- jolts[jolts$data_element == 'Quits' & 
               jolts$rate_level == 'Rate',]

quits <- quits %>%
  select(period, value, industry, NAICS) %>%
  rename(quits = value)

quits$NAICS <- as.numeric(quits$NAICS)
q <- inner_join(p, quits, by = c("period", 'industry', 'NAICS'))

lmod <- lm(quits ~ PT, q)
summary(lmod)
plot(lmod)

lmod <- lm(quits ~ PT + industryCategoryWR, q)
summary(lmod)
plot(lmod)

lmod <- lm(quits ~ PT + yearCategoryWR, q)
summary(lmod)
plot(lmod)

lmod <- lm(quits ~ PT + yearCategoryWR + industryCategoryWR, q)
summary(lmod)
plot(lmod)

ggplot(data = q[q$NAICS == 42,]) +
  geom_point(mapping = aes(x = RL, y = quits), colour = 'blue')


