library(ggplot2)
library(tidyverse)
library(dplyr)
library(scales)
library(tidyr)

df = read.csv("/home/sfczekalski/Documents/ZED/project/online_shoppers/online_shoppers_intention.csv")
head(df, 5)

# Categorical variables
df$OperatingSystems <- factor(df$OperatingSystems)
df$Browser <- factor(df$Browser)
df$Region <- factor(df$Region)
df$TrafficType <- factor(df$TrafficType)
df$VisitorType <- factor(df$VisitorType)
df$Month <- factor(df$Month, levels = c("Feb", "Mar", "May", "June", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

any(is.na(df))
summary(df)

# Class count
ggplot(data = df) + 
  geom_bar(mapping = aes(x = Revenue, fill = Revenue)) +
  ylab("Class count")

# Class proportion
ggplot(data = df, aes(x = Revenue, y = (..count..)/sum(..count..))) + 
  geom_bar(mapping = aes(fill = Revenue)) +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..))), stat= "count", vjust = -.5) +
  scale_y_continuous(labels=scales::percent) +
  ylab("Relative class frequencies")

df = mutate(df, Sum_Duration = Administrative_Duration + Informational_Duration + ProductRelated_Duration)

head(df, 5)
df %>%
  filter(Sum_Duration > 0) %>%
  ggplot(mapping = aes(x = Sum_Duration, color = Revenue)) +
  geom_density()

# Histograms of types of visit counts
df %>%
  filter(Administrative > 0) %>%
  ggplot(mapping = aes(x = Administrative, color = Revenue)) + 
  geom_freqpoly(binwidth = 1)

df %>%
  filter(Informational > 0) %>%
  ggplot(mapping = aes(x = Informational, color = Revenue)) + 
  geom_freqpoly(binwidth = 1)

df %>%
  filter(ProductRelated > 0) %>%
  ggplot(mapping = aes(x = ProductRelated, color = Revenue)) + 
  geom_freqpoly(binwidth = 1)


# Bounce rates histogram, depending on class
ggplot(data = df, mapping = aes(x = BounceRates, fill = Revenue)) + 
  geom_histogram()

# Exit rates histogram, depending on class
ggplot(data = df, mapping = aes(x = ExitRates, fill = Revenue)) + 
  geom_histogram()

df = mutate(df, Rate = 100 * BounceRates * ExitRates)
df %>%
  filter(Rate > 1) %>%
  ggplot(data = df, mapping = aes(x = Rate, color = Revenue)) + 
  geom_density()

df %>%
  filter(PageValues > 0) %>%
  ggplot(mapping = aes(x = PageValues, fill = Revenue)) + 
    geom_histogram()

# Class proportion depending on month
ggplot(data = df, group=Month) + 
  geom_histogram() +
  ylab("Relative class frequencies")


# Class proportion depending on month
ggplot(data = df, aes(x = Revenue, y = (..count..)/sum(..count..)), group = Month) + 
  geom_bar(mapping = aes(fill = Revenue)) +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..))), stat= "count", vjust = -.5) +
  scale_y_continuous(labels=scales::percent) +
  facet_wrap(~Month) +
  ylab("Relative class frequencies")

# Operating system
ggplot(data = df) + 
  geom_bar(mapping = aes(x = OperatingSystems, fill = OperatingSystems)) +
  ylab("Count")

df %>%
  filter(OperatingSystems == 1:4) %>%
  ggplot() + 
  geom_bar(mapping = aes(x = Revenue, fill = Revenue)) +
  facet_wrap(~OperatingSystems) +
  ylab("Class count depending on OS")

df %>%
  filter(OperatingSystems == 5:8) %>%
  ggplot() + 
  geom_bar(mapping = aes(x = Revenue, fill = Revenue)) +
  facet_wrap(~OperatingSystems) +
  ylab("Class count depending on OS")

# Browser
ggplot(data = df) + 
  geom_bar(mapping = aes(x = Browser, fill = Browser)) +
  ylab("Count")

# Class count depending on browser
df %>%
  filter(Browser == 1 | Browser == 2) %>%
  ggplot() + 
    geom_bar(mapping = aes(x = Revenue, fill = Revenue)) +
    facet_wrap(~Browser) +
    ylab("Class count depending on browser")

df %>%
  filter(Browser == 3:13) %>%
  ggplot() + 
  geom_bar(mapping = aes(x = Revenue, fill = Revenue)) +
  facet_wrap(~Browser) +
  ylab("Class count depending on browser")

# Region
ggplot(data = df) + 
  geom_bar(mapping = aes(x = Region, fill = Region)) +
  ylab("Count")

# Class count in each region
ggplot(data = df) + 
  geom_bar(mapping = aes(x = Revenue, fill = Revenue)) +
  facet_wrap(~Region) +
  ylab("Class count in each region")

# Traffic type
ggplot(data = df) + 
  geom_bar(mapping = aes(x = TrafficType, fill = TrafficType)) +
  ylab("Count")

# Class depending on traffic type
ggplot(data = df) + 
  geom_bar(mapping = aes(x = Revenue, fill = Revenue)) +
  facet_wrap(~TrafficType, nrow = 4) +
  ylab("Class proportion")

# What about some traffic types? Maybe they describe user that are no customers
df %>%
  filter(TrafficType==7 | TrafficType==9 | TrafficType==11 | TrafficType==13 | TrafficType == 19) %>%
  ggplot() +
    geom_bar(mapping = aes(x = Revenue, fill = Revenue)) +
    facet_wrap(~TrafficType, nrow = 4) +
    ylab("Class proportion")

df %>%
  filter(TrafficType==14 | TrafficType==15 | TrafficType==16 | TrafficType==17 | TrafficType == 18) %>%
  ggplot() +
  geom_bar(mapping = aes(x = Revenue, fill = Revenue)) +
  facet_wrap(~TrafficType, nrow = 4) +
  ylab("Class proportion")

# Visitor type
ggplot(data = df) + 
  geom_bar(mapping = aes(x = VisitorType, fill = VisitorType)) +
  ylab("Count")

# Class depending on visitor type
ggplot(data = df) + 
  geom_bar(mapping = aes(x = Revenue, fill = Revenue)) +
  facet_grid(~VisitorType) +
  ylab("Class proportion")


# Weekend / no weekend count
ggplot(data = df) + 
  geom_bar(mapping = aes(x = Weekend, fill = Weekend)) +
  ylab("Weekend count")

# Class depending on weekend / no weekend
ggplot(data = df) + 
  geom_bar(mapping = aes(x = Revenue, fill = Revenue)) +
  facet_grid(~Weekend) +
  ylab("Class proportion")

# Correlogram
library(ggcorrplot)

numeric_var <- c("Sum_Duration", "Administrative_Duration", "Informational_Duration", "ProductRelated_Duration", "BounceRates", "ExitRates")
corr <- cor(df[numeric_var])
head(corr[, 1:6])
ggcorrplot(corr, type = "lower", outline.color = "white")
