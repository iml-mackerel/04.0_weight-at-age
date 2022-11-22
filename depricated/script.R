#Required library
library(dplyr)
library(ggplot2)
library(cowplot)
library(tidyverse)

#read commercial bio data imported from Oracle
mack_bio<-read.csv("C:/Users/BoudreauMA/Desktop/Maquereau/Data/bio.csv", header = TRUE, sep=",")
unique(mack_bio$year)
unique(mack_bio$month)
unique(mack_bio$gear.full)

# Visualize age and weight to find outliers
plot(mack_bio$agef)  #outliers > 30 years old
plot(mack_bio$weight)  #No outliers but a couple of 0

# Remove outlier for age > 30 and weight = 0 and NA
mack_bio<-mack_bio[!is.na(mack_bio$weight),]
mack_bio<-subset(mack_bio, weight > 0)
data.temp<-subset(mack_bio, agef < 30 & agef  > 0)

#Check if there is no NA and outliers in agef and weight
unique(data.temp$agef)
summary(data.temp$weight)

#Create age.group 1 to 10+
data.temp$age.group <- ifelse(data.temp$agef < 10, data.temp$agef, 10)
unique(data.temp$age.group)


#### Scenario 1 ####
# Subset of June/July, 4T and gillnets
data.S1 <- data.temp %>% filter(month %in% c(6,7), nafo %in% c("4T"), gear %in% c("GND", "GNS", "GN"))
unique(data.S1$month)
unique(data.S1$nafo)
unique(data.S1$gear)

#For each age.group in S1, estimate mean weight, sd weight, IC.low and IC.up, nb of sample and nb of fish
Average.data.S1 = NULL

for (i in unique(data.S1$age.group)) {
  
  df1 <- subset(data.S1, age.group == i)
  
  year.data = NULL
  age = unique(df1$age.group)
  
  for (j in unique(df1$year)) {
    
    df2 <- subset(df1, year == j)
    
    mean.value <- mean(df2$weight * 1000, na.rm=TRUE)
    
    sd.value <- sd(df2$weight * 1000, na.rm = TRUE)
    
    IC.low <- mean.value - (1.96*sd.value)
    
    IC.up <- mean.value + (1.96*sd.value)
    
    nb.fish <- as.numeric(nrow(df2))
    
    data.sample <- df2 %>% group_by(sample.id) %>%
      summarize(n.tot = n())
    
    nb.sample <- as.numeric(nrow(data.sample))
    
    Year <- as.numeric(unique(df2$year))
    
    df3 <- data.frame(Year,mean.value,IC.low, IC.up, nb.fish, nb.sample, age)
    
    year.data <- rbind(year.data, df3)
    
  }
  
  Average.data.S1 <- rbind(Average.data.S1, year.data)

}

# Multi-line plot of the average weight for age 1 to 10 based on Scenario 1 (June-July, 4T and Gillnets)
S1<-ggplot(Average.data.S1, aes(x=Year, y=mean.value, col=as.factor(age))) + 
  geom_line(size=1) + 
  xlab("Year") +
  ylab("Average weight (g)") +
  labs(col="Age") +
  ylim(50,850) +
  ggtitle("4T + gillnets") +
  theme_minimal(base_size = 14)
S1  

#### Scenario 2 ####
# Subset of June/July, 4T, 4W, 4V, 4X and gillnets
data.S2 <- data.temp %>% filter(month %in% c(6,7), nafo %in% c("4T", "4V", "4W", "4X"), gear %in% c("GND", "GNS", "GN"))
unique(data.S2$month)
unique(data.S2$nafo)
unique(data.S2$gear)

#For each age.group in S2, estimate mean weight, sd weight, IC.low and IC.up, nb of sample and nb of fish
Average.data.S2 = NULL

for (i in unique(data.S2$age.group)) {
  
  df1 <- subset(data.S2, age.group == i)
  
  year.data = NULL
  age = unique(df1$age.group)
  
  for (j in unique(df1$year)) {
    
    df2 <- subset(df1, year == j)
    
    mean.value <- mean(df2$weight * 1000, na.rm=TRUE)
    
    sd.value <- sd(df2$weight * 1000, na.rm = TRUE)
    
    IC.low <- mean.value - (1.96*sd.value)
    
    IC.up <- mean.value + (1.96*sd.value)
    
    nb.fish <- as.numeric(nrow(df2))
    
    data.sample <- df2 %>% group_by(sample.id) %>%
      summarize(n.tot = n())
    
    nb.sample <- as.numeric(nrow(data.sample))
    
    Year <- as.numeric(unique(df2$year))
    
    df3 <- data.frame(Year,mean.value,IC.low, IC.up, nb.fish, nb.sample, age)
    
    year.data <- rbind(year.data, df3)
    
  }
  
  Average.data.S2 <- rbind(Average.data.S2, year.data)
  
}

# Multi-line plot of the average weight for age 1 to 10 based on Scenario 2 (June-July, 4TVWX and Gillnets)
S2<-ggplot(Average.data.S2, aes(x=Year, y=mean.value, col=as.factor(age))) + 
  geom_line(size=1) + 
  xlab("Year") +
  ylab("Average weight (g)") +
  labs(col="Age") +
  ylim(50,850) +
  ggtitle("4TVWX + gillnets") +
  theme_minimal(base_size = 14)

S2

#### Scenario 3 ####
# Subset of June/July, 4T, and gillnets, hooks and lines 
data.S3 <- data.temp %>% filter(month %in% c(6:7), nafo %in% c("4T"), gear %in% c("GND", "GNS", "GN", "LX"))
unique(data.S3$month)
unique(data.S3$nafo)
unique(data.S3$gear)

#Remove year-age group that contain less than 5 fish 
data.S3.temp<- data.S3 %>% group_by(year, age.group) %>% 
  summarize(nb.fish = n()) %>%
  filter(nb.fish < 5)

data.S3 <- anti_join(data.S3, data.S3.temp, by = c("year","age.group"))

Average.data.S3 = NULL

for (i in unique(data.S3$age.group)) {
  
  df1 <- subset(data.S3, age.group == i)
  
  year.data = NULL
  age = unique(df1$age.group)
  
  for (j in unique(df1$year)) {
    
    df2 <- subset(df1, year == j)
    
    mean.value <- mean(df2$weight * 1000, na.rm=TRUE)
    
    sd.value <- sd(df2$weight * 1000, na.rm = TRUE)
    
    IC.low <- mean.value - (1.96*sd.value)
    
    IC.up <- mean.value + (1.96*sd.value)
    
    nb.fish <- as.numeric(nrow(df2))
    
    data.sample <- df2 %>% group_by(sample.id) %>%
      summarize(n.tot = n())
    
    nb.sample <- as.numeric(nrow(data.sample))
    
    Year <- as.numeric(unique(df2$year))
    
    df3 <- data.frame(Year,mean.value,IC.low, IC.up, nb.fish, nb.sample, age)
    
    year.data <- rbind(year.data, df3)
    
  }
  
  Average.data.S3 <- rbind(Average.data.S3, year.data)
  
}


S3<-ggplot(Average.data.S3, aes(x=Year, y=mean.value, col=as.factor(age))) + 
  geom_line(size=1) + 
  xlab("Year") +
  ylab("Average weight (g)") +
  labs(col="Age") +
  ylim(50,850) +
  ggtitle("4T + gillnets and hooks") +
  theme_minimal(base_size = 14)



#### Scenario 4 ####
# Subset of June/July, 4T, 4V, 4W, 4X and gillnets, hooks and lines 
data.S4 <- data.temp %>% filter(month %in% c(6:7), nafo %in% c("4T", "4W", "4V", "4X"), gear %in% c("GND", "GNS", "GN", "LX"))
unique(data.S4$month)
unique(data.S4$nafo)
unique(data.S4$gear)

#Remove year-age group that contain less than 5 fish 
data.S4.temp<- data.S4 %>% group_by(year, age.group) %>% 
  summarize(nb.fish = n()) %>%
  filter(nb.fish < 5)

data.S4 <- anti_join(data.S4, data.S4.temp, by = c("year","age.group"))


Average.data.S4 = NULL

for (i in unique(data.S4$age.group)) {
  
  df1 <- subset(data.S4, age.group == i)
  
  year.data = NULL
  age = unique(df1$age.group)
  
  for (j in unique(df1$year)) {
    
    df2 <- subset(df1, year == j)
    
    mean.value <- mean(df2$weight * 1000, na.rm=TRUE)
    
    sd.value <- sd(df2$weight * 1000, na.rm = TRUE)
    
    IC.low <- mean.value - (1.96*sd.value)
    
    IC.up <- mean.value + (1.96*sd.value)
    
    nb.fish <- as.numeric(nrow(df2))
    
    data.sample <- df2 %>% group_by(sample.id) %>%
      summarize(n.tot = n())
    
    nb.sample <- as.numeric(nrow(data.sample))
    
    Year <- as.numeric(unique(df2$year))
    
    df3 <- data.frame(Year,mean.value,IC.low, IC.up, nb.fish, nb.sample, age)
    
    year.data <- rbind(year.data, df3)
    
  }
  
  Average.data.S4 <- rbind(Average.data.S4, year.data)
  
}


S4<-ggplot(Average.data.S4, aes(x=Year, y=mean.value, col=as.factor(age))) + 
  geom_line(size=1) + 
  xlab("Year") +
  ylab("Average weight (g)") +
  labs(col="Age") +
  ylim(50,850) +
  ggtitle("4TVWX + gillnets and hooks") +
  theme_minimal(base_size = 14)

S4

cowplot::plot_grid(S1,S2,S3,S4, align = "v", ncol = 2)

Average.data.S1$Scenario <- paste("4T + G")
Average.data.S2$Scenario <- paste("4TVWX + G")
Average.data.S3$Scenario <- paste("4T + GH")
Average.data.S4$Scenario <- paste("4TVWX + GH")

All.Average <- rbind(Average.data.S1, Average.data.S2, Average.data.S3, Average.data.S4)

for (i in unique(All.Average$age)) {
  age.dat <- subset(All.Average, age ==i)
  age.group <- unique(age.dat$age)
  
  pd <- position_dodge(0.1) # move them .05 to the left and right
  
  g5.1<-ggplot(age.dat, aes(x = Year, y = mean.value, col=Scenario)) +
    geom_line(size = 1.5) +
    geom_errorbar(size = 0.5, aes(ymin=IC.low, ymax=IC.up), width=1.5, position=pd) +
    geom_line(position=pd, size=1) +
    geom_point(position=pd, size=1) +
    theme_minimal(base_size = 14) +
    ylim(min(age.dat$IC.low, na.rm = TRUE) - 50,max(age.dat$IC.up, na.rm = TRUE) + 50) +
    xlim(1973,2021) +
    ylab("Average weight (g)") +
    ggtitle(paste("Age", age.group, sep = ' ')) +
    theme(legend.position = c(0.8, 0.9))
  
  g5.2<-ggplot(age.dat, aes(x = as.factor(Year), y = as.factor(Scenario), fill = nb.sample)) +
    ylab("") +  xlab("Year") +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "red") + 
    theme_minimal(base_size = 14) +
    theme(legend.position="none")+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    ggtitle("Number of sample") +
    xlab("") +
    geom_text(aes(label = nb.sample), color = "black", size = 3)
  
  g5.3<-ggplot(age.dat, aes(x = as.factor(Year), y = as.factor(Scenario), fill = nb.fish)) +
    ylab("") +  xlab("Year") +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "red") + 
    theme_minimal(base_size = 14) +
    theme(legend.position="none")+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    ggtitle("Number of fish") +
    xlab("") +
    geom_text(aes(label = nb.fish), color = "black", size = 3)
  
  total.graph<-cowplot::plot_grid(g5.2,g5.3, g5.1, align = "v", ncol = 1, rel_heights = c(1, 1, 2))
  print(total.graph)
  
}

#### Number of fish required suggested by Elisabeth ####
#1) for all fish in the subset (all years combined), calculate the average weight-at-age (mu)

#2) sample 1 fish randomly from this subset with weight x, and calculate the absolute relative error (ARE = abs((x-mu)/mu))

#3) Sample 2 fish randomly from this subset, average weight (x), calculate the absolute relative error

#4) keep doing this for 3 fish to e.g. 100 fish

#5) Repeat steps 2 to 4 1000x

#6) make a boxplot with x=number of fish sampled and y= ARE

#7) define what error is acceptable and go with that number of fish.


#read commercial bio data imported from Oracle
mack_bio<-read.csv("C:/Users/BoudreauMA/Desktop/Maquereau/Data/bio.csv", header = TRUE, sep=",")

unique(mack_bio$year)
unique(mack_bio$month)
unique(mack_bio$gear.full)

plot(mack_bio$agef)  #outliers > 30 years old
plot(mack_bio$weight)  #No outliers but a couple of 0

# Remove outlier for age > 30 and weight = 0 and NA
mack_bio<-mack_bio[!is.na(mack_bio$weight),]
mack_bio<-subset(mack_bio, weight > 0)
data.temp<-subset(mack_bio, agef < 30 & agef  > 0)

#Check if there is no NA and outliers in agef and weight
unique(data.temp$agef)
summary(data.temp$weight)

#Create age.group 1 to 10+
data.temp$age.group <- ifelse(data.temp$agef < 10, data.temp$agef, 10)
unique(data.temp$age.group)


#### Scenario selected ####
# Subset of June/July, 4TVWX and gillnets
data.S1 <- data.temp %>% filter(month %in% c(6,7), nafo %in% c("4T","4V","4W","4X"), gear %in% c("GND", "GNS", "GN"))
unique(data.S1$month)
unique(data.S1$nafo)
unique(data.S1$gear)

data.S1.average <- data.S1 %>% group_by(age.group) %>%
  summarize(Mean.Weight = mean(weight))

# The next couple of lines runs a simulation 1000 times for each age class
# The simulation consist of taking a sample of 1 to 100 fish for each age class and then calculate the 
df4 <- NULL

for (a in 1:1000) {
  
  sim.run <- a
  
  df3 <- NULL
  
  for (b in unique(data.S1.average$age.group)) {
    
    data.S1.age <- subset(data.S1, age.group == b)
    
    df2 <- NULL
    
      for (c in 1:100) {
      
      # Sample 1 to 100 fish for each step
      data.S1.sample <- sample_n(data.S1.age, c, replace = TRUE)
      
      # Keep only age group and weight of the sampled fish 
      data.S1.sample <- data.S1.sample %>% dplyr::select(age.group, weight)
      
      # Calculate mean weight of fish sampled
      Average.Weight.Sampled <- sum(data.S1.sample$weight)/nrow(data.S1.sample)
      
      # Join together the average weight at age of the whole subset of data with the individual weight of fish sampled
      test <- left_join(data.S1.sample, data.S1.average, by = "age.group")
      
      # Estimate absolute relative error
      ARE <- (Average.Weight.Sampled-unique(test$Mean.Weight))/unique(test$Mean.Weight)
      
      # Get the number of fish sampled
      nb.fish <- as.numeric(nrow(data.S1.sample))
      
      # Get the age.group of the fish sampled
      age.fish <- as.numeric(unique(data.S1.sample$age.group))
      
      # Data frame of the current run (i.e. i = 2 fish)
      df1 <- data.frame(age.fish, nb.fish, ARE)
      
      # Compiled the result of the current run with those of previous runs
      df2 <- rbind(df2,df1)
    
      }
    
    df3 <- rbind(df3, df2)
  
  }
  
  df3$sim.run <- as.numeric(sim.run)
  df4 <- rbind(df4, df3)
  
 }

write.csv(df4, "C:/Users/BoudreauMA/Desktop/Maquereau/Data/Nb.fish.simulation.csv", row.names = FALSE)

# for each age class and each number of fish sampled, estimate the mean error value ± IC95 
# Import simulation data.set
sim.data <- read.csv("C:/Users/BoudreauMA/Desktop/Maquereau/Data/Nb.fish.simulation.csv", header = TRUE, sep = ",")

df5 <- NULL

for (i in unique(sim.data$age.fish)) {
  
  sub.data <- subset(sim.data, age.fish == i)
  
  df6 <- NULL
  
  for (j in unique(sub.data$nb.fish)) {
    
    df2 <- subset(sub.data, nb.fish == j)
    
    mean.value <- mean(df2$ARE, na.rm=TRUE)
    
    sd.value <- sd(df2$ARE, na.rm = TRUE)
    
    IC.low <- mean.value - (1.96*sd.value)
    
    IC.up <- mean.value + (1.96*sd.value)
    
    nb.fish <- unique(df2$nb.fish)
    
    age <- unique(df2$age.fish)
    
    df3 <- data.frame(age, nb.fish, mean.value, IC.low, IC.up)
    
    df6 <- rbind(df6, df3) 
    
  }
  
  df5 <- rbind(df5, df6) 
  
  
} 

pd <- position_dodge(0.1) # move them .05 to the left and right

for (i in unique(df5$age)) {
  
  age.data <- subset(df5, age == i)
  
  p <- ggplot(age.data, aes(x = nb.fish, y = mean.value)) +
    geom_line(size = 1.5) +
    geom_errorbar(size = 0.5, aes(ymin=IC.low, ymax=IC.up), width=1.5, position=pd) +
    geom_line(position=pd, size=1) +
    geom_point(position=pd, size=1) +
    geom_hline(yintercept=0.1, linetype="dashed", color = "red") +
    geom_hline(yintercept=-0.1, linetype="dashed", color = "red") +
    theme_minimal(base_size = 14) + 
    ylab("Mean relative error ± IC 95%") + 
    xlab("Number of fish sampled") +
    scale_x_continuous(breaks = seq(0,100,5)) +
    ggtitle(paste("Age", unique(age.data$age), sep = " "))
  
  print(p)
  
}



#### Weight-at-age with selected scenario ####

library(dplyr)
library(ggplot2)
library(cowplot)
library(tidyr)

#read commercial bio data imported from Oracle
mack_bio<-read.csv("C:/Users/BoudreauMA/Desktop/Maquereau/Data/bio.csv", header = TRUE, sep=",")

plot(mack_bio$agef)  #outliers > 30 years old
plot(mack_bio$weight)  #No outliers but a couple of 0

# Remove outlier for age > 30 and weight = 0 and NA
mack_bio<-mack_bio[!is.na(mack_bio$weight),]
mack_bio<-subset(mack_bio, weight > 0)
data.temp<-subset(mack_bio, agef < 30 & agef  > 0)

#Check if there is no NA and outliers in agef and weight
unique(data.temp$agef)
summary(data.temp$weight)

#Create age.group 1 to 10+
data.temp$age.group <- ifelse(data.temp$agef < 10, data.temp$agef, 10)
unique(data.temp$age.group)

#### Scenario 2 ####
# Subset of June/July, 4T, 4W, 4V, 4X and gillnets
data.S2 <- data.temp %>% filter(month %in% c(6,7), nafo %in% c("4T", "4V", "4W", "4X"), gear %in% c("GND", "GNS", "GN"))
unique(data.S2$month)
unique(data.S2$nafo)
unique(data.S2$gear)

#Remove year-age group that contain less than 10 fish 
data.S2.temp<- data.S2 %>% group_by(year, age.group) %>% 
summarize(nb.fish = n()) %>%
filter(nb.fish < 10)

data.S2 <- anti_join(data.S2, data.S2.temp, by = c("year","age.group"))


# For each combinaitions of Year and age, summarize mean weight, IC low and high, nb of sample and nb of fish
Average.data.S2 = NULL

for (i in unique(data.S2$age.group)) {
  
  df1 <- subset(data.S2, age.group == i)
  
  year.data = NULL
  age = unique(df1$age.group)
  
  for (j in unique(df1$year)) {
    
    df2 <- subset(df1, year == j)
    
    mean.value <- mean(df2$weight * 1000, na.rm=TRUE)
    
    sd.value <- sd(df2$weight * 1000, na.rm = TRUE)
    
    IC.low <- mean.value - (1.96*sd.value)
    
    IC.up <- mean.value + (1.96*sd.value)
    
    nb.fish <- as.numeric(nrow(df2))
    
    data.sample <- df2 %>% group_by(sample.id) %>%
      summarize(n.tot = n())
    
    nb.sample <- as.numeric(nrow(data.sample))
    
    Year <- as.numeric(unique(df2$year))
    
    df3 <- data.frame(Year,mean.value,IC.low, IC.up, nb.fish, nb.sample, age)
    
    year.data <- rbind(year.data, df3)
    
  }
  
  Average.data.S2 <- rbind(Average.data.S2, year.data)
  
}

# graph of the average weight-at-age time series 
S2<-ggplot(Average.data.S2, aes(x=Year, y=mean.value, col=as.factor(age))) + 
  geom_line(size=1) + 
  #scale_x_continuous(breaks = seq(1970, 2022, 5)) +
  xlab("Year") +
  ylab("Average weight (g)") +
  labs(col="Age") +
  ylim(50,850) +
  ggtitle("4TVWX + gillnets") +
  theme_minimal(base_size = 14)
S2

# Complete all combinations of Year and age to have 0 value in tile graph
Average.data.S2 <- Average.data.S2 %>% complete(Year, age)
Average.data.S2["nb.fish"][is.na(Average.data.S2["nb.fish"])] <- 0

# Heat map kind of graph to show the number of fish weighted for each combination of year and age
g5.3<-ggplot(Average.data.S2, aes(x = Year, y = as.factor(age), fill = nb.fish)) +
  ylab("Age") +
  geom_tile() +
  #scale_x_continuous(breaks = seq(1970, 2022, 5)) +
  scale_fill_gradient(low = "white", high = "red") + 
  theme_minimal(base_size = 14) +
  labs(fill = "Nb fish") +
  #theme(legend.position="none")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Number of fish") +
  xlab("") +
  geom_text(aes(label = nb.fish), color = "black", size = 3)

cowplot::plot_grid(g5.3, S2, align = "v", nrow = 2, rel_heights = c(1, 2))

ggsave(filename = "C:/Users/BoudreauMA/Desktop/Maquereau/AverageWAA.n.tot.png", width = 12, height = 8)
ggsave(filename = "C:/Users/BoudreauMA/Desktop/Maquereau/AverageWAA.n10.png", width = 12, height = 8)
