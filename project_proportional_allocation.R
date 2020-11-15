setwd("/Users/eylulaygun/Desktop/Year\ 5/STAT\ 344/project" )
getwd()

data2018 <-  read.csv("sunshinelist_2018_cleaned.csv", header = T)
data2019 <-  read.csv("sunshinelist_2019_cleaned.csv", header = T)

data2019$Salary.Paid <- as.numeric(gsub(",", "", substring(data2019$Salary.Paid, 2) , fixed = TRUE) ) #change data type for salary paid from char to num
data2018$Salary.Paid <- as.numeric(gsub(",", "", substring(data2018$Salary.Paid, 2) , fixed = TRUE) ) #change data type for salary paid from char to num
var2018 <- var(data2018$Salary.Paid)
N <- length(data2019$Sector)
proportion_cutoff = 152980.88 # 100000$ in 1990 changed wrt inflation
n = 206 # from earlier

# PART 1: SAMPLE SIZE CALCULATION 
set.seed(10)

# ROPORTIONAL ALLOCATION SAMPLE SIZE
attach(data2019)
N.h.2019 <- tapply(Salary.Paid, Sector, length) 
sectors.2019 <- names(N.h.2019)
ratio_prop <- N.h.2019 / sum(N.h.2019)
n.h.prop <- round(ratio_prop *n - 0.1) # decreased with modifier -0.05 so our sample size sums up to 206
n.h.prop[3] = 2 # hardcoded them to 2 because otherwise it only gives 1 sample and it creates problem with calculating variance
n.h.prop[4] =2
sum(n.h.prop)
detach(data2019)

# PART 2 STRATIFIED SAMPLING WITH PROPORTIONAL ALLOCATION
set.seed(10)
STR.sample.prop <- NULL
i = 1
# creating sample
for (s in sectors.2019){
  group_s = data2019[data2019$Sector == s,]
  group_s_size = length(group_s$Sector)
  group_s_sample_indices <- sample(group_s_size, n.h.prop[i], replace = F)
  STR_sample_group_s <- group_s[group_s_sample_indices,]
  STR.sample.prop <- rbind(STR.sample.prop, STR_sample_group_s)
  i = i+1
}
table(STR.sample.prop$Sector) # sanity check


# now that we picked farms to check for our stratas, calculate mean and SE
ybar.h.prop <- tapply(STR.sample.prop$Salary.Paid, STR.sample.prop$Sector, mean) # array with mean from each region
var.h.prop <- tapply(STR.sample.prop$Salary.Paid, STR.sample.prop$Sector, var) #array with variance from each region
se.h.prop <- sqrt( var.h.prop / n.h.prop) # array with SE from each region
rbind(ybar.h.prop, se.h.prop) # for each region, pair each mean with their respective SE

# combine the stratified sample to get population mean
ybar.str.prop <- sum(N.h.2019 / N * ybar.h.prop)
se.str.prop <- sqrt(sum((N.h.2019 / N)^2 * se.h.prop^2))

# calculate proportion for binary variable
Salary_Paid_Binary <- STR.sample.prop$Salary.Paid > proportion_cutoff
STR.sample.prop$Binary <- (STR.sample.prop$Salary.Paid > proportion_cutoff)* 1
proportion_s <- NULL
group_s <- NULL
for (s in sectors.2019){
  group_s <- STR.sample.prop[STR.sample.prop$Sector == s,]
  group_s_true_count <- sum(group_s$Binary)
  group_s_proportion <- group_s_true_count/length(group_s$Sector)
  proportion_s <- c(proportion_s, group_s_proportion)
}
sectors.2019
proportion_s
proportion.h.prop <- sum(proportion_s * (N.h.2019/N))
proportion.h.prop
table(STR.sample.prop$Binary)

proportion.h.se = sqrt((proportion.h.prop * (1-proportion.h.prop))/n)
proportion.h.se
str.prop <- c(ybar.str.prop, se.str.prop, proportion.h.prop, proportion.h.se ) #array with out results, population mean and SE, population proportion and SE
str.prop

# DATA VISUALIZATION
library(ggplot2)

sectors_short = c("College", "Crown", "Judiciary", "Legislative", "Ministries", "Hospitals", "Municipalities", "Power", "Other", "School Board", "Universities")
sample_sizes  = c(8,9,2,22,26,60,25,2,10,35,9)
plot_df = data.frame(Sectors = sectors_short, Count = sample_sizes)
p <- ggplot(data = plot_df, aes(x = Sectors, y = Count)) 
p + geom_bar(stat="identity", fill = "steelblue") + geom_text(aes(label=Count), vjust=1.5, color="white", size=3.5) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Distribution of Sample Across Sectors for Proportional Allocation")

plot_df2 = data.frame(Sectors = sectors_short, Avg_Salary = round(ybar.h.prop))

p2 <- ggplot(data = plot_df2, aes(x = Sectors, y = Avg_Salary))
p2 + geom_bar(stat="identity", fill = "steelblue") + geom_text(aes(label=Avg_Salary), vjust=1.5, color="white", size=3.5) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Distribution of Sample Mean Salary Across Sectors for Proportional Allocation")

