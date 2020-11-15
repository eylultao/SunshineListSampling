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

# PART 1 OPTIMAL ALLOCATION SAMPLE SIZE


attach(data2018)
N.h.2018 <- tapply(Salary.Paid, Sector, length) 
sectors.2018 <- names(N.h.2018)
# fill in the variance matrix
variances = c()
# using 2018 data for variances -> will be used as guess variances for calculating optimal allocation
for (s in sectors.2018){
  group_s = data2018[data2018$Sector == s,]
  var_s = var(group_s$Salary.Paid)
  variances = c(variances, var_s)
}
detach(data2018)


attach(data2019)
N.h.2019 <- tapply(Salary.Paid, Sector, length)
sectors.2019 <- names(N.h.2019)
# assuming equal cost across strata
ratio = N.h.2019 * variances / sum(N.h.2019 * variances)
n.h.optim <- round (ratio * n)
n.h.optim[4] =2
sum(n.h.optim) # sanity check, adds up to 206
n.h.optim
detach(data2019)

# PART 2.2 STRATIFIED SAMPLING WITH OPTIMAL ALLOCATION
# create sample
set.seed(10)
STR.sample.optim <- NULL
i = 1
# creating sample
for (s in sectors.2019){
  group_s = data2019[data2019$Sector == s,]
  group_s_size = length(group_s$Sector)
  group_s_sample_indices <- sample(group_s_size, n.h.optim[i], replace = F)
  STR_sample_group_s <- group_s[group_s_sample_indices,]
  STR.sample.optim <- rbind(STR.sample.optim, STR_sample_group_s)
  i = i+1
}
table(STR.sample.optim$Sector) # sanity check


# now that we picked farms to check for our stratas, calculate mean and SE
ybar.h.optim <- tapply(STR.sample.optim$Salary.Paid, STR.sample.optim$Sector, mean) # array with mean from each region
var.h.optim <- tapply(STR.sample.optim$Salary.Paid, STR.sample.optim$Sector, var) #array with variance from each region
se.h.optim <- sqrt( var.h.optim / n.h.optim) # array with SE from each region
rbind(ybar.h.optim, se.h.optim) # for each region, pair each mean with their respective SE

# combine the stratified sample to get population mean
ybar.str.optim <- sum(N.h.2019 / N * ybar.h.optim)
se.str.optim <- sqrt(sum((N.h.2019 / N)^2 * se.h.optim^2))

# calculate proportion for binary variable
# calculate proportion for binary variable
Salary_Paid_Binary <- STR.sample.optim$Salary.Paid > proportion_cutoff
STR.sample.optim$Binary <- (STR.sample.optim$Salary.Paid > proportion_cutoff)* 1
proportion_s <- NULL
group_s <- NULL
for (s in sectors.2019){
  group_s <- STR.sample.optim[STR.sample.optim$Sector == s,]
  group_s_true_count <- sum(group_s$Binary)
  group_s_proportion <- group_s_true_count/length(group_s$Sector)
  proportion_s <- c(proportion_s, group_s_proportion)
}
sectors.2019
proportion_s
proportion.h.optim <- sum(proportion_s * ratio)
proportion.h.optim
table(STR.sample.optim$Binary)

proportion.h.se = sqrt((proportion.h.optim * (1-proportion.h.optim))/n)

str.optim <- c(ybar.str.optim, se.str.optim, proportion.h.optim, proportion.h.se ) #array with out results, population mean and SE, population proportion and SE
str.optim

# TODO: construct confidence intervals
se.str.optim
ybar.str.optim
proportion.h.se

# DATA VISUALIZATION
library(ggplot2)

sectors_short = c("College", "Crown", "Judiciary", "Legislative", "Ministries", "Hospitals", "Municipalities", "Power", "Other", "School Board", "Universities")
sample_sizes  = c(3,16,5,2,20,53,22,18,15,9,44)
plot_df = data.frame(Sectors = sectors_short, Count = sample_sizes)
p <- ggplot(data = plot_df, aes(x = Sectors, y = Count)) 
p + geom_bar(stat="identity", fill = "steelblue") + geom_text(aes(label=Count), vjust=1.5, color="white", size=3.5) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Distribution of Sample Across Sectors for Optimal Allocation")

plot_df2 = data.frame(Sectors = sectors_short, Avg_Salary = round(ybar.h.optim))

p2 <- ggplot(data = plot_df2, aes(x = Sectors, y = Avg_Salary))
p2 + geom_bar(stat="identity", fill = "steelblue") + geom_text(aes(label=Avg_Salary), vjust=1.5, color="white", size=3.5) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Distribution of Sample Mean Salary Across Sectors for Optimal Allocation")


plot_df3 = data.frame(Sectors = sectors_short, SE_Salary = round(se.h.optim))

p3 <- ggplot(data = plot_df3, aes(x = Sectors, y = SE_Salary))
p3 + geom_bar(stat="identity", fill = "steelblue") + geom_text(aes(label=SE_Salary), vjust=1.5, color="white", size=3.5) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Distribution of Sample Salary SE Across Sectors for Optimal Allocation")

