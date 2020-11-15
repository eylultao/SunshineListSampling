setwd("/Users/eylulaygun/Desktop/Year\ 5/STAT\ 344/project" )
getwd()

data2018 <-  read.csv("sunshinelist_2018_cleaned.csv", header = T)
data2019 <-  read.csv("sunshinelist_2019_cleaned.csv", header = T)

data2019$Salary.Paid <- as.numeric(gsub(",", "", substring(data2019$Salary.Paid, 2) , fixed = TRUE) ) #change data type for salary paid from char to num
data2018$Salary.Paid <- as.numeric(gsub(",", "", substring(data2018$Salary.Paid, 2) , fixed = TRUE) ) #change data type for salary paid from char to num

var2018 <- var(data2018$Salary.Paid)
var2019 <- var(data2019$Salary.Paid)
N <- length(data2019$Sector) # population size
proportion_cutoff = 152980.88 # 100000$ in 1996 adjusted w.r.t. inflation


# PART 1: SAMPLE SIZE CALCULATION 
set.seed(10)

# PART 1.1 SRS SAMPLE SIZE
# using 2018 data, determine sample size for 2019
# sample size constraints= fpc, 95% CI, +- $5,000

# ignoring FPC
# if we want a 95% confidence interval with +- 5000$ dollars
n1 = round((1.96^2 * var2018) /5000^2) # 206
# if we want a 95% confidence interval with +- 10000$ dollars (in case we were curious)
n2 = round(1.96^2 * var2018 /10000^2) # 52

# what would the sample sizes be like for the binary data?
# if we want a 95% CI with +- 0.05 half width (aka 5.0 percentage points)
var_guess = 0.25 # assuming worst case
n3 = round((1.96^2 * var_guess) /0.05^2)
#table(data2018$Salary.Paid > proportion_cutoff)
#informed_prop = 22184/(129191 + 22184)
#var_informed = informed_prop * (1 - informed_prop)
var_informed = 0.14655 * (1- 0.14655) # using 2018 data 
n4 = round((1.96^2 * var_informed) /0.05^2)

# determine n using the max of binary and continuous sample size
n = max(n1, n4)

# PART 2: SAMPLING
set.seed(10)
SRS.indices <- sample.int(N, n, replace = F) #randomly selected indices
SRS.sample <- data2019[SRS.indices, ] # random sample using the indices selected above
table(SRS.sample$Sector) # see the distribution of sectors in our sample

# sample mean
ybar.srs <- mean(SRS.sample$Salary.Paid) # sample mean from SRS sample
se.srs <- sqrt((1-n/N) * var(SRS.sample$Salary.Paid) / n) # sample variance, using FPC
# when we do a simple random sample, we see that the avg salary is = 129736.99$ with se = 2785.89$

# sample proportion 
#table(data2019$Salary.Paid > proportion_cutoff) # true value of the proportion we pretend not to know 0.1463674638 -> 14.6%
Salary_Paid_Binary <- SRS.sample$Salary.Paid > proportion_cutoff
table(Salary_Paid_Binary) # see what the table looks like
#in our sample of 206 employees, 33 of them make above the cutoff, resulting in %16 proportion
prop_true = sum(Salary_Paid_Binary, na.rm= TRUE) # counts number of TRUES
prop_srs <- prop_true / n
prop_se = sqrt((prop_srs * (1-prop_srs))/n)
srs <- c(ybar.srs, se.srs, prop_srs, prop_se) # put all the results of SRS sample together

# PART 3: Constructing CI 

# we say that the sample mean is an estimator for population mean
# we also say that sample SE is an estimator for population SE
mean_lower = ybar.srs - 1.96 * se.srs
mean_upper = ybar.srs + 1.96 * se.srs
srs_mean_ci = cbind(mean_lower, mean_upper) # we ended up with half width 5460CAD, is that ok?

# we say that the sample proportion is an estimator for population proportion
# we say that sqrt(p(1-p)/n) is an estimator for population se where p is the sample proportion = 0.16
prop_lower = prop_srs - 1.96* prop_se
prop_upper = prop_srs + 1.96* prop_se
srs_prop_ci = cbind(prop_lower, prop_upper)



# DATA VISUALIZATION
library(ggplot2)

sectors_short = c("College", "Crown", "Judiciary", "Legislative", "Ministries", "Hospitals", "Municipalities", "Power", "Other", "School Board", "Universities")
sample_sizes  = c(9,7,2,20,28,63,21,0,11,31,14)
plot_df = data.frame(Sectors = sectors_short, Count = sample_sizes)
p <- ggplot(data = plot_df, aes(x = Sectors, y = Count)) 
p + geom_bar(stat="identity", fill = "steelblue") + geom_text(aes(label=Count), vjust=1.5, color="white", size=3.5) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Distribution of Sample Across Sectors for SRS")


