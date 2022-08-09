# reliability analyses for data from a wearable device as described in Dudarev et al 2022.
# two types of reliability are computed: between participant and within participant.
# between participant reliability takes equal number of observations for at least 2 individuals and computes ICC. 
# within participant reliability is computed in two ways, time-sensitive and random, as describe din Dudarev et al 2022.

# BETWEEN PARTICIPANT RELIABILITY is estimated with ICC. 
# Data structure accepted is either 
# (a) two variables: participant and the physiological measurement, with multiple rows per participant, or
# (b) multiple variables: participant and several instances of physiological variable sampling, with one row per participant.
# INPUT WILL BE NEEDED in lines 29, 35.

# WITHIN PARTICIPANT RELIABILITY is estimated with mixed model regression and split-half approach. 
# Two methods of splitting the data are implemented: time-sensitive and random. Time-sensitive method returns one estimate for reliability. Random
# method returns a distribution of estimates of reliability and average of that distribution as a final estimate of within-participant
# Data structure accepted is: 
# 3 variables: participant, physiological measurement, and the variable that groups datapoints within participants (in our case, days).
# The code includes a quick data cleaning script that removes phsyiological variable = 0 and datapoints above and below 2SD from the mean per 
# participant. 
# INPUT WILL BE NEEDED in lines 60,67-70.

######################### BETWEEN PARTICIPANT RELIABILITY
library(psych)
library(readxl)
library(dplyr)
library(plyr)

# read data
# INPUT: point to your datafile in line 29.
data_bs <- read_excel('./Dudarev et al 2022 data.xlsx', sheet = "Aggregated_biometrics_with_mood")

#Select only variables of interest: Participant ID and physiological measurement of interest (in our case, HRV).
#Measurement of interest could be one or several columns, assuming that the values in them represent different 
#samples of the same physiological index.
## INPUT: replace variable names in line 35.
df <- data_bs[,c("Participant ID","averageHRV.day")] %>% na.omit
names(df)[1] <- c("participant")

# check if we have more than one value per participant, restructure so that data has 1 row per participant  
# and as many columns as the minimum number of observations within participants
num_participants <- length(unique(df$participant))
if ( num_participants < nrow(df)) {
  #determine number of observations per participant
  min_of_x <- min(count(df$participant))
  #select first "min_of_x" observations per participants
  df_icc <- lapply(split(df,df$participant),function(x) {t(x[1:min_of_x,2]) %>% unname}) %>% do.call(rbind,.) %>% as.data.frame
} else {
  df_icc <- within(df,rm("participant"))
  }

#compute Intraclass Correlations
icc <- ICC(df_icc)
icc

######################################### WITHIN PARTICIPANT RELIABILITY
library(readxl)
library(lmerTest)

# read data
# INPUT: point to your datafile in line 60.
data_ws <- read_excel('./Dudarev et al 2022 data.xlsx', sheet = "Biometric_raw_data")

# define variables: participant, X for the measurement of interest (in our case, HRV). Y for the variable that groups datapoints within participants
# (in our case, days). t for the variable that contains timestamps,
# X accepts only one column, with several datapoints for each participant arranged in rows.
# prepare dataframe
# INPUT: replace variable names on the right side of the equations in lines 67-70.
df <- data.frame("participant" = data_ws$user_id,
                 "x" = data_ws$hrv,
                 "y"= data_ws$restDate,
                 "t"= data_ws$capture_time) %>% na.omit

### Data cleaning OMIT OR REPLACE THIS STEP IF NOT APPLICABLE TO YOUR DATA
# remove X = 0
df_clean <- df[df$x > 0, ]
#remove values outside the range within [mean - 2*sd, mean + 2*sd] per participant.
df_clean <- lapply(split(df_clean,df_clean$participant),function(ss_data) {
  threshold_low <- mean(ss_data$x) - 2 * sd(ss_data$x)
  threshold_high <- mean(ss_data$x) + 2 * sd(ss_data$x)
  return(ss_data[ss_data$x > threshold_low & ss_data$x < threshold_high, ])
  }) %>% do.call(rbind,.) %>% as.data.frame
#####


## Define function to compute reliability of odd-even samples grouped by participant and by variable y (in our case rest date)
withinRealiability <- function (df) {
  #create dataframe that contains one row for each Y for each participant, with the average value of X for odd samples (X1) and 
  # average value of X for even samples (X2) 
  df_ws <- df %>% group_by (participant, y) %>% dplyr::summarise(
    x1 = mean (x[seq_len(n())%%2 == 1]),
    x2 = mean (x[seq_len(n())%%2 == 0]),
  ) %>% ungroup
  
  # compute reliability
  odd_even_rel <- lmer(x1 ~ x2 + (1|participant), data=df_ws)
  r_c <- coef(summary(odd_even_rel))
  return( r_c[2,1])
  
}

#################################### compute time-sensitive reliability
#Compute odd-even reliability, ordered by time
beta_ts <- withinRealiability(df_clean[order(df_clean$t),])

#################################### compute random split reliability
# silence warnings from summarise
options(dplyr.summarise.inform = FALSE)

#compute 1000 iterations of odd-even reliability with a randomly shuffled dataframe
beta_rs <- sapply(rep(1,1000), function(x) withinRealiability(df_clean[sample(1:nrow(df_clean)),]))

#set warnings options back to default
options(dplyr.summarise.inform = TRUE)

mean(beta_rs)
hist(beta_rs)

###print Results
paste('Within-participant reliability with time-sensitive split is estimated at',
      beta_ts,
      'Random split results in within-participant reliability estimate of',
      mean(beta_rs))
