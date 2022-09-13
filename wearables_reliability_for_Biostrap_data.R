# reliability analyses for data from a wearable device as described in Dudarev et al 2022.
# two types of reliability are computed: between participant and within participant.
# between participant reliability takes equal number of observations for at least 2 individuals and computes ICC. 
# within participant reliability is computed in two ways, time-sensitive and random, as describe din Dudarev et al 2022.

# this script is a case of wearables_reliability adapted for the specific study described in Dudarev et al 2022. 
# Namely, it removes participants with less than 6 days and 5 nights for between-participant reliability estimation, and
# separates data by sleep state into asleep vs. awake for all reliability calculations.

# BETWEEN PARTICIPANT RELIABILITY is estimated with ICC. 
# Data structure accepted is either 
# (a) two variables: participant and the physiological measurement, with multiple rows per participant, or
# (b) multiple variables: participant and several instances of physiological variable sampling, with one row per participant.

# WITHIN PARTICIPANT RELIABILITY is estimated with mixed model regression and split-half approach. 
# Two methods of splitting the data are implemented: time-sensitive and random. Time-sensitive method returns one estimate for reliability. Random
# method returns a distribution of estimates of reliability and average of that distribution as a final estimate of within-participant
# Data structure accepted is: 
# 3 variables: participant, physiological measurement, and the variable that groups datapoints within participants (in our case, days).
# The code includes a quick data cleaning script that removes phsyiological variable = 0 and datapoints above and below 2SD from the mean per 
# participant. 

######################### BETWEEN PARTICIPANT RELIABILITY
library(psych)
library(readxl)
library(dplyr)
library(plyr)

# read data
data_bs <- read_excel('./Dudarev et al 2022 data.xlsx', sheet = "Aggregated_biometrics_with_mood")
#remove participant 187 and 189 due to less than 5 nights and 6 days of data obtained for them
data_bs <- data_bs[data_bs$`Participant ID` != 187 & data_bs$`Participant ID` != 189, ]

#list to store results
bsr <- list()

for (x in c("averageHRV.day","averageHRV.nig","averageBPM.day","averageBPM.nig")) {
  # prepare dataframe
  df <- data_bs[,c("Participant ID",x)] %>% na.omit
  names(df)[1] <- c("participant")
  # check if we have more than one value per participant, restructure so that data has 1 row per participant  
  # and as many columns as the maximum number of observations within participants
  num_participants <- length(unique(df$participant))
  if ( num_participants < nrow(df)) {
    #determine number of observations per participant
    max_of_x <- max(count(df$participant)[2])
    #restructure the data
    df_icc <- lapply(split(df,df$participant),function(y) {t(y[1:max_of_x,2]) %>% unname}) %>% do.call(rbind,.) %>% as.data.frame
  } else {
    df_icc <- within(df,rm("participant"))
  }
  icc <- ICC(df_icc)
  bsr <- c(bsr,icc[1])
}

paste('Between-participant reliability for HRV during wakeful time')
bsr[1]
paste('Between-participant reliability for HRV during sleep')
bsr[2]
paste('Between-participant reliability for heart rate during wakeful time')
bsr[3]
paste('Between-participant reliability for heart rate during sleep')
bsr[4]

######################################### WITHIN PARTICIPANT RELIABILITY
library(readxl)
library(lmerTest)

# silence warnings from summarise
options(dplyr.summarise.inform = FALSE)

## Define function to compute reliability of odd-even samples grouped by participant and by variable y (in our case rest date)
withinRealiability <- function (df) {
  #create dataframe that contains one row for each participant, with the average value of X for odd samples (X1) and 
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

# read data
data_ws <- read_excel('./Dudarev et al 2022 data.xlsx', sheet = "Biometric_raw_data")
data_ws <- data_ws[data_ws$user_id != 187 & data_ws$user_id != 189, ]

#define functions to store results 
b <- c() #results for time sensitive
wsrel <- c() #results for random split

for (x in c("hrv","bpm")) {
  # prepare dataframe
  df_all <- data_ws[,c("user_id",x,"restDate","restType","capture_time")] %>% na.omit
  names(df_all) <- c("participant","x","y","z","t")
  
  for (user_state in c("awake","sleep")) {
    df <- df_all[df_all$z == user_state, ]
    ### Data cleaning OMIT OR REPLACE THIS STEP IF NOT APPLICABLE TO YOUR DATA
    # remove X = 0
    df_clean <- df[df$x > 0, ]
    #remove values outside the range within [mean - 2*sd, mean + 2*sd] per participant.
    df_clean <- lapply(split(df_clean,df_clean$participant),function(ss_data) {
      threshold_low <- mean(ss_data$x) - 2 * sd(ss_data$x)
      threshold_high <- mean(ss_data$x) + 2 * sd(ss_data$x)
      return(ss_data[ss_data$x > threshold_low & ss_data$x < threshold_high, ])
    }) %>% do.call(rbind,.) %>% as.data.frame
  
    #################################### compute time-sensitive reliability
    #Compute odd-even reliability, ordered by time
    b <- c(b,withinRealiability(df_clean[order(df_clean$t),]))
    #################################### compute random split reliability
    #compute 1000 iterations of odd-even reliability with a randomly shuffled dataframe
    beta_rs <- sapply(rep(1,1000), function(x) withinRealiability(df_clean[sample(1:nrow(df_clean)),]))
    #store results for this combination of user_state and measure of interest
    wsrel <- c(wsrel,mean(beta_rs))
    #plot histogram of 100 random interations
    hist(beta_rs,main=paste0(x,user_state,sep=" "))
  }
}


paste('Within-participant reliability with time-sensitive split is estimated as follows: HRV during wakeful time',
       round(b[1],3),
      "HRV during sleep",
      round(b[2],3),
      "heart rate during wakeful time",
      round(b[3],3),
      "heart rate during sleep",
      round(b[4],3))
paste('Random split results in within-participant reliability estimated as follows: HRV during wakeful time',
      round(wsrel[1],3),
      "HRV during sleep",
      round(wsrel[2],3),
      "heart rate during wakeful time",
      round(wsrel[3],3),
      "heart rate during sleep",
      round(wsrel[4],3))

#set warnings options back to default
options(dplyr.summarise.inform = TRUE)