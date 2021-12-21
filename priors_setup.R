

Qbs_rolling_prior_21 <- read_csv("/Users/dylanmervis/Desktop/Priors/Qbs/Qbs_2021_Priors.csv") %>%
  select(Name, VOA) %>%
  mutate(Season = 2021)
Qbs_rolling_prior_20 <- read_csv("/Users/dylanmervis/Desktop/Priors/Qbs/Qbs_2020_Priors.csv") %>%
  select(Name, VOA) %>%
  mutate(Season = 2020)
Qbs_rolling_prior_19 <- read_csv("/Users/dylanmervis/Desktop/Priors/Qbs/Qbs_2019_Priors.csv") %>%
  select(Name, VOA) %>%
  mutate(Season = 2019)
Qbs_rolling_prior_18 <- read_csv("/Users/dylanmervis/Desktop/Priors/Qbs/Qbs_2018_Priors.csv") %>%
  select(Name, VOA) %>%
  mutate(Season = 2018)
Qbs_rolling_prior_17 <- read_csv("/Users/dylanmervis/Desktop/Priors/Qbs/Qbs_2017_Priors.csv") %>%
  select(Name, VOA) %>%
  mutate(Season = 2017)
Qbs_rolling_prior_16 <- read_csv("/Users/dylanmervis/Desktop/Priors/Qbs/Qbs_2016_Priors.csv") %>%
  select(Name, VOA) %>%
  mutate(Season = 2016)
Qbs_rolling_prior_15 <- read_csv("/Users/dylanmervis/Desktop/Priors/Qbs/Qbs_2015_Priors.csv") %>%
  select(Name, VOA) %>%
  mutate(Season = 2015)
Qbs_rolling_prior_14 <- read_csv("/Users/dylanmervis/Desktop/Priors/Qbs/Qbs_2014_Priors.csv") %>%
  select(Name, VOA) %>%
  mutate(Season = 2014)
Qbs_rolling_prior_13 <- read_csv("/Users/dylanmervis/Desktop/Priors/Qbs/Qbs_2013_Priors.csv") %>%
  select(Name, VOA) %>%
  mutate(Season = 2013)
Qbs_rolling_prior_12 <- read_csv("/Users/dylanmervis/Desktop/Priors/Qbs/Qbs_2012_Priors.csv") %>%
  select(Name, VOA) %>%
  mutate(Season = 2012)
Qbs_rolling_prior_11 <- read_csv("/Users/dylanmervis/Desktop/Priors/Qbs/Qbs_2011_Priors.csv") %>%
  select(Name, VOA) %>%
  mutate(Season = 2011)
Qbs_rolling_prior_10 <- read_csv("/Users/dylanmervis/Desktop/Priors/Qbs/Qbs_2010_Priors.csv") %>%
  select(Name, VOA) %>%
  mutate(Season = 2010)
Qbs_rolling_prior_09 <- read_csv("/Users/dylanmervis/Desktop/Priors/Qbs/Qbs_2009_Priors.csv") %>%
  select(Name, VOA) %>%
  mutate(Season = 2009)
Qbs_rolling_prior_08 <- read_csv("/Users/dylanmervis/Desktop/Priors/Qbs/Qbs_2008_Priors.csv") %>%
  select(Name, VOA) %>%
  mutate(Season = 2008)
Qbs_rolling_prior_07 <- read_csv("/Users/dylanmervis/Desktop/Priors/Qbs/Qbs_2007_Priors.csv") %>%
  select(Name, VOA) %>%
  mutate(Season = 2007)

Qbs_Priors <- rbind(Qbs_rolling_prior_07, Qbs_rolling_prior_08,
                    Qbs_rolling_prior_09, Qbs_rolling_prior_10, Qbs_rolling_prior_11,
                    Qbs_rolling_prior_12, Qbs_rolling_prior_13, Qbs_rolling_prior_14,
                    Qbs_rolling_prior_15, Qbs_rolling_prior_16, Qbs_rolling_prior_17,
                    Qbs_rolling_prior_18, Qbs_rolling_prior_19, Qbs_rolling_prior_20,
                    Qbs_rolling_prior_21) %>%
  rename(Pass_Weight = VOA)



Wrs_rolling_prior_21 <- read_csv("/Users/dylanmervis/Desktop/Priors/Wrs/Wrs_2021_Priors.csv") %>%
  select(Name, ReVOA) %>%
  mutate(Season = 2021)
Wrs_rolling_prior_20 <- read_csv("/Users/dylanmervis/Desktop/Priors/Wrs/Wrs_2020_Priors.csv") %>%
  select(Name, ReVOA) %>%
  mutate(Season = 2020)
Wrs_rolling_prior_19 <- read_csv("/Users/dylanmervis/Desktop/Priors/Wrs/Wrs_2019_Priors.csv") %>%
  select(Name, ReVOA) %>%
  mutate(Season = 2019)
Wrs_rolling_prior_18 <- read_csv("/Users/dylanmervis/Desktop/Priors/Wrs/Wrs_2018_Priors.csv") %>%
  select(Name, ReVOA) %>%
  mutate(Season = 2018)
Wrs_rolling_prior_17 <- read_csv("/Users/dylanmervis/Desktop/Priors/Wrs/Wrs_2017_Priors.csv") %>%
  select(Name, ReVOA) %>%
  mutate(Season = 2017)
Wrs_rolling_prior_16 <- read_csv("/Users/dylanmervis/Desktop/Priors/Wrs/Wrs_2016_Priors.csv") %>%
  select(Name, ReVOA) %>%
  mutate(Season = 2016)
Wrs_rolling_prior_15 <- read_csv("/Users/dylanmervis/Desktop/Priors/Wrs/Wrs_2015_Priors.csv") %>%
  select(Name, ReVOA) %>%
  mutate(Season = 2015)
Wrs_rolling_prior_14 <- read_csv("/Users/dylanmervis/Desktop/Priors/Wrs/Wrs_2014_Priors.csv") %>%
  select(Name, ReVOA) %>%
  mutate(Season = 2014)
Wrs_rolling_prior_13 <- read_csv("/Users/dylanmervis/Desktop/Priors/Wrs/Wrs_2013_Priors.csv") %>%
  select(Name, ReVOA) %>%
  mutate(Season = 2013)
Wrs_rolling_prior_12 <- read_csv("/Users/dylanmervis/Desktop/Priors/Wrs/Wrs_2012_Priors.csv") %>%
  select(Name, ReVOA) %>%
  mutate(Season = 2012)
Wrs_rolling_prior_11 <- read_csv("/Users/dylanmervis/Desktop/Priors/Wrs/Wrs_2011_Priors.csv") %>%
  select(Name, ReVOA) %>%
  mutate(Season = 2011)
Wrs_rolling_prior_10 <- read_csv("/Users/dylanmervis/Desktop/Priors/Wrs/Wrs_2010_Priors.csv") %>%
  select(Name, ReVOA) %>%
  mutate(Season = 2010)
Wrs_rolling_prior_09 <- read_csv("/Users/dylanmervis/Desktop/Priors/Wrs/Wrs_2009_Priors.csv") %>%
  select(Name, ReVOA) %>%
  mutate(Season = 2009)
Wrs_rolling_prior_08 <- read_csv("/Users/dylanmervis/Desktop/Priors/Wrs/Wrs_2008_Priors.csv") %>%
  select(Name, ReVOA) %>%
  mutate(Season = 2008)
Wrs_rolling_prior_07 <- read_csv("/Users/dylanmervis/Desktop/Priors/Wrs/Wrs_2007_Priors.csv") %>%
  select(Name, ReVOA) %>%
  mutate(Season = 2007)


Wrs_Priors <- rbind(Wrs_rolling_prior_07, Wrs_rolling_prior_08,
                    Wrs_rolling_prior_09, Wrs_rolling_prior_10, Wrs_rolling_prior_11,
                    Wrs_rolling_prior_12, Wrs_rolling_prior_13, Wrs_rolling_prior_14,
                    Wrs_rolling_prior_15, Wrs_rolling_prior_16, Wrs_rolling_prior_17,
                    Wrs_rolling_prior_18, Wrs_rolling_prior_19, Wrs_rolling_prior_20,
                    Wrs_rolling_prior_21) %>%
  rename(Receiving_Weight = ReVOA)




Rbs_rolling_prior_21 <- read_csv("/Users/dylanmervis/Desktop/Priors/Rbs/Rbs_2021_Priors.csv") %>%
  select(Name, VOA) %>%
  mutate(Season = 2021)
Rbs_rolling_prior_20 <- read_csv("/Users/dylanmervis/Desktop/Priors/Rbs/Rbs_2020_Priors.csv") %>%
  select(Name, VOA) %>%
  mutate(Season = 2020)
Rbs_rolling_prior_19 <- read_csv("/Users/dylanmervis/Desktop/Priors/Rbs/Rbs_2019_Priors.csv") %>%
  select(Name, VOA) %>%
  mutate(Season = 2019)
Rbs_rolling_prior_18 <- read_csv("/Users/dylanmervis/Desktop/Priors/Rbs/Rbs_2018_Priors.csv") %>%
  select(Name, VOA) %>%
  mutate(Season = 2018)
Rbs_rolling_prior_17 <- read_csv("/Users/dylanmervis/Desktop/Priors/Rbs/Rbs_2017_Priors.csv") %>%
  select(Name, VOA) %>%
  mutate(Season = 2017)
Rbs_rolling_prior_16 <- read_csv("/Users/dylanmervis/Desktop/Priors/Rbs/Rbs_2016_Priors.csv") %>%
  select(Name, VOA) %>%
  mutate(Season = 2016)
Rbs_rolling_prior_15 <- read_csv("/Users/dylanmervis/Desktop/Priors/Rbs/Rbs_2015_Priors.csv") %>%
  select(Name, VOA) %>%
  mutate(Season = 2015)
Rbs_rolling_prior_14 <- read_csv("/Users/dylanmervis/Desktop/Priors/Rbs/Rbs_2014_Priors.csv") %>%
  select(Name, VOA) %>%
  mutate(Season = 2014)
Rbs_rolling_prior_13 <- read_csv("/Users/dylanmervis/Desktop/Priors/Rbs/Rbs_2013_Priors.csv") %>%
  select(Name, VOA) %>%
  mutate(Season = 2013)
Rbs_rolling_prior_12 <- read_csv("/Users/dylanmervis/Desktop/Priors/Rbs/Rbs_2012_Priors.csv") %>%
  select(Name, VOA) %>%
  mutate(Season = 2012)
Rbs_rolling_prior_11 <- read_csv("/Users/dylanmervis/Desktop/Priors/Rbs/Rbs_2011_Priors.csv") %>%
  select(Name, VOA) %>%
  mutate(Season = 2011)
Rbs_rolling_prior_10 <- read_csv("/Users/dylanmervis/Desktop/Priors/Rbs/Rbs_2010_Priors.csv") %>%
  select(Name, VOA) %>%
  mutate(Season = 2010)
Rbs_rolling_prior_09 <- read_csv("/Users/dylanmervis/Desktop/Priors/Rbs/Rbs_2009_Priors.csv") %>%
  select(Name, VOA) %>%
  mutate(Season = 2009)
Rbs_rolling_prior_08 <- read_csv("/Users/dylanmervis/Desktop/Priors/Rbs/Rbs_2008_Priors.csv") %>%
  select(Name, VOA) %>%
  mutate(Season = 2008)
Rbs_rolling_prior_07 <- read_csv("/Users/dylanmervis/Desktop/Priors/Rbs/Rbs_2007_Priors.csv") %>%
  select(Name, VOA) %>%
  mutate(Season = 2007)


Rbs_Priors <- rbind(Rbs_rolling_prior_07, Rbs_rolling_prior_08,
                    Rbs_rolling_prior_09, Rbs_rolling_prior_10, Rbs_rolling_prior_11,
                    Rbs_rolling_prior_12, Rbs_rolling_prior_13, Rbs_rolling_prior_14,
                    Rbs_rolling_prior_15, Rbs_rolling_prior_16, Rbs_rolling_prior_17,
                    Rbs_rolling_prior_18, Rbs_rolling_prior_19, Rbs_rolling_prior_20,
                    Rbs_rolling_prior_21) %>%
  rename(Rush_Weight = VOA) 

