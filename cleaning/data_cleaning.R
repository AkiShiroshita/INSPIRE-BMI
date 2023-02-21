packages = c("devtools",
             "usethis",
             "here",
             "readr",
             "readxl",
             "expss",
             "tidyverse",
             "lubridate")

package.check <- lapply(packages, FUN = function(x){
  if (!require(x, character.only = TRUE)){
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

df <- read_csv(here("input/analysis_data.csv"))

dataA <- read_csv("input/20230118AkiAtopicAsthmaRequest_A.csv") %>% 
  dplyr::select(cfsubjid, maternalBMIAtdelivery_dev, maternalBMIBeforPreg_dev,
                pregnancyWgtGain_dev, momsmoke_ef_dev)
dataB <- read_csv("input/20230118AkiAtopicAsthmaRequest_B.csv")

df <- left_join(df, dataA, by = "cfsubjid")

df <- df %>% 
  mutate(enroll_age = trunc(time_length(interval(INFANTDOB, STUDYENROLLDATE), "month")),
         other_sib_cat = case_when(OTHERLIVSIBS_BIO + OTHERLIVSIBS_HAL == 0 ~ 0,
                                   OTHERLIVSIBS_BIO + OTHERLIVSIBS_HAL == 1 ~ 1,
                                   1 < OTHERLIVSIBS_BIO + OTHERLIVSIBS_HAL ~ 2),
         MAT_EDUC_cat = case_when(MAT_EDUCYEARS < 9 ~ 0,
                                  9 <= MAT_EDUCYEARS & MAT_EDUCYEARS <= 12 ~ 1,
                                  12 < MAT_EDUCYEARS ~ 2))

df %>% write_csv(here("output/analysis_data_bmi.csv"))

# memo
df <- read_csv(here("output/analysis_data_bmi.csv"))
df %>% glimpse()
df %>% filter(!is.na(BSS2_dev) & oy_rsv_infected_dev == 1)
df %>% filter(!is.na(BSS2_dev))
