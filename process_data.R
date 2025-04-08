library(rvest)
library(httr)
library(tidyverse)
library(jsonlite)

source("utility_tables.R")


faa_data <- readRDS("./faa_data.rds")
print("Read faa RDS Complete")

smy_date_ctl <- faa_data %>% 
                  mutate(Date = as.Date(Send_Date_EST)) %>%
                  group_by(Date, Control_Code) %>%
                  summarise(Count = n(), .groups = "drop")

# get name for Control center
smy_date_ctl <- smy_date_ctl %>% 
  left_join(artcc_table %>% select(`Facility[3]`, Name),
            by=join_by(Control_Code == `Facility[3]`)) %>%
  mutate(Name = case_when(
    Control_Code=="CZE" ~ "Edmonton Center",
    Control_Code=="CZU" ~ "Montreal Center",
    Control_Code=="CZV" ~ "Vancouver Center",
    Control_Code=="CZW" ~ "Winnipeg Center",
    Control_Code=="CZY" ~ "Toronto Center",
    Control_Code=="DCC" ~ "FAA ATCSCC",
    Control_Code=="other entity" ~ "Other Entity",
    TRUE ~ Name
  ))

print("smy_date_ctl Complete")

airport_info <- read.csv("./airports-code@public.csv", header=T, sep=";")

smy_date_airport <- faa_data %>% 
                      mutate(Date = as.Date(Send_Date_EST)) %>%
                      group_by(Date, Airport_Code) %>%
                      summarise(Count = n(), .groups = "drop") %>%
                      mutate(Airport_Code = case_when(
                        nchar(Airport_Code)==4 ~ str_sub(Airport_Code, 2, 4),
                        TRUE ~ Airport_Code
                      )) %>%
                      left_join(airport_info%>%select(Airport.Code, Airport.Name),
                                by=join_by(Airport_Code==Airport.Code)) %>%
                      mutate(Airport.Name = case_when(
                        Airport_Code=="DCC" ~ "FAA ATCSCC",
                        Airport_Code=="other entity" ~ "Other Entity",
                        TRUE ~ Airport.Name
                      ))



print("smy_date_airport Complete")

smy_categ_ct <- faa_data %>%
                    mutate(Type = str_match(Classification, "\\[(.*?)\\]")[,2]) %>%
                    group_by(Month, Type) %>%
                    summarise(Count = n(), .groups = "drop")

print("smy_categ_ct Complete")

avg_eff_time <- faa_data %>%
                  mutate(Type = str_match(Classification, "\\[(.*?)\\]")[,2]) %>%
                  group_by(Month, Type) %>%
                  summarise(Mean_Effect_Duration = mean(Eff_Duration_Minutes, na.rm=T),
                            .groups = "drop") %>% drop_na()

print("avg_eff_time Complete")




smy_date_ctl <- smy_date_ctl %>% mutate(
  Name=case_when(
    Control_Code=="Other Entity" ~ "Other Entity",
    TRUE ~ Name
  )
) %>% drop_na()

avg_eff_time <- avg_eff_time %>% mutate(
  Mean_Effect_Duration=abs(Mean_Effect_Duration)
)%>% drop_na()

smy_categ_ct <- smy_categ_ct %>% drop_na()
avg_eff_time <- avg_eff_time %>% drop_na()



saveRDS(smy_date_ctl, "smy_date_ctl.rds")
saveRDS(smy_date_airport, "smy_date_airport.rds")
saveRDS(smy_categ_ct, "smy_categ_ct.rds")
saveRDS(avg_eff_time, "avg_eff_time.rds")