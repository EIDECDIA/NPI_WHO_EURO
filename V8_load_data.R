
# Script to load data & functions for analysis 

# load required packages and functions
pacman::p_load(tidyverse,
               here, 
               plm, 
               countrycode, 
               magrittr, 
               zoo, 
               ggdendro, 
               cowplot, 
               lubridate,
               data.table,
               pvclust,
               e1071,
               ggdendro,
               progress,
               ggsci,
               gridExtra,
               ggh4x,
               # covidregionaldata,
               MMWRweek,
               imputeTS,
               sf)
#remotes::install_github("epiforecasts/covidregionaldata")
require(covidregionaldata)

# Custom functions for regression exercise
source(here("aicbic_plm.R"))
source(here("select_variable.R"))
source(here("find_lag.R"))
source(here("plot_all_lags.R"))
source(here("aicbic_select.R"))

# Load full data for analysis 
# joined <- readRDS("data/joined_all_V8.RDS")
# joined <- readRDS("data/joined_V9.rds")

#~#~# The following script creates the data used in the analysis (joined_all_V8.RDS) #~#~#

# Only run the following script to see how it was compiled

oxford_data       <- read_csv("data/NPI_OX.csv")    # Oxford government response tracker data for NPIs/PHSMs
oxford_data_latest <- read_rds("data/oxford_data_latest.rds")
vaccine_data_owin <- read_csv("data/VAC_OWIN_v2.csv")  # Vaccine coverage data, calculated from script - 
rt_estimates      <- read_csv("data/rt_EURO.csv")   # Rt estimate data, calculated from script - 
country_index <- data.frame(CountryName = c("Albania","Andorra","Armenia","Austria","Azerbaijan",
                                            "Belarus","Belgium","Bosnia & Herzegovina","Bulgaria","Croatia",
                                            "Cyprus","Czechia","Denmark","Estonia","Finland","France",
                                            "Georgia","Germany","Greece","Hungary","Iceland","Ireland",
                                            "Israel","Italy","Kazakhstan","Kyrgyzstan","Latvia","Lithuania",
                                            "Luxembourg","Malta","Moldova","Monaco","Montenegro",
                                            "Netherlands","North Macedonia","Norway","Poland","Portugal",
                                            "Romania","Russia","San Marino","Serbia","Slovakia","Slovenia",
                                            "Spain","Sweden","Switzerland","Tajikistan","Turkey","Ukraine",
                                            "United Kingdom","Uzbekistan")) %>% 
  mutate(iso3c = countrycode::countrycode(CountryName,"country.name","iso3c"))

# Build policy_dic, lookup tibble of policy codes and names 
policy_dic <- colnames(oxford_data) %>% 
  .[grep(paste(c(paste0("C",1:8),
                 paste0("E",1:2),
                 paste0("H",1:6)), collapse = "|"), .,ignore.case = F)] %>% 
  str_split(pattern = "_") %>% 
  do.call("rbind",.) %>% 
  as_tibble %>% 
  setNames(c("policy_code","policy_name")) %>% 
  mutate(policy_max = c(3, 3, 2, 4, 2, 3, 2, 4,
                        2, 2,
                        2, 3, 2, 4)) %>% 
  mutate(cat = case_when(policy_code %in% 
                           paste0("C",1:8) ~ "Closure & Containment  ",
                         # policy_code == "C8" ~ "Intl Travel Restriction  ",
                         policy_code %in%
                           paste0("E", 1:4) ~ "Economic Response  ",
                         policy_code %in%
                           paste0("H", 1:6) ~ "Public Health & Health System Response  "),
         lab = gsub("\\.", " ", policy_name))#

policy_dic_V <- policy_dic %>% 
  add_row(policy_code = "V_all_adj", 
          policy_name = "Vaccine coverage - all population", 
          policy_max = 1, 
          cat = "Vaccination ", 
          lab = "Vaccine coverage - all population")

# Build policy_data
policy_data <- oxford_data_latest
# policy_data <-  oxford_data %>%
#   select(country, cnt, date, contains("_")) %>%
#   pivot_longer(cols = contains("_"), names_to = "policy_name") %>%
#   mutate(policy_name = str_replace(policy_name, "_.*", ""))

# check for missing data
# for(i in policy_dic$policy_code){
#   policy_data %>% 
#     filter(policy_name == i) %>% 
#     ggplot(., aes(x = date, y = value)) +
#     geom_point() +
#     geom_line() +
#     facet_wrap(~country) -> p
#   
#   ggsave(paste0("figs/intermediate/policy_raw/",i,"_new2.png"),
#          p)
# }
# 
# policy_data %>% 
#   filter(is.na(value)) %>% 
#   group_by(country, policy_name) %>% tally %>% 
#   arrange(desc(n))

# Impute missing data
# policy_data_1 <-  policy_data %>% 
#   group_by(cnt, policy_name) %>% 
#   arrange(date) %>% 
#   nest() %>% 
#   mutate(missing = map(data, ~any(is.na(.x$value))) %>% unlist,
#          all = map(data, ~all(is.na(.x$value))) %>% unlist) %>% 
#   group_by(missing) %>% 
#   group_split()
# 
# policy_data_2 <-  policy_data_1[[2]] %>% 
#   mutate(data = map(data, arrange, date),
#          # missingness at the tails are replaced by the last or the next 
#          # non-missing values; missingness not at the tails are replaced
#          # based on linear interpolation.
#          data = map(data, mutate, 
#                     value = imputeTS::na_interpolation(value))) %>% 
#   bind_rows(policy_data_1[[1]]) %>% 
#   dplyr::select(-c(missing, all)) %>% 
#   unnest(cols = data)

# set maximum and minimum values
# policy_data_3 <-  policy_data_2 %>% 
#   group_by(policy_name) %>% 
#   summarise(policy_value_UL = max(value, na.rm = T)) %>% 
#   right_join(policy_data_2, by = "policy_name") %>% 
#   arrange(cnt, policy_name) %>% 
#   mutate(policy_value_LL = 0) %>% 
#   select(country, cnt, date, policy_name, value, policy_value_LL, policy_value_UL) %>% 
#   mutate(policy_max = if_else(value < policy_value_UL, 0, 1)) %>% 
#   mutate(policy_any = if_else(value > 0, 1, 0)) %>% 
#   mutate(country = if_else(country == "Bosnia and Herzegovina", "Bosnia & Herzegovina", country))

policy_data_3 <-  policy_data %>% 
  group_by(policy_name) %>% 
  summarise(policy_value_UL = max(value, na.rm = T)) %>% 
  right_join(policy_data, by = "policy_name") %>% 
  arrange(cnt, policy_name) %>% 
  mutate(policy_value_LL = 0) %>% 
  select(country, cnt, date, policy_name, value, policy_value_LL, policy_value_UL) %>% 
  mutate(policy_max = if_else(value < policy_value_UL, 0, 1)) %>% 
  mutate(policy_any = if_else(value > policy_value_LL, 1, 0)) %>% 
  mutate(country = if_else(country == "Bosnia and Herzegovina", "Bosnia & Herzegovina", country))

# Build other data sets
rt_estimates <-  rt_estimates %>% 
  mutate(cnt = countrycode(country, "country.name", "iso3c"),
         date = lubridate::ymd(date)) %>%
  as_tibble() %>% 
  select(-country)

vaccine_data_join <- vaccine_data_owin %>% 
  select(-country)

# Total country in WHO Europe region, n = 53
# Check to see if these can be resolved later 

unique(policy_data_3$country) # n = 49,  rm(Turkmenistan, Armenia, Montenegro & North Macedonia)
unique(rt_estimates$country) # n = 52, rm(Turkmenistan)
unique(vaccine_data_owin$country)# n = 52, rm(Turkmenistan)

# Pivot to wide datasets and set factors

# Any Efforts
joined_any <- policy_data_3 %>%
  dplyr::select(-c(policy_max,
                   policy_value_LL,
                   policy_value_UL,
                   value)) %>%
  rename(value = policy_any) %>%
  pivot_wider(names_from = policy_name, values_from = value) %>%
  group_by(cnt, date) %>%
  left_join(vaccine_data_join,
            by = c("cnt", "date")) %>%
  left_join(rt_estimates %>%
              dplyr::select(-X1),
            by = c("cnt", "date")) %>%
  ungroup()
# 
# 
# # Continuous Efforts
joined_con <- policy_data_3 %>%
  dplyr::select(-c(policy_max,
                   policy_value_LL,
                   policy_any)) %>%
  mutate(value = value/policy_value_UL) %>%
  select(-policy_value_UL) %>%
  pivot_wider(names_from = policy_name, values_from = value) %>%
  group_by(cnt, date) %>%
  left_join(vaccine_data_join,
            by = c("cnt", "date")) %>%
  left_join(rt_estimates %>%
              dplyr::select(-X1),
            by = c("cnt", "date")) %>%
  ungroup()
# 
# # Max effort
joined_max <- policy_data_3 %>%
  dplyr::select(-c(policy_value_UL,
                   policy_value_LL,
                   policy_any)) %>%
  mutate(value = policy_max) %>%
  select(-policy_max) %>%
  pivot_wider(names_from = policy_name, values_from = value) %>%
  group_by(cnt, date) %>%
  left_join(vaccine_data_join,
            by = c("cnt", "date")) %>%
  left_join(rt_estimates %>%
              dplyr::select(-X1),
            by = c("cnt", "date")) %>%
  ungroup()
# 
# #check if things makes sense
# joined_any %>%
#   filter(country == "Italy") %>% 
#   .[1:4] %>% 
#   rename(C1_any = C1) %>% 
#   bind_cols(joined_con %>%
#               filter(country == "Italy") %>% 
#               .[4]) %>% rename(C1_con = C1) %>% 
#   bind_cols(joined_max %>%
#               filter(country == "Italy") %>% 
#               .[4]) %>% rename(C1_max = C1) %>% 
#   pivot_longer(starts_with("C1")) %>% 
#   ggplot(., aes(x = date, y = value, color = name)) +
#   geom_point() +
#   facet_wrap(~name,ncol = 1)
# 
# #~#~#~#~#~#~#~#~#~##~#~#~#~##~#~#~#~#~#~#~#~#
# ### Sensitivity analysis scenarios mid effort
# #~#~#~#~##~#~#~#~##~#~#~#~##~#~#~#~#~#~#~#~#
scenarios <- read_csv("data/milestones.csv") %>%
  mutate(scenario = paste0("s",scenario)) %>%
  unite("tag",metric,scenario,phase,remove = F)

joined <- list()

for(i in 1:nrow(scenarios)){
  if(scenarios$metric[i] == "any") tmp <- joined_any
  if(scenarios$metric[i] == "con") tmp <- joined_con
  if(scenarios$metric[i] == "max") tmp <- joined_max

  joined[[scenarios$tag[i]]] <- tmp %>%
    ungroup %>%
    filter(date > scenarios$start[i],
           date <= scenarios$end[i])

}
# 
write_rds(joined, "data/joined_v9.rds")

##### Full TS

# joined_mid_full <- joined_mid %>% 
#   ungroup()
# 
# # S1 - Scaled effort, Wild virus 
# joined_mid_S1_W <- joined_mid %>% 
#   ungroup() %>% 
#   filter(date <= as.Date("2020-11-22"))
# 
# # S1 - Scaled effort, Alpha virus 
# joined_mid_S1_A <- joined_mid %>% 
#   ungroup() %>% 
#   filter(date > as.Date("2020-11-22")) %>% 
#   filter(date <= as.Date("2021-05-10"))
# 
# # S1 - Scaled effort, Delta virus 
# joined_mid_S1_D <- joined_mid %>% 
#   ungroup() %>% 
#   filter(date > as.Date("2021-05-10")) %>% 
#   filter(date <= as.Date("2021-09-30"))
# 
# 
# #### Sensitivity analysis scenario 2
# # S2 - Scaled effort, Wild virus 
# joined_mid_S2_W <- joined_mid %>% 
#   ungroup() %>% 
#   filter(date <= as.Date("2020-12-11"))
# 
# # S2 - Scaled effort, Alpha virus 
# joined_mid_S2_A <- joined_mid %>% 
#   ungroup() %>% 
#   filter(date > as.Date("2020-12-11")) %>% 
#   filter(date <= as.Date("2021-05-19"))
# 
# # S2 - Scaled effort, Delta virus 
# joined_mid_S2_D <- joined_mid %>% 
#   ungroup() %>% 
#   filter(date > as.Date("2021-05-19")) %>% 
#   filter(date <= as.Date("2021-09-30"))
# 
# 
# #### Sensitivity analysis scenario 3
# # S3 - Scaled effort, Wild virus 
# joined_mid_S3_W <- joined_mid %>% 
#   ungroup() %>% 
#   filter(date <= as.Date("2020-12-25"))
# 
# # S3 - Scaled effort, Alpha virus 
# joined_mid_S3_A <- joined_mid %>% 
#   ungroup() %>% 
#   filter(date > as.Date("2020-12-25")) %>% 
#   filter(date <= as.Date("2021-05-31"))
# 
# # S3 - Scaled effort, Delta virus 
# joined_mid_S3_D <- joined_mid %>% 
#   ungroup() %>% 
#   filter(date > as.Date("2021-05-31")) %>% 
#   filter(date <= as.Date("2021-09-30"))
# 
# 
# #### Sensitivity analysis scenario 4
# # S4 - Scaled effort, Wild virus 
# joined_mid_S4_W <- joined_mid %>% 
#   ungroup() %>% 
#   filter(date <= as.Date("2021-01-23"))
# 
# # S4 - Scaled effort, Alpha virus 
# joined_mid_S4_A <- joined_mid %>% 
#   ungroup() %>% 
#   filter(date > as.Date("2021-01-23")) %>% 
#   filter(date <= as.Date("2021-06-13"))
# 
# # S4 - Scaled effort, Delta virus 
# joined_mid_S4_D <- joined_mid %>% 
#   ungroup() %>% 
#   filter(date > as.Date("2021-06-13")) %>% 
#   filter(date <= as.Date("2021-09-30"))
# 
# 
# #~#~#~#~#~#~#~#~#~##~#~#~#~##~#~#~#~#~#~#~#~#
# ### Sensitivity analysis scenarios max effort
# #~#~#~#~##~#~#~#~##~#~#~#~##~#~#~#~#~#~#~#~#
# 
# ##### Full TS
# 
# joined_max_full <- joined_max %>% 
#   ungroup()
# 
# # S1 - Max effort, Wild virus 
# joined_max_S1_W <- joined_max %>% 
#   ungroup() %>% 
#   filter(date <= as.Date("2020-11-22"))
# 
# # S1 - Max effort, Alpha virus 
# joined_max_S1_A <- joined_max %>% 
#   ungroup() %>% 
#   filter(date > as.Date("2020-11-22")) %>% 
#   filter(date <= as.Date("2021-05-10"))
# 
# # S1 - Max effort, Delta virus 
# joined_max_S1_D <- joined_max %>% 
#   ungroup() %>% 
#   filter(date > as.Date("2021-05-10")) %>% 
#   filter(date <= as.Date("2021-09-30"))
# 
# 
# #### Sensitivity analysis scenario 2
# # S2 - Max effort, Wild virus 
# joined_max_S2_W <- joined_max %>% 
#   ungroup() %>% 
#   filter(date <= as.Date("2020-12-11"))
# 
# # S2 - Max effort, Alpha virus 
# joined_max_S2_A <- joined_max %>% 
#   ungroup() %>% 
#   filter(date > as.Date("2020-12-11")) %>% 
#   filter(date <= as.Date("2021-05-19"))
# 
# # S2 - Max effort, Delta virus 
# joined_max_S2_D <- joined_max %>% 
#   ungroup() %>% 
#   filter(date > as.Date("2021-05-19")) %>% 
#   filter(date <= as.Date("2021-09-30"))
# 
# 
# #### Sensitivity analysis scenario 3
# # S3 - Max effort, Wild virus 
# joined_max_S3_W <- joined_max %>% 
#   ungroup() %>% 
#   filter(date <= as.Date("2020-12-25"))
# 
# # S3 - Max effort, Alpha virus 
# joined_max_S3_A <- joined_max %>% 
#   ungroup() %>% 
#   filter(date > as.Date("2020-12-25")) %>% 
#   filter(date <= as.Date("2021-05-31"))
# 
# # S3 - Max effort, Delta virus 
# joined_max_S3_D <- joined_max %>% 
#   ungroup() %>% 
#   filter(date > as.Date("2021-05-31")) %>% 
#   filter(date <= as.Date("2021-09-30"))
# 
# 
# #### Sensitivity analysis scenario 4
# # S4 - Max effort, Wild virus 
# joined_max_S4_W <- joined_max %>% 
#   ungroup() %>% 
#   filter(date <= as.Date("2021-01-23"))
# 
# # S4 - Max effort, Alpha virus 
# joined_max_S4_A <- joined_max %>% 
#   ungroup() %>% 
#   filter(date > as.Date("2021-01-23")) %>% 
#   filter(date <= as.Date("2021-06-13"))
# 
# # S4 - Max effort, Delta virus 
# joined_max_S4_D <- joined_max %>% 
#   ungroup() %>% 
#   filter(date > as.Date("2021-06-13")) %>% 
#   filter(date <= as.Date("2021-09-30"))

# Save RDS of all scenarios
# saveRDS(list(s1_full_mid = joined_mid_full,
#              
#              s1_W_mid = joined_mid_S1_W,
#              s1_A_mid = joined_mid_S1_A,
#              s1_D_mid = joined_mid_S1_D,
#              
#              s2_W_mid = joined_mid_S2_W,
#              s2_A_mid = joined_mid_S2_A,
#              s2_D_mid = joined_mid_S2_D,
#              
#              s3_W_mid = joined_mid_S3_W,
#              s3_A_mid = joined_mid_S3_A,
#              s3_D_mid = joined_mid_S3_D,
#              
#              s4_W_mid = joined_mid_S4_W,
#              s4_A_mid = joined_mid_S4_A,
#              s4_D_mid = joined_mid_S4_D,
#              
#              
#              s1_full_max = joined_max_full,
#              
#              s1_W_max = joined_max_S1_W,
#              s1_A_max = joined_max_S1_A,
#              s1_D_max = joined_max_S1_D,
#              
#              s2_W_max = joined_max_S2_W,
#              s2_A_max = joined_max_S2_A,
#              s2_D_max = joined_max_S2_D,
#              
#              s3_W_max = joined_max_S3_W,
#              s3_A_max = joined_max_S3_A,
#              s3_D_max = joined_max_S3_D,
#              
#              s4_W_max = joined_max_S4_W,
#              s4_A_max = joined_max_S4_A,
#              s4_D_max = joined_max_S4_D,
#              
#              policy_dic = policy_dic_V), 
#         here("data", "joined_all_V8.RDS"))

# vaccine data
# vaccine_data_owin %>% 
#   ggplot(., aes(x = date, y = V_all_adj)) +
#   geom_line() +
#   facet_wrap(~country)
