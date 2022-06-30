
# Script to investigate if PHSM presence is associated with vaccine uptake

#Load in data
joined <- read_rds("data/joined_v9.rds")

# Label data 
lab <- readRDS("data/joined_all_V8.RDS")$policy_dic %>% 
  mutate(cat = case_when(policy_code %in% 
                           paste0("C",1:8) ~ "Closure & Containment  ",
                         policy_code %in%
                           paste0("E", 1:4) ~ "Economic Response  ",
                         policy_code %in%
                           paste0("H", 1:6) ~ "Public Health & Health System Response  "),
         lab = gsub("\\.", " ", policy_name)) %>% 
  mutate(lab = factor(lab, levels = lab))


#~#~# All countries continuous  effort, facet by PHSM #~#~# 

effort_val <- c("con_s0_full", "con_s1_A","con_s1_D")

# Function plot all countries together
plot_PHSM_x_VAC <- function(i){

test_data <- joined[[i]] %>% 
  mutate(period = i) %>% 
  mutate(period = case_when(grepl("s0_full", period) ~ "Full_TS", 
                                grepl("s1_A", period) ~ "Alpha",
                                grepl("s1_D", period) ~ "Delta")) %>% 
  replace_na(list(V_all_adj = 0)) %>% 
  select(cnt, npi_val, V_all_adj, period) %>% 
  pivot_longer(cols = npi_val, names_to = "NPI") %>% 
  left_join(lab, by = c("NPI" = "policy_code")) 

test_data %>% 
  ggplot(aes(x= V_all_adj, y= value))+
  geom_jitter(size = 0.1, alpha = 0.1)+
  geom_smooth(method = "lm", linetype = "dashed") +
  facet_wrap(~lab, nrow = 2, ncol = 7, labeller = label_wrap_gen(multi_line = T, width = 12))+
  labs(y = "PHSM implementation strength", 
       x = "Vaccine uptake proportion",
       title = test_data %>% 
         pull(period) %>% 
         first())+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 12),
        strip.background = element_rect(fill = NA),
        axis.text.x = element_text(vjust = 0.5,
                                   angle = 90,
                                   hjust = 1),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 8),
        strip.text = element_text(size = 8))

ggsave(filename = paste0("figs/PHSM_x_VAC/all_cnt/",  test_data %>% 
                           pull(period) %>% 
                           first(), "_cont.png"),
       plot = last_plot(),
       width = 12,
       height = 6)

}

# Map function
map(effort_val, plot_PHSM_x_VAC)



#~#~# All countries continuous effort by individual PHSM and facet by country #~#~# 
# PHSM code
npi_val <- c("C1","C2","C3","C4","C5","C6","C7","C8","E1","E2","H1","H2","H3","H6") 

# By PHSM and country
plot_PHSM_x_VAC_cnt <- function(i){
  
  test_data <- joined[["con_s0_full"]] %>% 
    replace_na(list(V_all_adj = 0)) %>% 
    select(country, cnt, npi_val, V_all_adj) %>% 
    pivot_longer(cols = npi_val, names_to = "NPI") %>% 
    left_join(lab, by = c("NPI" = "policy_code")) %>% 
    filter(NPI == i)
  
  test_data %>% 
    ggplot(aes(x= V_all_adj, y= value))+
    geom_jitter(size = 0.1, alpha = 0.1)+
    geom_smooth(method = "lm", linetype = "dashed", size = 0.5) +
    facet_wrap(~country, nrow = 7, ncol = 7)+
    labs(y = "PHSM implementation strength", 
         x = "Vaccine uptake proportion",
         title = paste0(test_data %>% 
                          pull(NPI) %>% 
                          first(), " - ", test_data %>% 
                          pull(lab) %>% 
                          first()))+
    theme_bw()+
    theme(panel.grid = element_blank(),
          legend.position = "bottom",
          legend.title = element_text(size = 12),
          strip.background = element_rect(fill = NA),
          axis.text.x = element_text(vjust = 0.5,
                                     angle = 90,
                                     hjust = 1),
          axis.text = element_text(size = 6),
          axis.title = element_text(size = 10),
          legend.text = element_text(size = 8),
          strip.text = element_text(size = 5))
  
  ggsave(filename = paste0("figs/PHSM_x_VAC/all_PHSM/", test_data %>% 
                             pull(NPI) %>% 
                             first(), "_cont.png"),
         plot = last_plot(),
         width = 8,
         height = 8)
  
}

# Map function
map(npi_val, plot_PHSM_x_VAC_cnt)



#~#~# Binary PHSM scenarios #~#~# 

# Any effort and max effort scenarios :
# Combinations of scenarios
effort_val <- c("max_s0_full", "any_s0_full")
npi_val <- c("C1","C2","C3","C4","C5","C6","C7","C8","E1","E2","H1","H2","H3","H6") 
npi_on <- c("1", "0")

# All combination of parameters
parameter_df <- crossing(effort_val, npi_val, npi_on) %>% 
  rowid_to_column(var = "scenario") %>% 
  nest(-scenario)

# Function to calc diff between mean vaccine uptake in presence/absence of individual PHSMs 
test_func <- function(data){
  
  test_data <- joined[[data$effort_val]] %>% 
    select(cnt, date, data$npi_val, V_all_adj) %>% 
    replace_na(list(V_all_adj = 0)) %>% 
    select(NPI = 3, 4) %>% 
    mutate(NPI = as.character(NPI))
  
  # Calc mean and 95% CI for each group
  calc_ci <- test_data %>%
    filter(NPI == data$npi_on) %>% 
    summarise(mean = mean(V_all_adj, na.rm = TRUE),
              sd = sd(V_all_adj, na.rm = TRUE),
              n = n()) %>%
    mutate(se = sd/ sqrt(n),
           lower_ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
           upper_ci = mean + qt(1 - (0.05 / 2), n - 1) * se) %>% 
    select(mean, lower_ci, upper_ci) %>% 
    as_vector() 
  
  # T-test
  t_test_res <- t.test(data = test_data, V_all_adj ~ NPI)
  
  output <- tibble(Effort = data$effort_val,
                   NPI_code = data$npi_val,
                   NPI_ON = data$npi_on,
                   NPI_mean = calc_ci[1],
                   NPI_mean_low = calc_ci[2],
                   NPI_mean_high = calc_ci[3],
                   pval = t_test_res$p.value) 
  
  return(output)
  
}

# Raw result for each combination
full_result <- parameter_df %>% 
  mutate(result = map(.x = data, .f = test_func)) %>% 
  unnest_wider(result) %>% 
  select(-data) %>% 
  unnest()

# Add labs
full_result_labs <- full_result %>% 
  mutate(sig = if_else(pval >= 0.05, "Not significant", "Significant")) %>% 
  mutate(sig = factor(sig, levels = c("Significant", "Not significant"))) %>% 
  left_join(lab, by = c("NPI_code" = "policy_code"))  %>% 
  mutate(effort_lab = case_when(grepl("any", Effort) ~ "Any Effort", 
                                grepl("max", Effort) ~ "Max. Effort")) %>% 
  mutate(PHSM_lab = case_when(grepl("1", NPI_ON) ~ "Active", 
                              grepl("0", NPI_ON) ~ "Inactive"))

# Plot as point range 
full_result_labs %>% 
  ggplot()+
  geom_pointrange(aes(x = PHSM_lab, y = NPI_mean, ymin = NPI_mean_low, ymax = NPI_mean_high, color = PHSM_lab, shape = sig), size = 0.5)+
  scale_shape_manual(values = c(16, 1)) +
  ggh4x::facet_nested(effort_lab ~  lab,
                      labeller = label_wrap_gen(multi_line = T,
                                                width = 12))+
  labs(y = "Mean vaccine uptake proportion", 
       x = "PHSM presence",
       color = "",
       shape = "",)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 12),
        strip.background = element_rect(fill = NA),
        axis.text.x = element_text(vjust = 0.5,
                                   angle = 90,
                                   hjust = 1),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 8),
        strip.text = element_text(size = 8))

ggsave(filename = "figs/PHSM_x_VAC/Binary_PHSM/binary.png",
              plot = last_plot(),
              width = 12,
              height = 6)
