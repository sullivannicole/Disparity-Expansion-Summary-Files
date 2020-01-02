#' Functions to pull variables from ACS summary files for 25 largest CBSAs in US from 2005-2018
#'
#' Using separate 1-year ACS summary files, calculates employment and poverty status by disability
#'
#' @param df dataframe containing 1-year ACS summary file data for a year 2005 or later; in this case from local, custom-processed files in N:/CommDev/Research/Research/Census
#' @param df_name dataframe name
#' 
#' @return dataframe containing employment status and poverty status by disability, employment by age, poverty by age, tenure by age, or housing cost burden by age for 26 US metros for 2005-2018
#' 
#' @examples
#' disability_ls <- purrr::map2(df_list, df_list_names, disability_vars)
#'
#' @export 

disability_vars <- function(df, df_name) {
  
  df_year <- as.numeric(df_name)
  
  if(df_year < 2005) {
    
    print("Not a valid year for the function.  Function current as of January 2020.")
    
  } else if (df_year >= 2009) {
    
    # Years 2009 and later use different table codes than 2007-2008 or 2005-2006
    df_pared <- acs2009 %>%
      dplyr::select(CBSA, starts_with("B18120"), starts_with("B18130")) %>%
      transmute(CBSA = CBSA,
                Employed_disability = B18120e4/(B18120e4 + B18120e13 + B18120e22), # Denom = total population with disability
                Employed_no_disability = B18120e11/(B18120e11 + B18120e20 + B18120e29), # Denom = total population with no disability
                Above_poverty_disability = !! census_sum("B18130e", 5, 40, 7),
                Above_poverty_no_disability = !! census_sum("B18130e", 8, 43, 7),
                Prop_above_poverty_disability = Above_poverty_disability/(Above_poverty_disability + !! census_sum("B18130e", 4, 39, 7)),
                Prop_above_poverty_no_disability = Above_poverty_no_disability/(Above_poverty_no_disability + !! census_sum("B18130e", 7, 42, 7))) %>%
      dplyr::select(-starts_with("Above"))
    
    invisible(df_pared)
    
  } else if (df_year <= 2007 & df_year >= 2005) {
    
    # Years 2005 and 2006
    df_pared <- df %>%
      dplyr::select(CBSA, starts_with("B18020e"), starts_with("B18030")) %>%
      transmute(CBSA = CBSA,
                Employed_disability = (B18020e5 + B18020e8 + B18020e12 + B18020e15)/B18020e2, # Num = males and females of two different age groups that have 1+ disabilities and are employed
                Employed_no_disability = (B18020e20 + B18020e23 + B18020e27 + B18020e30)/B18020e17,
                Prop_above_poverty_disability = (!! census_sum("B18030e", 6, 15, 3) + B18030e19 + B18030e22 + B18030e25 + B18030e28)/B18030e2,
                Prop_above_poverty_no_disability = (!! census_sum("B18030e", 33, 42, 3) + !! census_sum("B18030e", 46, 55, 3))/B18030e29)
    
    invisible(df_pared)
    
  }
  
  else if (df_year == 2008) {
    
    
    df_pared <- df %>%
      dplyr::select(CBSA, starts_with("B18120"), starts_with("B18130")) %>%
      transmute(CBSA = CBSA,
                Employed_disability = (B18120e4 + B18120e11)/(B18120e3 + B18120e10),
                Employed_no_disability = (B18120e7 + B18120e14)/(B18120e6 + B18120e13),
                Above_poverty_disability = !! census_sum("B18130e", 5, 40, 7),
                Above_poverty_no_disability = !! census_sum("B18130e", 8, 43, 7),
                Prop_above_poverty_disability = Above_poverty_disability/(Above_poverty_disability + !! census_sum("B18130e", 4, 39, 7)),
                Prop_above_poverty_no_disability = Above_poverty_no_disability/(Above_poverty_no_disability + !! census_sum("B18130e", 7, 42, 7))) %>%
      dplyr::select(-starts_with("Above"))
    
    invisible(df_pared)
    
  }
  
  else {
    
    print("Missing data for input.")
  }
  
  # Same calculation process for all years
  
  # Employment and poverty status by disability
  df_disparity <- df_pared %>%            
    transmute(CBSA = CBSA,
              Odds_employ_no_disability = Employed_no_disability/(1-Employed_no_disability), # Create odds variable (See "OddsRatioDemo.xlsx for more info")
              Odds_employ_disability = Employed_disability/(1-Employed_disability), # Create odds variable
              Employ_disparity = Odds_employ_disability/Odds_employ_no_disability,
              Odds_no_poverty_no_disability = Prop_above_poverty_no_disability/(1-Prop_above_poverty_no_disability), 
              Odds_no_poverty_disability = Prop_above_poverty_disability/(1-Prop_above_poverty_disability), # Create odds variable
              Poverty_disparity = Odds_no_poverty_disability/Odds_no_poverty_no_disability) %>% # Ratio of odds of being employed (Disability:No disability)
    dplyr::select(-starts_with("Odds")) %>%
    arrange(CBSA) %>% # ex. CBSA 47900 == Index 26
    mutate(Index = dplyr::row_number())
  
  df_employ_pov <- df_pared %>%
    arrange(desc(CBSA)) %>%
    mutate(Index = dplyr::row_number()) # ex. CBSA 47900 == Index 1
  
  
  # Get rank of each column (descending order, largest # = highest rank) --> CBSA_rank 1 is CBSA 47900 (largest #)
  ep_ranks <- as.data.frame(purrr::map(-df_employ_pov, rank))
  colnames(ep_ranks) <- paste(colnames(df_employ_pov), "rank", sep = "_")
  
  ep_full <- full_join(df_employ_pov, ep_ranks, by = c("Index" = "CBSA_rank"))
  
  # Get rank of disparities (ascending/default order, smallest # = worst disparity and highest rank) --> CBSA_rank 1 is CBSA 12060 (smallest #)
  disparity_ranks <- as.data.frame(purrr::map(df_disparity, rank))
  colnames(disparity_ranks) <- paste(colnames(df_disparity), "rank", sep = "_")
  
  disparity_full <- full_join(df_disparity, disparity_ranks, by = c("Index" = "CBSA_rank"))
  
  ep_disparity <- full_join(ep_full, disparity_full, by = "CBSA")
  
  # Re-structure/tidy df        
  disability <- ep_disparity %>%
    dplyr::select(-starts_with("Index"), -Employ_disparity, -Poverty_disparity) %>%
    gather(key = "Topic_group", value = "VALUE", -one_of("CBSA")) %>%
    mutate(Topic_employ = str_detect(Topic_group, "Employ"),
           Group_no_disability = str_detect(Topic_group, "no"),
           Rank = str_detect(Topic_group, "rank"),
           Disparity = str_detect(Topic_group, "disparity"),
           TOPIC_AREA = ifelse(Topic_employ == "TRUE", "Employment:equity group population ratio", "At/above poverty:equity group population ratio"),
           EQUITY_GROUP = ifelse(Group_no_disability == "TRUE", "No disability", "One or more disabilities"),
           EQUITY_CHARACTERISTIC = "Disability status",
           MEASURE = ifelse(Rank == "TRUE" & Disparity == "TRUE", "Rank of disparity",
                            ifelse(Rank == "TRUE" & Disparity == "FALSE", "Rank of measure", "Measure")),
           YEAR = df_name) %>%
    dplyr::select(-Topic_employ, -Group_no_disability, -Rank, -Disparity, -Topic_group)
  
  #assign(paste(df_name, "disability_vars", sep = "_"), disability, envir = .GlobalEnv)
}

employment_by_age <- function(df, df_name) {
  
  df_year <- as.numeric(df_name)
  
  df_tidy <- df %>%
    dplyr::select(CBSA, starts_with("B23001")) %>%
    transmute(CBSA = CBSA,
              Employed_16_19 = (!! census_sum_select("B23001e", c(5, 7, 91, 93)))/(B23001e3 + B23001e89),
              Employed_20_34 = (!! census_sum_select("B23001e", c(12, 14, 19, 21, 26, 28, 33, 35, 98, 100, 105, 107, 112, 114, 119, 121)))/(!! census_sum("B23001e", 10, 31, 7) + !! census_sum("B23001e", 96, 117, 7)),
              Employed_35_64 = (!! census_sum_select("B23001e", c(40, 42, 47, 49, 54, 56, 61, 63, 68, 70, 126, 128, 133, 135, 140, 142, 147, 149, 154, 156)))/(!! census_sum("B23001e", 38, 66, 7) +  !! census_sum("B23001e", 124, 152, 7)),
              Employed_65_over = (!! census_sum_select("B23001e", c(75, 80, 85, 161, 166, 171)))/(!! census_sum_select("B23001e", c(73, 78, 83, 159, 164, 169)))) %>%
    arrange(desc(CBSA)) %>%
    mutate(Index = row_number())
  
  # Get rank of each column (descending order, largest = highest rank)
  employment_ranks <- as.data.frame(purrr::map(-df_tidy, rank))
  colnames(employment_ranks) <- paste(colnames(df_tidy), "rank", sep = "_")
  
  employment_full <- full_join(df_tidy, employment_ranks, by = c("Index" = "CBSA_rank"))
  
  # Re-structure/tidy df
  employment <- employment_full %>%
    dplyr::select(-starts_with("Index")) %>%
    gather(key = "Topic_group", value = "VALUE", -one_of("CBSA")) %>%
    mutate(Rank = str_detect(Topic_group, "rank"),
           EQUITY_GROUP = str_extract_all(Topic_group, paste(c("16_19","20_34","35_64","65"), collapse = "|")),
           EQUITY_CHARACTERISTIC = "Age",
           TOPIC_AREA = "Employment:equity group population ratio",
           MEASURE = ifelse(Rank == "TRUE", "Rank of measure", "Measure"),
           EQUITY_GROUP = recode(as.character(EQUITY_GROUP), 
                                 `16_19` = "16-19",
                                 `20_34` = "20-34",
                                 `35_64` = "35-64",
                                 `65` = "65+"),
           YEAR = df_name) %>%
    dplyr::select(-Topic_group, -Rank)
  
  #assign(paste(df_name, "e_by_age", sep = "_"), employment, envir = .GlobalEnv)
  
}

poverty_by_age <- function(df, df_name) {
  
  df_year <- as.numeric(df_name)
  
  df_tidy <- df %>%
    dplyr::select(CBSA, starts_with("B17001")) %>%
    transmute(CBSA = CBSA,
              Above_poverty_18_34 = !! census_sum_select("B17001e", c(39, 40, 53, 54)),
              Above_poverty_35_64 = !! census_sum_select("B17001e", c(41, 42, 43, 55, 56, 57)),
              Above_poverty_65 = !! census_sum_select("B17001e", c(44, 45, 58, 59)),
              Prop_18_34 = Above_poverty_18_34/(Above_poverty_18_34 + !! census_sum_select("B17001e", c(10, 11, 24, 25))),
              Prop_35_64 = Above_poverty_35_64/(Above_poverty_35_64 + !! census_sum_select("B17001e", c(12, 13, 14, 26, 27, 28))),
              Prop_65 = Above_poverty_65/(Above_poverty_65 + !! census_sum_select("B17001e", c(15, 16, 29, 30)))) %>%
    dplyr::select(-starts_with("Above")) %>%
    arrange(desc(CBSA)) %>%
    mutate(Index = row_number())
  
  # Get rank of each column (descending order, largest = highest rank)
  poverty_ranks <- as.data.frame(purrr::map(-df_tidy, rank))
  colnames(poverty_ranks) <- paste(colnames(df_tidy), "rank", sep = "_")
  
  poverty_full <- full_join(df_tidy, poverty_ranks, by = c("Index" = "CBSA_rank"))    
  
  poverty <- poverty_full %>%
    dplyr::select(-starts_with("Index")) %>%
    unite(`18-34`, Prop_18_34, Prop_18_34_rank) %>%
    unite(`35-64`, Prop_35_64, Prop_35_64_rank) %>%
    unite(`65+`, Prop_65, Prop_65_rank) %>%
    gather(`18-34`:`65+`, key = "EQUITY_GROUP", value = "Measure_rank") %>%
    mutate(EQUITY_CHARACTERISTIC = "Age",
           TOPIC_AREA = "At/above poverty:equity group population ratio") %>%
    separate(Measure_rank, into = c("Measure", "Rank of measure"), sep = "_") %>%
    gather(Measure, `Rank of measure`, key = "MEASURE", value = "VALUE") %>%
    mutate(YEAR = df_name)
  
  #assign(paste(df_name, "p_by_age", sep = "_"), poverty, envir = .GlobalEnv)
  
}

tenure_by_age <- function(df, df_name) {
  
  df_year <- as.numeric(df_name)
  
  df_tidy <- df %>%
    dplyr::select(CBSA, starts_with("B25007")) %>%
    transmute(CBSA = CBSA,
              Owner_occ_15_24 = B25007e3/(B25007e3 + B25007e13),
              Owner_occ_25_34 = B25007e4/(B25007e4 + B25007e14),
              Owner_occ_35_64 = (!! census_sum("B25007e", 5, 8, 1))/(!! census_sum("B25007e", 5, 8, 1) + !! census_sum("B25007e", 15, 18, 1)),
              Owner_occ_65_over = (B25007e9 + B25007e10 + B25007e11)/(!! census_sum_select("B25007e", c(9, 10, 11, 19, 20, 21)))) %>%
    arrange(desc(CBSA)) %>%
    mutate(Index = row_number())
  
  # Get rank of each column (descending order, largest = highest rank)
  tenure_ranks <- as.data.frame(purrr::map(-df_tidy, rank))
  colnames(tenure_ranks) <- paste(colnames(df_tidy), "rank", sep = "_")
  
  tenure_full <- full_join(df_tidy, tenure_ranks, by = c("Index" = "CBSA_rank"))
  
  tenure <- tenure_full %>%
    dplyr::select(-starts_with("Index")) %>%
    unite(`15-24`, Owner_occ_15_24, Owner_occ_15_24_rank) %>%
    unite(`25-34`, Owner_occ_25_34, Owner_occ_25_34_rank) %>%
    unite(`35-64`, Owner_occ_35_64, Owner_occ_35_64_rank) %>%
    unite(`65+`, Owner_occ_65_over, Owner_occ_65_over_rank) %>%
    gather(`15-24`:`65+`, key = "EQUITY_GROUP", value = "Measure_rank") %>%
    mutate(EQUITY_CHARACTERISTIC = "Age",
           TOPIC_AREA = "Owner-occupied units:equity group population ratio") %>%
    separate(Measure_rank, into = c("Measure", "Rank of measure"), sep = "_") %>%
    gather(Measure, `Rank of measure`, key = "MEASURE", value = "VALUE") %>%
    mutate(YEAR = df_name)
  
  #assign(paste(df_name, "tenure_by_age", sep = "_"), tenure, envir = .GlobalEnv)
  
}

rent_burden_by_age <- function(df, df_name) {
  
  df_year <- as.numeric(df_name)
  
  df_tidy <- df %>%
    dplyr::select(CBSA, starts_with("B25072")) %>%
    transmute(CBSA = CBSA,
              B15_24 = (B25072e6 + B25072e7)/B25072e2,
              B25_34 = (B25072e13 + B25072e14)/B25072e9,
              B35_64 = (B25072e20 + B25072e21)/B25072e16,
              B35_65 = (B25072e27 + B25072e28)/B25072e23,
              LT20 = (!! census_sum("B25072e", 3, 24, 7))/B25072e1,
              B20_24 = (!! census_sum("B25072e", 4, 25, 7))/B25072e1,
              B25_29 = (!! census_sum("B25072e", 5, 26, 7))/B25072e1,
              B30_34 = (!! census_sum("B25072e", 6, 27, 7))/B25072e1,
              GT35 = (!! census_sum("B25072e", 7, 27, 7))/B25072e1) %>%
    arrange(desc(CBSA)) %>%
    mutate(Index = row_number())
  
  # Get rank of each column (descending order, largest % = highest rank)
  rent_ranks <- as.data.frame(purrr::map(-df_tidy, rank))
  
  colnames(rent_ranks) <- paste(colnames(df_tidy), "rank", sep = "_")
  
  rent_full <- full_join(df_tidy, rent_ranks, by = c("Index" = "CBSA_rank"))
  
  rent <- rent_full %>%
    dplyr::select(-starts_with("Index")) %>%
    gather(key = "Topic_group", value = "VALUE", -one_of("CBSA")) %>%
    mutate(Rank = str_detect(Topic_group, "rank"),
           EQUITY_GROUP = str_extract_all(Topic_group, paste(c("15_24","25_34","35_64","65"), collapse = "|")),
           EQUITY_CHARACTERISTIC = "Age",
           TOPIC_AREA = "Housing cost burden for renters:equity group population ratio",
           MEASURE = ifelse(Rank == "TRUE", "Rank of measure", "Measure"),
           EQUITY_GROUP = recode(as.character(EQUITY_GROUP), 
                                 `15_24` = "15-24",
                                 `25_34` = "25-34",
                                 `35_64` = "35-64",
                                 `65` = "65+",
                                 `character(0)` = "Total"),
           YEAR = df_name) %>%
    dplyr::select(-Topic_group, -Rank)
  
  #assign(paste(df_name, "rburden_by_age", sep = "_"), rent, envir = .GlobalEnv)
  
}

hcosts_burden_by_age <- function(df, df_name) {
  
  df_tidy <- df %>%
    dplyr::select(CBSA, starts_with("B25093")) %>%
    transmute(CBSA = CBSA,
              B15_24 = (B25093e6 + B25093e7)/B25093e2,
              B25_34 = (B25093e13 + B25093e14)/B25093e9,
              B35_64 = (B25093e20 + B25093e21)/B25093e16,
              B35_65 = (B25093e27 + B25093e28)/B25093e23,
              LT20 = (!! census_sum("B25093e", 3, 24, 7))/B25093e1,
              B20_24 = (!! census_sum("B25093e", 4, 25, 7))/B25093e1,
              B25_29 = (!! census_sum("B25093e", 5, 26, 7))/B25093e1,
              B30_34 = (!! census_sum("B25093e", 6, 27, 7))/B25093e1,
              GT35 = (!! census_sum("B25093e", 7, 27, 7))/B25093e1) %>%
    arrange(desc(CBSA)) %>%
    mutate(Index = row_number())
  
  # Get rank of each column (descending order, largest % = highest rank)
  hcost_ranks <- as.data.frame(purrr::map(-df_tidy, rank))
  
  colnames(hcost_ranks) <- paste(colnames(df_tidy), "rank", sep = "_")
  
  hcost_full <- full_join(df_tidy, hcost_ranks, by = c("Index" = "CBSA_rank"))
  
  hcosts <- hcost_full %>%
    dplyr::select(-starts_with("Index")) %>%
    gather(key = "Topic_group", value = "VALUE", -one_of("CBSA")) %>%
    mutate(Rank = str_detect(Topic_group, "rank"),
           EQUITY_GROUP = str_extract_all(Topic_group, paste(c("15_24","25_34","35_64","65"), collapse = "|")),
           EQUITY_CHARACTERISTIC = "Age",
           TOPIC_AREA = "Housing cost burden for renters:equity group population ratio",
           MEASURE = ifelse(Rank == "TRUE", "Rank of measure", "Measure"),
           EQUITY_GROUP = recode(as.character(EQUITY_GROUP), 
                                 `15_24` = "15-24",
                                 `25_34` = "25-34",
                                 `35_64` = "35-64",
                                 `65` = "65+",
                                 `character(0)` = "Total"),
           YEAR = df_name) %>%
    dplyr::select(-Topic_group, -Rank)
  
  #assign(paste(df_name, "hburden_by_age", sep = "_"), hcosts, envir = .GlobalEnv)
  
}