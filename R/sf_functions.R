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
    df_pared <- df %>%
      dplyr::select(CBSA, starts_with("B18120"), starts_with("B18130")) %>%
      transmute(CBSA = CBSA,
                Employed_disability = B18120e4/(B18120e4 + B18120e13 + B18120e22), # Denom = civilian noninstitutionalized pop with disability
                Employed_no_disability = B18120e11/(B18120e11 + B18120e20 + B18120e29), # Denom = civilian noninstitutionalized pop with no disability
                Above_poverty_disability = !! census_sum("B18130e", 5, 40, 7),
                Above_poverty_no_disability = !! census_sum("B18130e", 8, 43, 7),
                Prop_below_poverty_disability = 1 - (Above_poverty_disability/(Above_poverty_disability + !! census_sum("B18130e", 4, 39, 7))), # Denom = civilian noninstitutionalized pop with no disability
                Prop_below_poverty_no_disability = 1 - (Above_poverty_no_disability/(Above_poverty_no_disability + !! census_sum("B18130e", 7, 42, 7)))) %>%
      dplyr::select(-starts_with("Above"))
    
    invisible(df_pared)
    
  } else if (df_year <= 2007 & df_year >= 2005) {
    
    # Universe = civilian noninstitutionalized pop 18-64
    

    df_pared <- df %>%
      dplyr::select(CBSA, starts_with("B18020e"), starts_with("B18030")) %>%
      transmute(CBSA = CBSA,
                Employed_disability = (B18020e5 + B18020e8 + B18020e12 + B18020e15)/B18020e2, # Num = males and females of two different age groups that have 1+ disabilities and are employed
                Employed_no_disability = (B18020e20 + B18020e23 + B18020e27 + B18020e30)/B18020e17,
                Prop_below_poverty_disability = 1 - ((!! census_sum("B18030e", 6, 15, 3) + B18030e19 + B18030e22 + B18030e25 + B18030e28)/B18030e2),
                Prop_below_poverty_no_disability = 1 - ((!! census_sum("B18030e", 33, 42, 3) + !! census_sum("B18030e", 46, 55, 3))/B18030e29))
    
    invisible(df_pared)
    
  }
  
  else if (df_year == 2008) {
    
    # Universe = civilian noninstitutionalized pop 18-64
    
    df_pared <- df %>%
      dplyr::select(CBSA, starts_with("B18120"), starts_with("B18130")) %>%
      transmute(CBSA = CBSA,
                Employed_disability = (B18120e4 + B18120e11)/(B18120e3 + B18120e10),
                Employed_no_disability = (B18120e7 + B18120e14)/(B18120e6 + B18120e13),
                Above_poverty_disability = !! census_sum("B18130e", 5, 40, 7),
                Above_poverty_no_disability = !! census_sum("B18130e", 8, 43, 7),
                Prop_below_poverty_disability = 1 - (Above_poverty_disability/(Above_poverty_disability + !! census_sum("B18130e", 4, 39, 7))),
                Prop_below_poverty_no_disability = 1 - (Above_poverty_no_disability/(Above_poverty_no_disability + !! census_sum("B18130e", 7, 42, 7)))) %>%
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
              Odds_poverty_no_disability = Prop_below_poverty_no_disability/(1-Prop_below_poverty_no_disability), 
              Odds_poverty_disability = Prop_below_poverty_disability/(1-Prop_below_poverty_disability), # Create odds variable
              Poverty_disparity = Odds_poverty_disability/Odds_poverty_no_disability) %>% # Ratio of odds of being employed (Disability:No disability)
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
           TOPIC_AREA = ifelse(Topic_employ == "TRUE", "Employment:equity group population ratio", "Below poverty level:equity group population ratio"),
           EQUITY_GROUP = ifelse(Group_no_disability == "TRUE", "No disability", "One or more disabilities"),
           EQUITY_CHARACTERISTIC = "Disability status",
           MEASURE = ifelse(Rank == "TRUE" & Disparity == "TRUE", "Rank of disparity",
                            ifelse(Rank == "TRUE" & Disparity == "FALSE", "Rank of measure", "Measure")),
           YEAR = df_name) %>%
    dplyr::select(-Topic_employ, -Group_no_disability, -Rank, -Disparity, -Topic_group)
  
  #assign(paste(df_name, "disability_vars", sep = "_"), disability, envir = .GlobalEnv)
}

age_vars <- function(df, df_name) {
  
  # Universe = civilian, non-institutionalized pop (excludes armed forces members)
  
  df_tidy <- df %>%
    dplyr::select(CBSA, starts_with("B25007"), starts_with("B17001"), starts_with("B23001")) %>%
    transmute(CBSA = CBSA,
              Owner_occ_under_35 = (B25007e3 + B25007e4)/(!! census_sum_select("B25007e", c(3, 4, 13, 14))), # ages 15 - 34
              Owner_occ_35_64 = (!! census_sum("B25007e", 5, 8, 1))/(!! census_sum("B25007e", 5, 8, 1) + !! census_sum("B25007e", 15, 18, 1)),
              Owner_occ_65_over = (B25007e9 + B25007e10 + B25007e11)/(!! census_sum_select("B25007e", c(9, 10, 11, 19, 20, 21))),
              Above_poverty_under_35 = !! census_sum_select("B17001e", c(39, 40, 53, 54)), # ages 18 - 34
              Above_poverty_35_64 = !! census_sum_select("B17001e", c(41, 42, 43, 55, 56, 57)),
              Above_poverty_65 = !! census_sum_select("B17001e", c(44, 45, 58, 59)),
              Prop_below_pov_under_35 = 1-(Above_poverty_under_35/(Above_poverty_under_35 + !! census_sum_select("B17001e", c(10, 11, 24, 25)))),
              Prop_below_pov_35_64 = 1-(Above_poverty_35_64/(Above_poverty_35_64 + !! census_sum_select("B17001e", c(12, 13, 14, 26, 27, 28)))),
              Prop_below_pov_65 = 1-(Above_poverty_65/(Above_poverty_65 + !! census_sum_select("B17001e", c(15, 16, 29, 30)))),
              #Employed_16_19 = (B23001e7 + B23001e93)/(!! census_sum_select("B23001e", c(6, 9, 92, 95))),
              #Employed_20_34 = (!! census_sum_select("B23001e", c(14, 21, 28, 35, 100, 107, 114, 121)))/(!! census_sum_select("B23001e", c(13, 16, 20, 23, 27, 30, 34, 37, 99, 102, 106, 109, 113, 116, 120, 123))),
              Employed_under_35 = (!! census_sum("B23001e", 7, 35, 7) + !! census_sum("B23001e", 93, 121, 7))/(!! census_sum("B23001e", 6, 34, 7) + !! census_sum("B23001e", 9, 37, 7) + !! census_sum("B23001e", 92, 120, 7) + !! census_sum("B23001e", 95, 123, 7)),
              Employed_35_64 = (!! census_sum_select("B23001e", c(42, 49, 56, 63, 70, 128, 135, 142, 149, 156)))/(!! census_sum_select("B23001e", c(41, 44, 48, 51, 55, 58, 62, 65, 69, 72, 127, 130, 134, 137, 141, 144, 148, 151, 155, 158))),
              Employed_65_over = (!! census_sum_select("B23001e", c(75, 80, 85, 161, 166, 171)))/(!! census_sum_select("B23001e", c(73, 78, 83, 159, 164, 169)))) %>% # 65+ are civilian-only by definition
    dplyr::select(-starts_with("Above")) %>%
    arrange(desc(CBSA)) %>%
    mutate(Index = row_number())
  
  burden <- df %>%
    dplyr::select(CBSA, starts_with("B25072"), starts_with("B25093")) %>%
    transmute(CBSA = CBSA,
              #Hburden_15_24 = (B25093e6 + B25093e7)/B25093e2,
              #Hburden_25_34 = (B25093e13 + B25093e14)/B25093e9,
              Hburden_under_35 = (!! census_sum_select("B25093e", c(6, 7, 13, 14)))/(B25093e2 + B25093e9 - B25093e8 - B25093e15), # Subtract not-computed households from universe
              Hburden_35_64 = (B25093e20 + B25093e21)/B25093e16,
              Hburden_65 = (B25093e27 + B25093e28)/B25093e23,
              #Rburden_15_24 = (B25072e6 + B25072e7)/B25072e2,
              #Rburden_25_34 = (B25072e13 + B25072e14)/B25072e9,
              Rburden_under_35 = (!! census_sum_select("B25072e", c(6, 7, 13, 14)))/(B25072e2 + B25072e9 - B25072e8 - B25072e15), # Subtract not-computed households from universe
              Rburden_35_64 = (B25072e20 + B25072e21)/B25072e16,
              Rburden_65 = (B25072e27 + B25072e28)/B25072e23) %>%
    arrange(CBSA) %>%
    mutate(Index = row_number())
  
  # Get rank of each column (descending order, largest % = highest rank)
  ranks <- as.data.frame(purrr::map(-df_tidy, rank))
  colnames(ranks) <- paste(colnames(df_tidy), "rank", sep = "_")
  
  ranks_full <- full_join(df_tidy, ranks, by = c("Index" = "CBSA_rank"))
  
  # Get rank of each column (ascending order, smallest % = highest rank) - ie higher housing cost burden = lower rank
  burden_ranks <- as.data.frame(purrr::map(burden, rank))
  colnames(burden_ranks) <- paste(colnames(burden), "rank", sep = "_")
  
  burden_ranks_full <- full_join(burden, burden_ranks, by = c("Index" = "CBSA_rank"))
  
  df_ranks <- full_join(ranks_full, burden_ranks_full, by = "CBSA")
  
  df_full_tidy <- df_ranks %>%
    dplyr::select(-starts_with("Index")) %>%
    gather(key = "Topic_group", value = "VALUE", -one_of("CBSA")) %>%
    mutate(Rank = str_detect(Topic_group, "rank"),
           EQUITY_GROUP = str_extract_all(Topic_group, paste(c("under_35", "35_64", "65"), collapse = "|")),
           EQUITY_CHARACTERISTIC = "Age",
           TOPIC_AREA = str_extract_all(Topic_group, paste(c("Owner_occ", "pov", "Employ", "Hburden", "Rburden"), collapse = "|")),
           MEASURE = ifelse(Rank == "TRUE", "Rank of measure", "Measure"),
           TOPIC_AREA = recode(as.character(TOPIC_AREA),
                               Owner_occ = "Owner-occupied units:equity group population ratio",
                               pov = "Below poverty level:equity group population ratio",
                               Employ = "Employment:equity group population ratio",
                               Hburden = "Housing cost burden for owners:equity group population ratio",
                               Rburden = "Housing cost burden for renters:equity group population ratio"),
           EQUITY_GROUP = recode(as.character(EQUITY_GROUP), 
                                 #`15_24` = "15-24",
                                 #`18_34` = "18-34",
                                 under_35 = "Under 35",
                                 #`25-34` = "25-34",
                                 #`16_19` = "16-19",
                                 #`20_34` = "20-34",
                                 `35_64` = "35-64",
                                 `65` = "65+"),
           YEAR = df_name) %>%
    dplyr::select(-Topic_group, -Rank)
  
}