library(tidyverse)
library(readxl)
library(broom)

aov_data <- read_excel("strawberry_aroma.xlsx", sheet = "aovData")

id_cols <- c("scUnit", "sBlock!", "sbPlot!", "ssPlot!", "sClone!")
long_df <- aov_data %>%
  pivot_longer(cols = 6:ncol(aov_data), names_to = "column", values_to = "value") %>%
  select(all_of(id_cols), column, value)

cutoffs <- long_df %>%
  group_by(column) %>%
  dplyr::summarise(q1 = quantile(value, 0.25, na.rm = TRUE), q3 = quantile(value, 0.75, na.rm = TRUE), iqr = q3 - q1, lower = q1 - 10 * iqr, upper = q3 + 10 * iqr, .groups = "drop")

flagged <- long_df %>%
  left_join(cutoffs, by = "column") %>%
  mutate(is_outlier = !is.na(value) & (value < lower | value > upper))

outlier_cells <- flagged %>% filter(is_outlier) %>% select(scUnit, column)
long_no_outliers <- long_df %>% anti_join(outlier_cells, by = c("scUnit", "column"))

aov_data_no_outliers <- long_no_outliers %>%
  tidyr::pivot_wider(id_cols = all_of(id_cols), names_from = column, values_from = value) %>%
  dplyr::select(any_of(names(aov_data)))

aov_data_means <- aov_data_no_outliers %>%
  group_by(`sClone!`) %>%
  summarise(across(c(where(is.numeric), -any_of(c("scUnit"))), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  select(where(~ mean(is.na(.)) <= 0.8), -c(`sBlock!`, `sbPlot!`, `ssPlot!`))

all_vocs <- c("%1_Butanol_3_methyl_acetate", "%1_Hexanol_2_ethyl", "%2_3H_Furanone_5_hexyldihydro", "%2_2_4_Trimethyl_1_3_pentanediol", "%2_3_Butanedione", "%2_4_Hexadienal_E_E", "%2_Butenoic_acid_methyl_ester", "%2_Heptanone", "%2_Hexen_1_ol_acetate_Z", "%2_Pentanone", "%3_2H_Furanone_4_methoxy_2_5_dim", "Acetic_acid_butyl_ester", "Acetic_acid_hexyl_ester", "Acetic_acid_methyl_ester", "Acetone", "alpha_Terpineol", "Butanethioic_acid_S_methyl_ester", "Butanoic_acid_2_methyl", "Butanoic_acid_2_methyl_ethyl_est", "Butanoic_acid_2_methyl_methyl_es", "Butanoic_acid_3_methyl_ethyl_est", "Butanoic_acid_butyl_ester", "Butanoic_acid_ethyl_ester", "Butanoic_acid_methyl_ester", "Ethanol", "Ethyl_Acetate", "Furan_2_ethyl", "Hexanal", "Hexanoic_acid", "Hexanoic_acid_ethyl_ester", "Hexanoic_acid_methyl_ester", "Isopropyl_butyrate", "Linalool", "Methyl_isovalerate", "Methyl_propionate", "Methyl_valerate")

class_branched_chain_esters <- c("%1_Butanol_3_methyl_acetate", "%2_Hexen_1_ol_acetate_Z", "Butanoic_acid_2_methyl_methyl_es", "Isopropyl_butyrate", "%2_Butenoic_acid_methyl_ester", "Butanoic_acid_2_methyl_ethyl_est", "Butanoic_acid_3_methyl_ethyl_est", "Methyl_isovalerate")
class_straight_chain_esters <- c("Acetic_acid_methyl_ester", "Butanoic_acid_butyl_ester", "Ethyl_Acetate", "Methyl_valerate", "Acetic_acid_butyl_ester", "Butanoic_acid_ethyl_ester", "Hexanoic_acid_ethyl_ester", "Acetic_acid_hexyl_ester", "Butanoic_acid_methyl_ester", "Hexanoic_acid_methyl_ester", "Methyl_propionate")
class_carbonyls_ketones_diketones <- c("Acetone", "%2_Heptanone", "%2_Pentanone", "%2_3_Butanedione")
class_carbonyls_aldehydes <- c("Hexanal", "%2_4_Hexadienal_E_E")
class_carboxylic_acids <- c("Butanoic_acid_2_methyl", "Hexanoic_acid")
class_alcohols_diols <- c("Ethanol", "%1_Hexanol_2_ethyl", "%2_2_4_Trimethyl_1_3_pentanediol")
class_terpenoids <- c("alpha_Terpineol", "Linalool")
class_thioesters <- c("Butanethioic_acid_S_methyl_ester")
class_oxygen_heterocycles <- c("%2_3H_Furanone_5_hexyldihydro", "Furan_2_ethyl", "%3_2H_Furanone_4_methoxy_2_5_dim")

ordered_columns <- c(class_branched_chain_esters, class_straight_chain_esters, class_carbonyls_ketones_diketones, class_carbonyls_aldehydes, class_carboxylic_acids, class_alcohols_diols, class_terpenoids, class_thioesters, class_oxygen_heterocycles)

id_keep <- intersect(c("sClone!", "scUnit", "sBlock!", "sbPlot!", "ssPlot!"), names(aov_data_means))
aov_data_means_totals <- aov_data_means %>% select(all_of(c(id_keep, ordered_columns)))

aov_data_means_totals <- aov_data_means_totals %>%
  mutate(
    total_Branched_Esters = rowSums(select(., all_of(class_branched_chain_esters)), na.rm = TRUE),
    total_Straight_Esters = rowSums(select(., all_of(class_straight_chain_esters)), na.rm = TRUE),
    total_Carboxylic_Acids = rowSums(select(., all_of(class_carboxylic_acids)), na.rm = TRUE),
    total_Alcohols_Diols = rowSums(select(., all_of(class_alcohols_diols)), na.rm = TRUE),
    total_Ketones = rowSums(select(., all_of(class_carbonyls_ketones_diketones)), na.rm = TRUE),
    total_Aldehydes = rowSums(select(., all_of(class_carbonyls_aldehydes)), na.rm = TRUE),
    total_Monoterpenoids = rowSums(select(., all_of(class_terpenoids)), na.rm = TRUE),
    total_Sulfur_Compounds = rowSums(select(., all_of(class_thioesters)), na.rm = TRUE),
    total_Oxygen_Heterocycles = rowSums(select(., all_of(class_oxygen_heterocycles)), na.rm = TRUE),
    total_Aroma_Compounds = rowSums(select(., all_of(ordered_columns)), na.rm = TRUE),
    sample_Ubiquity = rowSums(select(., all_of(ordered_columns)) > 0, na.rm = TRUE)
  )

total_cols <- grep("_total$", names(aov_data_means_totals), value = TRUE)
prop_cols  <- grep("^prop_",  names(aov_data_means_totals), value = TRUE)
compound_cols <- setdiff(names(aov_data_means_totals), c(id_keep, prop_cols))
compound_cols <- intersect(compound_cols, all_vocs)
analysis_cols <- c(compound_cols, total_cols)

agronomic_data <- read_csv("strawberry_agronomic_data.csv")
name_map <- c("CA 42.8-16 Tioga parent" = "CA 42.8‑16 Tioga parent", "F. x ananassa NC 98-22-4" = "F. x ananassa NC 98‑22‑4", "Annapolis (Kentville)" = "K-Annapolis", "Bounty (Kentville)" = "K-Bounty", "Brunswick (Kentville)" = "K-Brunswick", "Cabot (Kentville)" = "K-Cabot", "Cavendish (Kentville)" = "K-Cavendish", "Evangeline (Kentville)" = "K-Evangeline", "Honeoye (Kentville)" = "K-Honeoye", "Jewel (Kentville)" = "K-Jewel", "Kent (Kentville)" = "K-Kent", "Laurel (Kentville)" = "K-Laurel", "Lila (Kentville)" = "K-Lila", "Mira (Kentville)" = "K-Mira", "Sable (Kentville)" = "K-Sable", "Valley Sunset (Kentville)" = "K-Valley Sunset", "Veestar (Kentville)" = "K-Veestar", "Wendy (Kentville)" = "K-Wendy", "NW 90054-37" = "NW 90054‑37")

agronomic_data <- agronomic_data %>% mutate(Plant_Name_clean = dplyr::recode(Plant_Name, !!!name_map))
combined_data <- left_join(aov_data_means_totals, agronomic_data, by = c("sClone!" = "Plant_Name_clean")) %>% select(-X, -Y, -Z) %>% mutate(across(c(`L.`, `a.`, `b.`, `C.`, `h`), ~ ifelse(`sClone!` %in% c("Weisse Anasa", "White D"), NA, .)))

safe_name <- function(x) gsub("[^A-Za-z0-9._-]", "_", gsub("%","pct_", x))

combined_data_STATUS <- combined_data %>%
  filter(STATUS == "Cultivar") %>%
  mutate(YEAR_ORIGIN = as.numeric(YEAR_ORIGIN))

aroma_cols <- c(analysis_cols, "total_Aroma_Compounds")

model_summaries <- aroma_cols %>%
  set_names() %>%
  map_dfr(function(aroma) {
    df <- combined_data_STATUS %>%
      transmute(YEAR_ORIGIN, aroma_value = as.numeric(.data[[aroma]])) %>%
      drop_na(YEAR_ORIGIN, aroma_value)

    year_var <- stats::var(df$YEAR_ORIGIN)
    
    if (nrow(df) < 3 || is.na(year_var) || year_var == 0) {
      return(tibble(compound = aroma, estimate = NA_real_, raw_p_value = NA_real_, r_squared = NA_real_))
    }

    fit <- lm(aroma_value ~ YEAR_ORIGIN, data = df)
    fit_tidy <- tidy(fit) %>% filter(term == "YEAR_ORIGIN")
    fit_glance <- glance(fit)

    tibble(compound = aroma, estimate = fit_tidy$estimate, raw_p_value = fit_tidy$p.value, r_squared = fit_glance$r.squared)
  })

final_results <- model_summaries %>%
  drop_na(raw_p_value) %>%
  mutate(fdr_p_value = p.adjust(raw_p_value, method = "fdr")) %>%
  arrange(raw_p_value) %>%
  select(compound, estimate, r_squared, raw_p_value, fdr_p_value)

print(final_results)

potential_covariates <- c("Flowering_Date", "Petal_Number", "Chl..June.", "Chl..July.", "Chl..Aug.", "Harvest_Date", "Days_Ripe", "Berry_Wt", "L.", "a.", "b.", "C.", "h")

covariate_testing <- map_dfr(aroma_cols, function(aroma) {
  map_dfr(potential_covariates, function(cov) {
    df <- combined_data_STATUS %>% select(aroma_value = all_of(aroma), cov_value = all_of(cov)) %>% drop_na()
    if (nrow(df) < 10 || stats::var(df$cov_value) == 0) {
      return(tibble(compound = aroma, covariate = cov, r_squared = NA_real_))
    }
    fit <- lm(aroma_value ~ cov_value, data = df)
    tibble(compound = aroma, covariate = cov, r_squared = glance(fit)$r.squared)
  })
})

top_global_covariates <- covariate_testing %>%
  drop_na(r_squared) %>%
  group_by(covariate) %>%
  summarise(mean_r_squared = mean(r_squared), max_r_squared = max(r_squared)) %>%
  arrange(desc(mean_r_squared))

print(top_global_covariates)

combined_data_STATUS <- combined_data %>%
  filter(STATUS == "Cultivar") %>%
  mutate(YEAR_ORIGIN = as.numeric(YEAR_ORIGIN), Harvest_Date = as.numeric(Harvest_Date), Berry_Wt = as.numeric(Berry_Wt))

model_summaries_complex <- aroma_cols %>%
  set_names() %>%
  map_dfr(function(aroma) {
    df <- combined_data_STATUS %>%
      transmute(YEAR_ORIGIN, Harvest_Date, Berry_Wt, aroma_value = as.numeric(.data[[aroma]])) %>%
      drop_na(YEAR_ORIGIN, Harvest_Date, Berry_Wt, aroma_value)

    year_var <- stats::var(df$YEAR_ORIGIN)
    if (nrow(df) < 4 || is.na(year_var) || year_var == 0) {
      return(tibble(compound = aroma, estimate = NA_real_, raw_p_value = NA_real_, r_squared = NA_real_))
    }

    fit <- lm(aroma_value ~ YEAR_ORIGIN + Harvest_Date + Berry_Wt, data = df)
    fit_tidy <- tidy(fit) %>% filter(term == "YEAR_ORIGIN")
    fit_glance <- glance(fit)

    tibble(compound = aroma, estimate = fit_tidy$estimate, raw_p_value = fit_tidy$p.value, r_squared = fit_glance$r.squared)
  })

final_complex_results <- model_summaries_complex %>%
  drop_na(raw_p_value) %>%
  mutate(fdr_p_value = p.adjust(raw_p_value, method = "fdr")) %>%
  arrange(raw_p_value) %>%
  select(compound, estimate, r_squared, raw_p_value, fdr_p_value)

print(final_complex_results)