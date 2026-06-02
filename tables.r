library(tidyverse)
library(readxl)

aov_data <- read_excel("strawberry_aroma.xlsx", sheet = "aovData")

id_cols <- c("scUnit", "sBlock!", "sbPlot!", "ssPlot!", "sClone!")

long_df <- aov_data %>%
  pivot_longer(
    cols = 6:ncol(aov_data),
    names_to = "column",
    values_to = "value"
  ) %>%
  mutate(`sClone!` = str_replace(`sClone!`, "F\\. x ananassa 90537-3", "F. x a 90537-3")) %>%
  select(all_of(id_cols), column, value)

cutoffs <- long_df %>%
  group_by(column) %>%
  dplyr::summarise(
    q1    = quantile(value, 0.25, na.rm = TRUE),
    q3    = quantile(value, 0.75, na.rm = TRUE),
    iqr   = q3 - q1,
    lower = q1 - 10 * iqr,
    upper = q3 + 10 * iqr,
    .groups = "drop"
  )

outlier_cells <- long_df %>%
  left_join(cutoffs, by = "column") %>%
  mutate(is_outlier = !is.na(value) & (value < lower | value > upper)) %>%
  filter(is_outlier) %>%
  select(scUnit, column)

aov_data_clean <- long_df %>%
  anti_join(outlier_cells, by = c("scUnit", "column")) %>%
  tidyr::pivot_wider(
    id_cols = all_of(id_cols),
    names_from = column,
    values_from = value
  ) %>%
  dplyr::select(any_of(names(aov_data)))

aov_data_filtered <- aov_data_clean %>%
  select(all_of(id_cols), where(~ is.numeric(.) && mean(is.na(.)) <= 0.8))

valid_compounds <- setdiff(names(aov_data_filtered), id_cols)

class_branched_chain_esters <- c("%1_Butanol_3_methyl_acetate", "%2_Hexen_1_ol_acetate_Z", "Butanoic_acid_2_methyl_methyl_es", "Isopropyl_butyrate", "%2_Butenoic_acid_methyl_ester", "Butanoic_acid_2_methyl_ethyl_est", "Butanoic_acid_3_methyl_ethyl_est", "Methyl_isovalerate")
class_straight_chain_esters <- c("Acetic_acid_methyl_ester", "Butanoic_acid_butyl_ester", "Ethyl_Acetate", "Methyl_valerate", "Acetic_acid_butyl_ester", "Butanoic_acid_ethyl_ester", "Hexanoic_acid_ethyl_ester", "Acetic_acid_hexyl_ester", "Butanoic_acid_methyl_ester", "Hexanoic_acid_methyl_ester", "Methyl_propionate")
class_carbonyls_ketones_diketones <- c("Acetone", "%2_Heptanone", "%2_Pentanone", "%2_3_Butanedione")
class_carbonyls_aldehydes <- c("Hexanal", "%2_4_Hexadienal_E_E")
class_carboxylic_acids <- c("Butanoic_acid_2_methyl", "Hexanoic_acid")
class_alcohols_diols <- c("Ethanol", "%1_Hexanol_2_ethyl", "%2_2_4_Trimethyl_1_3_pentanediol")
class_terpenoids <- c("alpha_Terpineol", "Linalool")
class_thioesters <- c("Butanethioic_acid_S_methyl_ester")
class_oxygen_heterocycles <- c("%2_3H_Furanone_5_hexyldihydro", "Furan_2_ethyl", "%3_2H_Furanone_4_methoxy_2_5_dim")

aov_data_totals <- aov_data_filtered %>%
  mutate(
    `Ester, Branched chain` = rowSums(select(., any_of(class_branched_chain_esters)), na.rm = TRUE),
    `Ester, Straight chain` = rowSums(select(., any_of(class_straight_chain_esters)), na.rm = TRUE),
    Ketones_Diketones = rowSums(select(., any_of(class_carbonyls_ketones_diketones)), na.rm = TRUE),
    Aldehydes = rowSums(select(., any_of(class_carbonyls_aldehydes)), na.rm = TRUE),
    Carboxylic_Acids = rowSums(select(., any_of(class_carboxylic_acids)), na.rm = TRUE),
    Alcohols_Diols = rowSums(select(., any_of(class_alcohols_diols)), na.rm = TRUE),
    Terpenoids = rowSums(select(., any_of(class_terpenoids)), na.rm = TRUE),
    Thioesters = rowSums(select(., any_of(class_thioesters)), na.rm = TRUE),
    Oxygen_Heterocycles = rowSums(select(., any_of(class_oxygen_heterocycles)), na.rm = TRUE),
    Total_Aroma_Abundance = rowSums(select(., any_of(valid_compounds)), na.rm = TRUE)
  )

class_names <- c(
  "Ester, Branched chain", "Ester, Straight chain", "Ketones_Diketones",
  "Aldehydes", "Carboxylic_Acids", "Alcohols_Diols", "Terpenoids",
  "Thioesters", "Oxygen_Heterocycles", "Total_Aroma_Abundance"
)

variety_means <- aov_data_totals %>%
  group_by(`sClone!`) %>%
  summarise(across(any_of(c(valid_compounds, class_names)), ~ mean(.x, na.rm = TRUE)), .groups = "drop")

master_summary_table <- variety_means %>%
  select(-`sClone!`) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Compound_or_Class",
    values_to = "Value"
  ) %>%
  group_by(Compound_or_Class) %>%
  summarise(
    Mean = mean(Value, na.rm = TRUE),
    Median = median(Value, na.rm = TRUE),
    Min = min(Value, na.rm = TRUE),
    Max = max(Value, na.rm = TRUE),
    n = sum(!is.na(Value)),
    .groups = "drop"
  )

write.csv(master_summary_table, "aroma_compounds_all_summary.csv", row.names = FALSE)

class_summary_table <- master_summary_table %>%
  filter(Compound_or_Class %in% class_names) %>%
  rename(Class = Compound_or_Class)

write.csv(class_summary_table, "aroma_class_totals_summary.csv", row.names = FALSE)

variety_combined_table <- variety_means %>%
  mutate(n = rowSums(select(., any_of(valid_compounds)) > 0, na.rm = TRUE)) %>%
  select(
    `sClone!`,
    n,
    `Ester, Branched chain`,
    `Ester, Straight chain`,
    Ketones_Diketones,
    Aldehydes,
    Carboxylic_Acids,
    Alcohols_Diols,
    Terpenoids,
    Thioesters,
    Oxygen_Heterocycles,
    Total_Aroma_Abundance
  )

write.csv(variety_combined_table, "variety_combined_aroma_summary.csv", row.names = FALSE)

output_dir <- "/Users/clayelzinga/Desktop/plots/VOC_histograms"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

safe_filename <- function(x) {
  x <- gsub("%", "pct_", x)
  x <- gsub("[^A-Za-z0-9._-]", "_", x)
  x
}

voc_name_map <- c(
  "%1_Butanol_3_methyl_acetate"        = "Isoamyl acetate",
  "X.1_Butanol_3_methyl_acetate"       = "Isoamyl acetate",
  "%2_Hexen_1_ol_acetate_Z"            = "2Z-Hexenyl acetate",
  "X.2_Hexen_1_ol_acetate_Z"           = "2Z-Hexenyl acetate",
  "Acetic_acid_methyl_ester"           = "Methyl acetate",
  "Butanoic_acid_2_methyl_methyl_es"   = "Methyl 2-methylbutyrate",
  "Butanoic_acid_butyl_ester"          = "Butyl butyrate",
  "Ethyl_Acetate"                      = "Ethyl acetate",
  "Isopropyl_butyrate"                 = "Isopropyl butyrate",
  "Methyl_valerate"                    = "Methyl valerate",
  "%2_3_Butanedione"                   = "Butanedione",
  "X.2_3_Butanedione"                  = "Butanedione",
  "Hexanoic_acid"                      = "Hexanoic acid",
  "alpha_Terpineol"                    = "α Terpineol",
  "%2_3H_Furanone_5_hexyldihydro"      = "γ-Decalactone",
  "X.2_3H_Furanone_5_hexyldihydro"     = "γ-Decalactone",
  "%2_Butenoic_acid_methyl_ester"      = "Methyl 2-butanoate",
  "X.2_Butenoic_acid_methyl_ester"     = "Methyl 2-butanoate",
  "Acetic_acid_butyl_ester"            = "Butyl acetate",
  "Butanoic_acid_2_methyl_ethyl_est"   = "Ethyl 2-methyl butanoate",
  "Butanoic_acid_3_methyl_ethyl_est"   = "Ethyl isovalerate",
  "Butanoic_acid_ethyl_ester"          = "Ethyl butyrate",
  "Hexanoic_acid_ethyl_ester"          = "Ethyl hexanoate",
  "Methyl_isovalerate"                 = "Methyl isovalerate",
  "Acetone"                            = "Acetone",
  "Hexanal"                            = "Hexanal",
  "Ethanol"                            = "Ethanol",
  "Linalool"                           = "Linalool",
  "Furan_2_ethyl"                      = "2-Ethylfuran",
  "Acetic_acid_hexyl_ester"            = "Hexyl acetate",
  "Butanoic_acid_methyl_ester"         = "Methyl butyrate",
  "Hexanoic_acid_methyl_ester"         = "Methyl hexanoate",
  "Methyl_propionate"                  = "Methyl propionate",
  "%2_Heptanone"                       = "2-Heptanone",
  "X.2_Heptanone"                      = "2-Heptanone",
  "%2_4_Hexadienal_E_E"                = "2,4-Hexadienal",
  "X.2_4_Hexadienal_E_E"               = "2,4-Hexadienal",
  "%1_Hexanol_2_ethyl"                 = "2-Ethylhexanol",
  "X.1_Hexanol_2_ethyl"                = "2-Ethylhexanol",
  "Butanethioic_acid_S_methyl_ester"   = "Methyl thiobutanoate",
  "%3_2H_Furanone_4_methoxy_2_5_dim"   = "Mesifurane",
  "X.3_2H_Furanone_4_methoxy_2_5_dim"  = "Mesifurane",
  "%2_Pentanone"                       = "2-Pentanone",
  "X.2_Pentanone"                      = "2-Pentanone",
  "Butanoic_acid_2_methyl"             = "2-Methylbutanoic acid",
  "%2_2_4_Trimethyl_1_3_pentanediol"   = "2,2,4-Trimethyl-1,3-pentanediol",
  "X.2_2_4_Trimethyl_1_3_pentanediol"  = "2,2,4-Trimethyl-1,3-pentanediol",
  "Ester, Branched chain"              = "Branched Esters",
  "Ester, Straight chain"              = "Straight Esters",
  "Ketones_Diketones"                  = "Ketones/Diketones",
  "Aldehydes"                          = "Aldehydes",
  "Carboxylic_Acids"                   = "Carboxylic Acids",
  "Alcohols_Diols"                     = "Alcohols/Diols",
  "Terpenoids"                         = "Terpenoids",
  "Thioesters"                         = "Thioesters",
  "Oxygen_Heterocycles"                = "Oxygen Heterocycles",
  "Total_Aroma_Abundance"              = "Total Aroma Abundance"
)

get_display_name <- function(col_name) {
  if (col_name %in% names(voc_name_map)) {
    return(voc_name_map[[col_name]])
  } else {
    return(col_name)
  }
}

cols_for_hist <- c(valid_compounds, class_names)

for (compound in cols_for_hist) {
  if (!is.numeric(variety_means[[compound]])) next
  
  display_name <- get_display_name(compound)
  
  p <- ggplot(variety_means, aes(x = .data[[compound]])) +
    geom_histogram(bins = 30, color = "black", fill = "steelblue", na.rm = TRUE) +
    labs(title = paste("Histogram of", display_name), x = display_name, y = "Count") +
    theme_minimal(base_size = 14)
  
  out_file <- file.path(output_dir, paste0("hist_", safe_filename(compound), ".png"))
  ggsave(filename = out_file, plot = p, width = 7, height = 5, dpi = 300)
}

summary_total_aroma <- summary(variety_means$Total_Aroma_Abundance)

compound_means <- variety_means %>%
  select(any_of(valid_compounds)) %>%
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "Compound", values_to = "Mean_Abundance") %>%
  arrange(desc(Mean_Abundance))

top_compound <- head(compound_means, 1)
bottom_compound <- tail(compound_means, 1)

class_totals <- variety_means %>%
  select(
    `Ester, Branched chain`, `Ester, Straight chain`, Ketones_Diketones,
    Aldehydes, Carboxylic_Acids, Alcohols_Diols, Terpenoids,
    Thioesters, Oxygen_Heterocycles
  ) %>%
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "Class", values_to = "Mean_Class_Abundance") %>%
  mutate(Relative_Abundance_Pct = (Mean_Class_Abundance / sum(Mean_Class_Abundance)) * 100) %>%
  arrange(desc(Relative_Abundance_Pct))

chemical_class_list <- list(
  "Branched Ester" = class_branched_chain_esters,
  "Straight Ester" = class_straight_chain_esters,
  "Ketone" = class_carbonyls_ketones_diketones,
  "Aldehyde" = class_carbonyls_aldehydes,
  "Carboxylic Acid" = class_carboxylic_acids,
  "Alcohol/Diol" = class_alcohols_diols,
  "Terpenoid" = class_terpenoids,
  "Thioester" = class_thioesters,
  "Oxygen Het" = class_oxygen_heterocycles
)

class_counts <- data.frame(
  Class = names(chemical_class_list),
  Compound_Count = sapply(chemical_class_list, length)
) %>%
  arrange(desc(Compound_Count))

variety_extremes <- variety_means %>%
  select(`sClone!`, Total_Aroma_Abundance) %>%
  filter(!is.na(Total_Aroma_Abundance)) %>%
  arrange(desc(Total_Aroma_Abundance))

highest_variety <- head(variety_extremes, 1)
lowest_variety <- tail(variety_extremes, 1)

variety_diversity <- variety_combined_table %>%
  select(`sClone!`, n) %>%
  arrange(desc(n))

highest_diversity <- head(variety_diversity, 1)

total_samples <- nrow(variety_means)

compound_prevalence <- variety_means %>%
  select(any_of(valid_compounds)) %>%
  summarise(across(everything(), ~ sum(.x > 0, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "Compound", values_to = "Count_Present") %>%
  mutate(
    Total_Samples = total_samples,
    Percent_Samples_Present = (Count_Present / Total_Samples) * 100,
    Fraction_Present = paste0(Count_Present, "/", Total_Samples, " samples")
  ) %>%
  arrange(desc(Percent_Samples_Present))

most_common <- head(compound_prevalence, 3)
least_common <- tail(compound_prevalence, 3)

print(summary_total_aroma)
print(top_compound)
print(bottom_compound)
print(class_totals)
print(class_counts)
print(highest_variety)
print(lowest_variety)
print(highest_diversity)
print(most_common)
print(least_common)
