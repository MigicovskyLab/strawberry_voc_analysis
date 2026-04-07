library(tidyverse)
library(readxl)
library(Hmisc)
library(cowplot)

aov_data <- read_excel("../strawberry_aroma.xlsx", sheet = "aovData")

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

voc_name_map <- c(
  "%1_Butanol_3_methyl_acetate" = "Isoamyl acetate", "X.1_Butanol_3_methyl_acetate" = "Isoamyl acetate",
  "%2_Hexen_1_ol_acetate_Z" = "2Z-Hexenyl acetate", "X.2_Hexen_1_ol_acetate_Z" = "2Z-Hexenyl acetate",
  "Acetic_acid_methyl_ester" = "Methyl acetate", "Butanoic_acid_2_methyl_methyl_es" = "Methyl 2-methylbutyrate",
  "Butanoic_acid_butyl_ester" = "Butyl butyrate", "Ethyl_Acetate" = "Ethyl acetate",
  "Isopropyl_butyrate" = "Isopropyl butyrate", "Methyl_valerate" = "Methyl valerate",
  "%2_3_Butanedione" = "Butanedione", "X.2_3_Butanedione" = "Butanedione",
  "Hexanoic_acid" = "Hexanoic acid", "alpha_Terpineol" = "α Terpineol",
  "%2_3H_Furanone_5_hexyldihydro" = "γ-Decalactone", "X.2_3H_Furanone_5_hexyldihydro" = "γ-Decalactone",
  "%2_Butenoic_acid_methyl_ester" = "Methyl 2-butanoate", "X.2_Butenoic_acid_methyl_ester" = "Methyl 2-butanoate",
  "Acetic_acid_butyl_ester" = "Butyl acetate", "Butanoic_acid_2_methyl_ethyl_est" = "Ethyl 2-methyl butanoate",
  "Butanoic_acid_3_methyl_ethyl_est" = "Ethyl isovalerate", "Butanoic_acid_ethyl_ester" = "Ethyl butyrate",
  "Hexanoic_acid_ethyl_ester" = "Ethyl hexanoate", "Methyl_isovalerate" = "Methyl isovalerate",
  "Acetone" = "Acetone", "Hexanal" = "Hexanal", "Ethanol" = "Ethanol",
  "Linalool" = "Linalool", "Furan_2_ethyl" = "2-Ethylfuran",
  "Acetic_acid_hexyl_ester" = "Hexyl acetate", "Butanoic_acid_methyl_ester" = "Methyl butyrate",
  "Hexanoic_acid_methyl_ester" = "Methyl hexanoate", "Methyl_propionate" = "Methyl propionate",
  "%2_Heptanone" = "2-Heptanone", "X.2_Heptanone" = "2-Heptanone",
  "%2_4_Hexadienal_E_E" = "2,4-Hexadienal", "X.2_4_Hexadienal_E_E" = "2,4-Hexadienal",
  "%1_Hexanol_2_ethyl" = "2-Ethylhexanol", "X.1_Hexanol_2_ethyl" = "2-Ethylhexanol",
  "Butanethioic_acid_S_methyl_ester" = "Methyl thiobutanoate", "%3_2H_Furanone_4_methoxy_2_5_dim" = "Mesifurane",
  "X.3_2H_Furanone_4_methoxy_2_5_dim" = "Mesifurane", "%2_Pentanone" = "2-Pentanone",
  "X.2_Pentanone" = "2-Pentanone", "Butanoic_acid_2_methyl" = "2-Methylbutanoic acid",
  "%2_2_4_Trimethyl_1_3_pentanediol" = "2,2,4-Trimethyl-1,3-pentanediol", "X.2_2_4_Trimethyl_1_3_pentanediol" = "2,2,4-Trimethyl-1,3-pentanediol"
)

fdr_threshold <- 0.05
aroma_cols <- c(analysis_cols, "total_Aroma_Compounds")
agronomic_cols <- c("Harvest_Date", "Berry_Wt")
color_cols <- c("L.", "a.")

aroma_data <- combined_data[, aroma_cols, drop = FALSE]
agronomic_data_merge <- combined_data[, c(agronomic_cols, color_cols), drop = FALSE]

aroma_data <- as.data.frame(lapply(aroma_data, as.numeric))
agronomic_data_merge <- as.data.frame(lapply(agronomic_data_merge, as.numeric))

cross_cor <- cor(aroma_data, agronomic_data_merge, use = "pairwise.complete.obs")
cor_results <- rcorr(as.matrix(aroma_data), as.matrix(agronomic_data_merge))

p_matrix <- cor_results$P[rownames(cross_cor), colnames(cross_cor)]
n_tests <- ncol(aroma_data) * ncol(agronomic_data_merge)

p_values_vector <- as.vector(p_matrix)
p_adj_vector <- p.adjust(p_values_vector, method = "BH", n = n_tests)
p_adj_matrix <- matrix(p_adj_vector, nrow = nrow(p_matrix), ncol = ncol(p_matrix))
dimnames(p_adj_matrix) <- dimnames(p_matrix)

sig_indices <- which(p_adj_matrix < fdr_threshold, arr.ind = TRUE)

if (nrow(sig_indices) > 0) {
  pos_plots <- list()
  neg_plots <- list()
  for (i in seq_len(nrow(sig_indices))) {
    row_idx <- sig_indices[i, 1]
    col_idx <- sig_indices[i, 2]
    raw_aroma_name <- rownames(p_adj_matrix)[row_idx]
    raw_agro_name <- colnames(p_adj_matrix)[col_idx]
    if (is.na(raw_aroma_name) || is.na(raw_agro_name)) next
    
    if (!is.null(voc_name_map) && (raw_aroma_name %in% names(voc_name_map))) {
      display_aroma <- voc_name_map[[raw_aroma_name]]
    } else {
      display_aroma <- gsub("total_", "Total ", raw_aroma_name)
      display_aroma <- gsub("_", " ", display_aroma)
      display_aroma <- gsub("^%", "", display_aroma)
    }
    
    clean_agro_name <- raw_agro_name
    if (raw_agro_name %in% "Berry_Wt") {
      clean_agro_name <- "Berry Weight (g)"
    } else if (raw_agro_name %in% "Harvest_Date") {
      clean_agro_name <- "Harvest Date (Julian)"
    }
    
    r_raw <- cross_cor[row_idx, col_idx]
    r_val <- round(r_raw, 2)
    p_val <- formatC(p_adj_matrix[row_idx, col_idx], format = "e", digits = 2)
    plot_color <- ifelse(r_raw > 0, "blue", "red")
    
    temp_df <- data.frame(
      Aroma_Value = aroma_data[[raw_aroma_name]],
      Agro_Value = agronomic_data_merge[[raw_agro_name]]
    )
    
    x_center <- mean(range(temp_df$Aroma_Value, na.rm = TRUE))
    stat_label <- paste0("r = ", r_val, " | p-FDR = ", p_val)
    
    p_individual <- ggplot(temp_df, aes(x = Aroma_Value, y = Agro_Value)) +
      geom_point(alpha = 0.6, size = 2) +
      geom_smooth(method = "lm", color = plot_color, se = FALSE, linewidth = 1) +
      annotate("text", x = Inf, y = Inf, label = stat_label, vjust = 2, hjust = 1, size = 7) +
      theme_bw() +
      theme(
        axis.title = element_text(size = 28),
        axis.text = element_text(size = 28),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank(),
        plot.margin = margin(t = 30, r = 15, b = 15, l = 30, unit = "pt")
      ) +
      labs(x = bquote(.(display_aroma) ~ (ng ~ g^-1)), y = clean_agro_name)
    
    if (r_raw > 0) {
      pos_plots[[length(pos_plots) + 1]] <- p_individual
    } else {
      neg_plots[[length(neg_plots) + 1]] <- p_individual
    }
  }
  
  png("aroma_agronomic_clean_names.png", width = 18, height = 11, units = "in", res = 300)
  combined_plots <- c(pos_plots, list(NULL), neg_plots)
  final_plot <- plot_grid(
    plotlist = combined_plots, ncol = 3, nrow = 2,
    labels = c("A", "B", "", "C", "D", "E"), label_size = 28,
    label_y = 1, label_x = 0, align = "hv", axis = "tblr", scale = 1
  )
  print(final_plot)
  dev.off()
}