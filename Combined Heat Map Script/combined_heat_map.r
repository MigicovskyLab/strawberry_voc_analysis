library(tidyverse)
library(readxl)
library(ComplexHeatmap)
library(colorRamp2)
library(grid)

aov_data <- read_excel("../strawberry_aroma.xlsx", sheet = "aovData")

id_cols <- c("scUnit", "sBlock!", "sbPlot!", "ssPlot!", "sClone!")
long_df <- aov_data %>%
  pivot_longer(cols = 6:ncol(aov_data), names_to = "column", values_to = "value") %>%
  select(all_of(id_cols), column, value)

cutoffs <- long_df %>%
  group_by(column) %>%
  dplyr::summarise(
    q1 = quantile(value, 0.25, na.rm = TRUE),
    q3 = quantile(value, 0.75, na.rm = TRUE),
    iqr = q3 - q1,
    lower = q1 - 10 * iqr,
    upper = q3 + 10 * iqr,
    .groups = "drop"
  )

flagged <- long_df %>%
  left_join(cutoffs, by = "column") %>%
  mutate(is_outlier = !is.na(value) & (value < lower | value > upper))

outlier_cells <- flagged %>%
  filter(is_outlier) %>%
  select(scUnit, column)

long_no_outliers <- long_df %>%
  anti_join(outlier_cells, by = c("scUnit", "column"))

aov_data_no_outliers <- long_no_outliers %>%
  tidyr::pivot_wider(id_cols = all_of(id_cols), names_from = column, values_from = value) %>%
  dplyr::select(any_of(names(aov_data)))

aov_data_means <- aov_data_no_outliers %>%
  group_by(`sClone!`) %>%
  summarise(across(c(where(is.numeric), -any_of(c("scUnit"))), ~ mean(.x, na.rm = TRUE)), .groups = "drop")

aov_data_means <- aov_data_means %>%
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

get_pretty_labels <- function(raw_names) {
  new_labels <- voc_name_map[raw_names]
  missing_idx <- is.na(new_labels)
  if (any(missing_idx)) {
    backup_names <- raw_names[missing_idx]
    backup_names <- gsub("^%", "", backup_names)
    backup_names <- gsub("^total_", "Total ", backup_names)
    backup_names <- gsub("_", " ", backup_names)
    new_labels[missing_idx] <- backup_names
  }
  return(new_labels)
}

get_class_name <- function(x) {
  if (x %in% class_branched_chain_esters) return("Branched Esters")
  if (x %in% class_straight_chain_esters) return("Straight Esters")
  if (x %in% class_carbonyls_ketones_diketones) return("Ketones/Diketones")
  if (x %in% class_carbonyls_aldehydes) return("Aldehydes")
  if (x %in% class_carboxylic_acids) return("Carboxylic Acids")
  if (x %in% class_alcohols_diols) return("Alcohols/Diols")
  if (x %in% class_terpenoids) return("Terpenoids")
  if (x %in% class_thioesters) return("Thioesters")
  if (x %in% class_oxygen_heterocycles) return("Oxygen Heterocycles")
  return("Unclassified")
}

desired_class_order <- c("Alcohols/Diols", "Aldehydes", "Branched Esters", "Carboxylic Acids", "Ketones/Diketones", "Oxygen Heterocycles", "Straight Esters", "Thioesters", "Terpenoids")
totals_class_map <- c("total_Alcohols_Diols" = "Alcohols/Diols", "total_Aldehydes" = "Aldehydes", "total_Branched_Esters" = "Branched Esters", "total_Carboxylic_Acids" = "Carboxylic Acids", "total_Ketones" = "Ketones/Diketones", "total_Oxygen_Heterocycles" = "Oxygen Heterocycles", "total_Straight_Esters" = "Straight Esters", "total_Sulfur_Compounds" = "Thioesters", "total_Monoterpenoids" = "Terpenoids")
desired_class_order_totals <- desired_class_order
class_colors <- c("Branched Esters" = "darkred", "Straight Esters" = "darkgreen", "Ketones/Diketones" = "#0072B2", "Aldehydes" = "#009E73", "Carboxylic Acids" = "purple", "Alcohols/Diols" = "#E69F00", "Terpenoids" = "darkcyan", "Thioesters" = "deeppink", "Oxygen Heterocycles" = "saddlebrown")
fdr_threshold <- 0.05

compound_cols_clean <- intersect(compound_cols, colnames(aov_data_means_totals))

sorting_df <- data.frame(compound = compound_cols_clean, class_name = sapply(compound_cols_clean, get_class_name), stringsAsFactors = FALSE)
sorting_df$display_label <- get_pretty_labels(sorting_df$compound)
sorting_df <- sorting_df %>% arrange(factor(class_name, levels = desired_class_order), display_label)

final_sorted_cols <- sorting_df$compound
final_display_labels <- sorting_df$display_label

compound_data <- aov_data_means_totals %>% dplyr::select(all_of(final_sorted_cols)) %>% mutate(across(everything(), as.numeric))
mat <- as.matrix(compound_data)

class_labels_vector <- sorting_df$class_name
ann <- data.frame(Class = factor(class_labels_vector, levels = desired_class_order))
rownames(ann) <- final_sorted_cols
ann_colors <- list(Class = class_colors)

corr <- suppressWarnings(cor(mat, use = "pairwise.complete.obs", method = "pearson"))
p_raw <- matrix(NA, nrow = ncol(mat), ncol = ncol(mat))
rownames(p_raw) <- colnames(p_raw) <- colnames(mat)

for (i in 1:ncol(mat)) {
  for (j in 1:ncol(mat)) {
    if (i != j) {
      vec_x <- mat[, i]
      vec_y <- mat[, j]
      valid_indices <- complete.cases(vec_x, vec_y)
      if (sum(valid_indices) >= 3) {
        test <- cor.test(vec_x[valid_indices], vec_y[valid_indices], method = "pearson")
        p_raw[i, j] <- test$p.value
      } else {
        p_raw[i, j] <- NA
      }
    }
  }
}

p_adj <- matrix(NA, nrow = ncol(mat), ncol = ncol(mat))
dimnames(p_adj) <- dimnames(p_raw)
upper_tri_indices <- upper.tri(p_raw)
raw_p_vec <- p_raw[upper_tri_indices]
adj_p_vec <- p.adjust(raw_p_vec, method = "BH")
p_adj[upper_tri_indices] <- adj_p_vec
p_adj[lower.tri(p_adj)] <- t(p_adj)[lower.tri(p_adj)]

corr_plot <- corr
corr_plot[p_adj > fdr_threshold] <- NA
corr_plot[is.na(p_adj)] <- NA
corr_plot[upper.tri(corr_plot, diag = TRUE)] <- NA

heat_col_fun <- colorRamp2(c(-1, 0, 1), c("blue", "white", "red"))

bottom_ann_obj <- HeatmapAnnotation(df = ann, col = ann_colors, show_legend = FALSE, show_annotation_name = FALSE)
left_ann_obj <- rowAnnotation(df = ann, col = ann_colors, show_annotation_name = FALSE, show_legend = FALSE)

heatmap_plot <- Heatmap(
  corr_plot,
  name = "Correlation",
  col = heat_col_fun,
  na_col = "white",
  cell_fun = function(j, i, x, y, width, height, fill) {
    if(i > j) {
      grid.rect(x, y, width, height, gp = gpar(col = "grey80", lwd = 0.5, fill = NA))
    }
  },
  row_labels = final_display_labels,
  column_labels = final_display_labels,
  column_title = " ",
  column_title_gp = gpar(fontsize = 22, fontface = "bold"),
  cluster_rows = FALSE,
  cluster_columns = FALSE,
  bottom_annotation = bottom_ann_obj,
  left_annotation = left_ann_obj,
  row_split = ann$Class,
  column_split = ann$Class,
  cluster_row_slices = FALSE,
  cluster_column_slices = FALSE,
  row_title = NULL,
  row_names_gp = gpar(fontsize = 22),
  column_names_gp = gpar(fontsize = 22),
  column_names_rot = 90,
  row_names_side = "left",
  heatmap_width = unit(18, "inch"),
  heatmap_height = unit(18, "inch"),
  heatmap_legend_param = list(
    title_gp = gpar(fontsize = 28),
    labels_gp = gpar(fontsize = 22),
    legend_height = unit(4, "inch"),
    grid_width = unit(0.4, "inch")
  )
)

totals_df_map <- data.frame(col_name = names(totals_class_map), class_name = unname(totals_class_map), stringsAsFactors = FALSE) %>% arrange(factor(class_name, levels = desired_class_order_totals))
totals_ordered_cols <- totals_df_map$col_name
totals_display_labels <- totals_df_map$class_name

mat_totals <- aov_data_means_totals %>% dplyr::select(all_of(totals_ordered_cols)) %>% as.matrix()
ann_totals <- data.frame(Class = factor(totals_display_labels, levels = desired_class_order_totals))
rownames(ann_totals) <- totals_ordered_cols
ann_colors_totals <- list(Class = class_colors[levels(ann_totals$Class)])

corr_totals <- cor(mat_totals, use = "pairwise.complete.obs", method = "pearson")
p_raw_totals <- matrix(NA, nrow = ncol(mat_totals), ncol = ncol(mat_totals))
rownames(p_raw_totals) <- colnames(p_raw_totals) <- colnames(mat_totals)

for (i in 1:ncol(mat_totals)) {
  for (j in 1:ncol(mat_totals)) {
    if (i != j) {
      vec_x <- mat_totals[, i]
      vec_y <- mat_totals[, j]
      valid_indices <- complete.cases(vec_x, vec_y)
      if (sum(valid_indices) >= 3) {
        test <- cor.test(vec_x[valid_indices], vec_y[valid_indices], method = "pearson")
        p_raw_totals[i, j] <- test$p.value
      } else {
        p_raw_totals[i, j] <- NA
      }
    }
  }
}

p_adj_totals <- matrix(NA, nrow = ncol(mat_totals), ncol = ncol(mat_totals))
dimnames(p_adj_totals) <- dimnames(p_raw_totals)
upper_tri_indices <- upper.tri(p_raw_totals)
raw_p_vec <- p_raw_totals[upper_tri_indices]
adj_p_vec <- p.adjust(raw_p_vec, method = "BH")
p_adj_totals[upper_tri_indices] <- adj_p_vec
p_adj_totals[lower.tri(p_adj_totals)] <- t(p_adj_totals)[lower.tri(p_adj_totals)]

corr_plot_totals <- corr_totals
corr_plot_totals[p_adj_totals > fdr_threshold] <- NA
corr_plot_totals[is.na(p_adj_totals)] <- NA
corr_plot_totals[upper.tri(corr_plot_totals, diag = TRUE)] <- NA

bottom_ann_totals <- HeatmapAnnotation(df = ann_totals, col = ann_colors_totals, show_legend = FALSE, show_annotation_name = FALSE)
left_ann_totals <- rowAnnotation(df = ann_totals, col = ann_colors_totals, show_legend = FALSE, show_annotation_name = FALSE)

heatmap_object_totals <- Heatmap(
  corr_plot_totals,
  name = "Correlation",
  col = heat_col_fun,
  na_col = "transparent",
  show_heatmap_legend = FALSE,
  column_title = NULL,
  row_title = NULL,
  cell_fun = function(j, i, x, y, width, height, fill) {
    if(i > j) {
      grid.rect(x, y, width, height, gp = gpar(col = "grey80", lwd = 0.5, fill = NA))
    }
  },
  row_labels = totals_display_labels,
  column_labels = totals_display_labels,
  cluster_rows = FALSE,
  cluster_columns = FALSE,
  bottom_annotation = bottom_ann_totals,
  left_annotation = left_ann_totals,
  row_split = ann_totals$Class,
  column_split = ann_totals$Class,
  cluster_row_slices = FALSE,
  cluster_column_slices = FALSE,
  row_names_gp = gpar(fontsize = 22),
  column_names_gp = gpar(fontsize = 22),
  column_names_rot = 90,
  row_names_side = "left",
  heatmap_width = unit(7, "inch"),
  heatmap_height = unit(7, "inch")
)

png("combined_heatmaps.png", width = 28, height = 22, units = "in", res = 300)
grid.newpage()
draw(heatmap_plot, newpage = FALSE)
grid.text("A", x = unit(0.18, "npc"), y = unit(0.94, "npc"), gp = gpar(fontsize = 40, fontface = "bold"))
vp_inset <- viewport(x = 0.65, y = 0.73, width = 0.14, height = 0.14, just = c("right", "top"))
pushViewport(vp_inset)
draw(heatmap_object_totals, newpage = FALSE, background = "transparent")
grid.text("B", x = unit(0.02, "npc"), y = unit(1.85, "npc"), gp = gpar(fontsize = 40, fontface = "bold"))
popViewport()
dev.off()