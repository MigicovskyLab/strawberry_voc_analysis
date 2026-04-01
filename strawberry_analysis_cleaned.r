# %%############################################################################# load libs
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(janitor)
library(e1071)  
library(pheatmap)
library(viridisLite)
library(Hmisc)
library(ggrepel)
library(naniar)
library(RColorBrewer)
library(purrr)
library(readxl)
library(corrplot)
library(cluster)
library(vegan)
library(tibble)
library(plotly)
library(htmlwidgets)
library(glue)
library(pcaMethods)
library(fmsb)
library(rlang)
library(broom)
library(ComplexHeatmap)
library(colorRamp2)
library(scales)
library(ggfortify)
library(ggbeeswarm) 
library(grid)
library(patchwork)

# %%############################################################################# data loading and cleaning
aov_data <- read_excel("/Users/clayelzinga/Desktop/Work_School/School/Thesis/Data_sets/strawberry_data/sbDivConc21out.xlsx", sheet = "aovData")

id_cols <- c("scUnit", "sBlock!", "sbPlot!", "ssPlot!", "sClone!")
long_df <- aov_data %>%
  pivot_longer(
    cols = 6:ncol(aov_data),
    names_to = "column",
    values_to = "value"
  ) %>%
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

flagged <- long_df %>%
  left_join(cutoffs, by = "column") %>%
  mutate(is_outlier = !is.na(value) & (value < lower | value > upper))

pairs_with_outlier <- flagged %>%
  filter(is_outlier) %>%
  distinct(column, `sClone!`)

dup_outliers_df <- flagged %>%
  semi_join(pairs_with_outlier, by = c("column", "sClone!")) %>%
  arrange(column, `sClone!`, scUnit) %>%
  select(
    column, `sClone!`, scUnit, `sBlock!`, `sbPlot!`, `ssPlot!`,
    value, is_outlier, q1, q3, lower, upper
  )

outlier_cells <- flagged %>%
  filter(is_outlier) %>%
  select(scUnit, column)

long_no_outliers <- long_df %>%
  anti_join(outlier_cells, by = c("scUnit", "column"))

aov_data_no_outliers <- long_no_outliers %>%
  tidyr::pivot_wider(
    id_cols = all_of(id_cols),
    names_from = column,
    values_from = value
  ) %>%
  dplyr::select(any_of(names(aov_data)))

cat("Dropped cells:", nrow(outlier_cells), "\n")
cat("Rows (scUnit) before/after:", nrow(aov_data), "→", nrow(aov_data_no_outliers), "\n")

removed_outliers_df <- flagged %>%
  filter(is_outlier) %>%
  select(column, `sClone!`,`sBlock!`, `sbPlot!`, `ssPlot!`, value, is_outlier)

write.csv(removed_outliers_df, file = "removed_outliers.csv", row.names = FALSE)

aov_data_means <- aov_data_no_outliers %>%
  group_by(`sClone!`) %>%
  summarise(
    across(
      c(where(is.numeric), -any_of(c("scUnit"))),
      ~ mean(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )

original_columns <- colnames(aov_data_means)

aov_data_means <- aov_data_means %>%
  select(where(~ mean(is.na(.)) <= 0.8), -c(`sBlock!`, `sbPlot!`, `ssPlot!`))

removed_columns <- setdiff(original_columns, colnames(aov_data_means))
cat("Removed columns:", removed_columns, "\n")

hexanoic_acid_count <- sum(!is.na(aov_data_no_outliers$Hexanoic_acid_1))
nerolidol_count <- sum(!is.na(aov_data_no_outliers$Nerolidol))
hexenal_E_count <- sum(!is.na(aov_data_no_outliers$`%2_Hexenal_E`))

cat("Hexanoic_acid_1 non-NA values:", hexanoic_acid_count, "\n")
cat("Nerolidol non-NA values:", nerolidol_count, "\n")
cat("Hexenal_E non-NA values:", hexenal_E_count, "\n")

str(aov_data_means)

all_vocs <- c(
  "%1_Butanol_3_methyl_acetate", "%1_Hexanol_2_ethyl",
  "%2_3H_Furanone_5_hexyldihydro", "%2_2_4_Trimethyl_1_3_pentanediol",
  "%2_3_Butanedione", "%2_4_Hexadienal_E_E", "%2_Butenoic_acid_methyl_ester",
  "%2_Heptanone", "%2_Hexen_1_ol_acetate_Z", "%2_Pentanone",
  "%3_2H_Furanone_4_methoxy_2_5_dim", "Acetic_acid_butyl_ester",
  "Acetic_acid_hexyl_ester", "Acetic_acid_methyl_ester", "Acetone",
  "alpha_Terpineol", "Butanethioic_acid_S_methyl_ester", "Butanoic_acid_2_methyl",
  "Butanoic_acid_2_methyl_ethyl_est", "Butanoic_acid_2_methyl_methyl_es",
  "Butanoic_acid_3_methyl_ethyl_est", "Butanoic_acid_butyl_ester",
  "Butanoic_acid_ethyl_ester", "Butanoic_acid_methyl_ester", "Ethanol",
  "Ethyl_Acetate", "Furan_2_ethyl", "Hexanal", "Hexanoic_acid",
  "Hexanoic_acid_ethyl_ester", "Hexanoic_acid_methyl_ester", "Isopropyl_butyrate",
  "Linalool", "Methyl_isovalerate", "Methyl_propionate", "Methyl_valerate"
)

add_class_total <- function(df, class_cols, new_colname) {
  present <- intersect(class_cols, colnames(df))
  missing <- setdiff(class_cols, present)
  if (length(missing) > 0) {
    message(sprintf("[%s] %d columns missing and skipped: %s",
                    new_colname, length(missing), paste(missing, collapse = ", ")))
  }
  df[[new_colname]] <- if (length(present) == 0) NA_real_ else
    rowSums(df[, present, drop = FALSE], na.rm = TRUE)
  df
}

aov_data_means_totals <- aov_data_means

class_branched_chain_esters <- c(
  "%1_Butanol_3_methyl_acetate",
  "%2_Hexen_1_ol_acetate_Z",
  "Butanoic_acid_2_methyl_methyl_es",
  "Isopropyl_butyrate",
  "%2_Butenoic_acid_methyl_ester",
  "Butanoic_acid_2_methyl_ethyl_est",
  "Butanoic_acid_3_methyl_ethyl_est",
  "Methyl_isovalerate"
)

class_straight_chain_esters <- c(
  "Acetic_acid_methyl_ester",
  "Butanoic_acid_butyl_ester",
  "Ethyl_Acetate",
  "Methyl_valerate",
  "Acetic_acid_butyl_ester",
  "Butanoic_acid_ethyl_ester",
  "Hexanoic_acid_ethyl_ester",
  "Acetic_acid_hexyl_ester",
  "Butanoic_acid_methyl_ester",
  "Hexanoic_acid_methyl_ester",
  "Methyl_propionate"
)

class_carbonyls_ketones_diketones <- c(
  "Acetone", "%2_Heptanone", "%2_Pentanone", "%2_3_Butanedione"
)

class_carbonyls_aldehydes <- c(
  "Hexanal", "%2_4_Hexadienal_E_E"
)

class_carboxylic_acids <- c(
  "Butanoic_acid_2_methyl", "Hexanoic_acid"
)

class_alcohols_diols <- c(
  "Ethanol", "%1_Hexanol_2_ethyl", "%2_2_4_Trimethyl_1_3_pentanediol"
)

class_terpenoids <- c(
  "alpha_Terpineol", "Linalool"
)

class_thioesters <- c(
  "Butanethioic_acid_S_methyl_ester"
)

class_oxygen_heterocycles <- c(
  "%2_3H_Furanone_5_hexyldihydro", "Furan_2_ethyl",
  "%3_2H_Furanone_4_methoxy_2_5_dim"
)

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

ordered_columns <- c(class_branched_chain_esters, class_straight_chain_esters, class_carbonyls_ketones_diketones, 
            class_carbonyls_aldehydes, class_carboxylic_acids, 
            class_alcohols_diols, class_terpenoids, 
            class_thioesters, class_oxygen_heterocycles)

id_keep <- intersect(c("sClone!", "scUnit", "sBlock!", "sbPlot!", "ssPlot!"), names(aov_data_means_totals))

aov_data_means_totals <- aov_data_means_totals %>%
  select(all_of(c(id_keep, ordered_columns)))

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

output_dir <- "/Users/clayelzinga/Desktop/plots/VOC_histograms"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

safe_filename <- function(x) {
  x <- gsub("%", "pct_", x)
  x <- gsub("[^A-Za-z0-9._-]", "_", x)
  x
}

desired_class_order <- c(
  "Alcohols/Diols", "Aldehydes", "Branched Esters", "Carboxylic Acids",
  "Ketones/Diketones", "Oxygen Heterocycles", "Straight Esters",
  "Thioesters", "Terpenoids"
)

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
  "X.2_2_4_Trimethyl_1_3_pentanediol"  = "2,2,4-Trimethyl-1,3-pentanediol"
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

totals_class_map <- c(
  "total_Alcohols_Diols"      = "Alcohols/Diols",
  "total_Aldehydes"           = "Aldehydes",
  "total_Branched_Esters"     = "Branched Esters",
  "total_Carboxylic_Acids"    = "Carboxylic Acids",
  "total_Ketones"             = "Ketones/Diketones",
  "total_Oxygen_Heterocycles" = "Oxygen Heterocycles",
  "total_Straight_Esters"     = "Straight Esters",
  "total_Sulfur_Compounds"    = "Thioesters",
  "total_Monoterpenoids"      = "Terpenoids"  
)
desired_class_order_totals <- c(
  "Alcohols/Diols", 
  "Aldehydes", 
  "Branched Esters", 
  "Carboxylic Acids",
  "Ketones/Diketones", 
  "Oxygen Heterocycles", 
  "Straight Esters",
  "Thioesters", 
  "Terpenoids"
)

class_colors <- c(
  "Branched Esters" = "darkred", "Straight Esters" = "darkgreen", 
  "Ketones/Diketones" = "#0072B2", "Aldehydes" = "#009E73",
  "Carboxylic Acids" = "purple", "Alcohols/Diols" = "#E69F00",
  "Terpenoids" = "darkcyan", "Thioesters" = "deeppink", 
  "Oxygen Heterocycles" = "saddlebrown"
)

# %%############################################################################# histogram loop
compound_cols_for_hist <- c(compound_cols, total_cols)

for (compound in compound_cols_for_hist) {
    if (!is.numeric(aov_data_means_totals[[compound]])) next

    p <- ggplot(aov_data_means_totals, aes(x = .data[[compound]])) +
        geom_histogram(bins = 30, color = "black", fill = "steelblue", na.rm = TRUE) +
        labs(title = paste("Histogram of", voc_name_map[compound]), x = voc_name_map[compound], y = "Count") +
        theme_minimal(base_size = 14)

    out_file <- file.path(output_dir, paste0("hist_", safe_filename(compound), ".png"))
    ggsave(filename = out_file, plot = p, width = 7, height = 5, dpi = 300)
}


# %%############################################################################# combined heatmap
compound_cols_clean <- intersect(compound_cols, colnames(aov_data_means_totals))

sorting_df <- data.frame(
  compound = compound_cols_clean,
  class_name = sapply(compound_cols_clean, get_class_name),
  stringsAsFactors = FALSE
)

sorting_df$display_label <- get_pretty_labels(sorting_df$compound)

sorting_df <- sorting_df %>%
  arrange(factor(class_name, levels = desired_class_order), display_label)

final_sorted_cols <- sorting_df$compound
final_display_labels <- sorting_df$display_label

compound_data <- aov_data_means_totals %>%
  dplyr::select(all_of(final_sorted_cols)) %>%
  mutate(across(everything(), as.numeric))

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

fdr_threshold <- 0.05
corr_plot <- corr
corr_plot[p_adj > fdr_threshold] <- NA
corr_plot[is.na(p_adj)] <- NA
corr_plot[upper.tri(corr_plot, diag = TRUE)] <- NA

heat_col_fun <- colorRamp2(c(-1, 0, 1), c("blue", "white", "red"))

bottom_ann_obj <- HeatmapAnnotation(
  df = ann,
  col = ann_colors,
  show_legend = FALSE,
  show_annotation_name = FALSE
)

left_ann_obj <- rowAnnotation(
  df = ann,
  col = ann_colors,
  show_annotation_name = FALSE,
  annotation_legend_param = list(
      title_gp = gpar(fontsize = 30),
      labels_gp = gpar(fontsize = 22)
  )
)

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
  row_names_gp = gpar(fontsize = 16),
  column_names_gp = gpar(fontsize = 16),
  column_names_rot = 90,
  row_names_side = "left",
  heatmap_width = unit(18, "inch"),
  heatmap_height = unit(18, "inch"),
  heatmap_legend_param = list(
    title_gp = gpar(fontsize = 18),
    labels_gp = gpar(fontsize = 14)
  )
)

totals_df_map <- data.frame(
  col_name = names(totals_class_map),
  class_name = unname(totals_class_map),
  stringsAsFactors = FALSE
)

totals_df_map <- totals_df_map %>% 
  arrange(factor(class_name, levels = desired_class_order_totals))

totals_ordered_cols <- totals_df_map$col_name
totals_display_labels <- totals_df_map$class_name

mat_totals <- aov_data_means_totals %>%
  dplyr::select(all_of(totals_ordered_cols)) %>%
  as.matrix()

ann_totals <- data.frame(
  Class = factor(totals_display_labels, levels = desired_class_order_totals)
)
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

bottom_ann_totals <- HeatmapAnnotation(
  df = ann_totals,
  col = ann_colors_totals,
  show_legend = FALSE,
  show_annotation_name = FALSE
)

left_ann_totals <- rowAnnotation(
  df = ann_totals,
  col = ann_colors_totals,
  show_legend = FALSE,
  show_annotation_name = FALSE
)

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
  row_names_gp = gpar(fontsize = 16),
  column_names_gp = gpar(fontsize = 16),
  column_names_rot = 90,
  row_names_side = "left",
  heatmap_width = unit(7, "inch"),
  heatmap_height = unit(7, "inch")
)

png("/Users/clayelzinga/Desktop/plots/strawberry/combined_heatmaps.png", width = 28, height = 22, units = "in", res = 300)

grid.newpage()

draw(heatmap_plot, newpage = FALSE)

grid.text("A", x = unit(0.18, "npc"), y = unit(0.94, "npc"), gp = gpar(fontsize = 40, fontface = "bold"))

vp_inset <- viewport(x = 0.65, y = 0.73, width = 0.14, height = 0.14, just = c("right", "top"))
pushViewport(vp_inset)

draw(heatmap_object_totals, newpage = FALSE, background = "transparent")

grid.text("B", x = unit(0.02, "npc"), y = unit(1.85, "npc"), gp = gpar(fontsize = 40, fontface = "bold"))

popViewport()

dev.off()



# %%############################################################################# intra v inter class
stats_summary <- data.frame(
  Type = c("Intra-Class", "Inter-Class"),
  n_Significant = c(0, 0),
  n_NonSig = c(0, 0),
  Total_Pairs = c(0, 0),
  stringsAsFactors = FALSE
)

indices <- which(upper.tri(p_adj), arr.ind = TRUE)

for (k in 1:nrow(indices)) {
   row_idx <- indices[k, 1]
   col_idx <- indices[k, 2]
   
   comp_a <- rownames(p_adj)[row_idx]
   comp_b <- colnames(p_adj)[col_idx]
   
   class_a <- as.character(ann[comp_a, "Class"])
   class_b <- as.character(ann[comp_b, "Class"])
   
   if (length(class_a) == 0 || is.na(class_a) || length(class_b) == 0 || is.na(class_b)) next
   
   is_sig <- !is.na(p_adj[row_idx, col_idx]) && p_adj[row_idx, col_idx] < fdr_threshold
   
   if (class_a == class_b) {
      stats_summary$Total_Pairs[1] <- stats_summary$Total_Pairs[1] + 1
      if (is_sig) {
         stats_summary$n_Significant[1] <- stats_summary$n_Significant[1] + 1
      } else {
         stats_summary$n_NonSig[1] <- stats_summary$n_NonSig[1] + 1
      }
   } else {
      stats_summary$Total_Pairs[2] <- stats_summary$Total_Pairs[2] + 1
      if (is_sig) {
         stats_summary$n_Significant[2] <- stats_summary$n_Significant[2] + 1
      } else {
         stats_summary$n_NonSig[2] <- stats_summary$n_NonSig[2] + 1
      }
   }
}

stats_summary$Percent_Sig <- (stats_summary$n_Significant / stats_summary$Total_Pairs) * 100

contingency_matrix <- matrix(
  c(stats_summary$n_Significant[1], stats_summary$n_Significant[2],
    stats_summary$n_NonSig[1], stats_summary$n_NonSig[2]),
  nrow = 2,
  dimnames = list(Group = c("Intra-Class", "Inter-Class"),
                  Outcome = c("Significant", "Non-Sig"))
)

fisher_result <- fisher.test(contingency_matrix)

print(stats_summary)

cat("\n--- Fisher's Exact Test Results ---\n")
cat("P-value: ", fisher_result$p.value, "\n")
cat("Odds Ratio: ", fisher_result$estimate, "\n")


# %%############################################################################# Load in Agronomic Data
agronomic_data <- read_csv("/Users/clayelzinga/Desktop/Work_School/School/Thesis/Data_sets/strawberry_data/strawberry_data_updated_2025_05_21.csv")
# Make a lookup table for known mismatches
name_map <- c(
  "CA 42.8-16 Tioga parent" = "CA 42.8‑16 Tioga parent",
  "F. x ananassa NC 98-22-4" = "F. x ananassa NC 98‑22‑4",
  "Annapolis (Kentville)" = "K-Annapolis",
  "Bounty (Kentville)" = "K-Bounty",
  "Brunswick (Kentville)" = "K-Brunswick",
  "Cabot (Kentville)" = "K-Cabot",
  "Cavendish (Kentville)" = "K-Cavendish",
  "Evangeline (Kentville)" = "K-Evangeline",
  "Honeoye (Kentville)" = "K-Honeoye",
  "Jewel (Kentville)" = "K-Jewel",
  "Kent (Kentville)" = "K-Kent",
  "Laurel (Kentville)" = "K-Laurel",
  "Lila (Kentville)" = "K-Lila",
  "Mira (Kentville)" = "K-Mira",
  "Sable (Kentville)" = "K-Sable",
  "Valley Sunset (Kentville)" = "K-Valley Sunset",
  "Veestar (Kentville)" = "K-Veestar",
  "Wendy (Kentville)" = "K-Wendy",
  "NW 90054-37" = "NW 90054‑37"
)
agronomic_data <- agronomic_data %>%
  mutate(Plant_Name_clean = dplyr::recode(Plant_Name, !!!name_map))

# Now join on harmonized names
combined_data <- left_join(aov_data_means_totals,
                           agronomic_data,
                           by = c("sClone!" = "Plant_Name_clean"))

#Removing XYZ columns due to a large number of color (and highly associated to one another)
combined_data <- combined_data %>%
  select(-X, -Y, -Z)

# Set color data to NA for the two white strawberry varieties
combined_data <- combined_data %>%
  mutate(across(c(`L.`, `a.`, `b.`, `C.`, `h`), 
                ~ ifelse(`sClone!` %in% c("Weisse Anasa", "White D"), NA, .)))

# %%############################################################################# Top Aroma Producers and Radar Plot
variety_col <- "sClone!"

target_compounds <- c(
  "Butanoic_acid_butyl_ester",
  "Acetic_acid_butyl_ester",
  "Linalool",
  "%3_2H_Furanone_4_methoxy_2_5_dim",
  "%2_3H_Furanone_5_hexyldihydro"
)

other_aroma_compounds <- c(
  "Acetic_acid_hexyl_ester",
  "Butanoic_acid_2_methyl_ethyl_est"
) 

all_plot_compounds <- c(target_compounds, other_aroma_compounds)

clean_names <- c(
  "Butanoic_acid_butyl_ester" = "Butyl Butyrate",
  "Acetic_acid_butyl_ester" = "Butyl Acetate",
  "Linalool" = "Linalool",
  "%3_2H_Furanone_4_methoxy_2_5_dim" = "Mesifurane",
  "%2_3H_Furanone_5_hexyldihydro" = "γ-Decalactone",
  "Acetic_acid_hexyl_ester" = "Hexyl Acetate",
  "Butanoic_acid_2_methyl_ethyl_est" = "Ethyl 2-Methylbutyrate"
)

combined_data <- combined_data %>%
  mutate(!!sym(variety_col) := ifelse(!!sym(variety_col) == "F. x ananassa 90537-3", 
                                      "F. x a 90537-3", 
                                      !!sym(variety_col)))

top_producers_list <- list()

for (compound in target_compounds) {
  clean_compound_data <- combined_data %>%
    select(!!sym(variety_col), !!sym(compound)) %>%
    drop_na()

  threshold <- quantile(clean_compound_data[[compound]], probs = 0.985)

  top_varieties <- clean_compound_data %>%
    filter(!!sym(compound) >= threshold) %>%
    pull(!!sym(variety_col))

  top_producers_list[[compound]] <- top_varieties
}

all_mentions <- unlist(top_producers_list)
mention_counts <- table(all_mentions)
overlapping_producers <- names(mention_counts[mention_counts >= 2])

radar_data <- combined_data %>%
  select(!!sym(variety_col), all_of(all_plot_compounds)) %>%
  mutate(across(all_of(all_plot_compounds), rescale)) %>%
  filter(!!sym(variety_col) %in% overlapping_producers) %>%
  mutate(across(all_of(all_plot_compounds), ~replace_na(., 0))) %>%
  column_to_rownames(var = variety_col)

colnames(radar_data) <- clean_names[colnames(radar_data)]
colnames(radar_data) <- str_wrap(colnames(radar_data), width = 10)

radar_fmsb <- rbind(rep(1, ncol(radar_data)), rep(0, ncol(radar_data)), radar_data)

cb_palette <- c("#56B4E9", "#009E73", "#E69F00")
colors_border <- rep(cb_palette, length.out = nrow(radar_data))

specific_varieties <- c("K-Jewel", "K-Honeoye", "K-Cavendish", 
  "K-Wendy", "K-Valley Sunset",
  "Monterey", "Fronteras", "Cabrillo", "Albion",
  "Tillamook", "Hood", "Rainier", "Shuksan", "Puget Reliance",
  "Puget Crimson", "Totem", "Annapolis", "Itasca", "Jewel",
  "Honeoye", "Earliglow", "Allstar", "Cavendish", "AC Wendy",
  "Wendy", "AC Valley Sunset", "Valley Sunset", "Chandler", "Camarosa",
  "Sweet Charlie", "San Andreas", "Sweet Ann", "Camino Real", "Flavourfest",
  "Strawberry Festival", "Florida Radiance", "Sweet Sensation"
  )

radar_data_specific <- combined_data %>%
  select(!!sym(variety_col), all_of(all_plot_compounds)) %>%
  mutate(across(all_of(all_plot_compounds), rescale)) %>%
  filter(!!sym(variety_col) %in% specific_varieties) %>%
  mutate(across(all_of(all_plot_compounds), ~replace_na(., 0))) %>%
  column_to_rownames(var = variety_col)

# print row names of radar_data_specific
print(rownames(radar_data_specific))

colnames(radar_data_specific) <- clean_names[colnames(radar_data_specific)]
colnames(radar_data_specific) <- str_wrap(colnames(radar_data_specific), width = 10)

radar_fmsb_specific <- rbind(rep(1, ncol(radar_data_specific)), rep(0, ncol(radar_data_specific)), radar_data_specific)

colors_border_specific <- alpha("grey20", 0.5)

label_colors <- c(
  rep("black", length(target_compounds)), 
  rep("#ff3333", length(other_aroma_compounds))
)
label_colors_specific <- label_colors[seq_len(ncol(radar_data_specific))]

png("/Users/clayelzinga/Desktop/plots/strawberry/combined_radar_panels_dash.png",
    width = 12, height = 7, units = "in", res = 300)

par(mfrow = c(1, 2), mar = c(5, 3, 6, 3), xpd = TRUE, pty = "s")

radarchart(radar_fmsb,
           axistype = 1,
           pcol = colors_border,
           caxislabels = c(0, 25, 50, 75, 100),
           calcex = 0.8,
           pfcol = NA,
           axislabcol = "black",
           plwd = 5,
           plty = 2,
           cglcol = "grey80",
           cglty = 1,
           title = "",
           vlabels = rep("", ncol(radar_data)),
           vlcex = 1)

mtext("A", side = 3, line = 4, adj = -0.1, cex = 1.5, font = 2)

n_vars <- ncol(radar_data)
angles <- seq(pi / 2, pi / 2 + 2 * pi, length.out = n_vars + 1)[1:n_vars]
text(x = 1.3 * cos(angles), y = 1.3 * sin(angles),
     labels = colnames(radar_data),
     col = label_colors,
     cex = 1.3,
     font = 2)

italicized_labels <- lapply(rownames(radar_data), function(x) {
  if (x == "F. x a 90537-3") {
    bquote(italic("F. x a") ~ "90537-3")
  } else {
    x
  }
})

legend(x = "top", legend = as.expression(italicized_labels),
       inset = -0.25,
       title = "Top Producers",
       horiz = FALSE,
       ncol = 2,
       bty = "n",
       pch = 20,
       col = colors_border,
       text.col = "black",
       cex = 1.1,
       pt.cex = 2)

radarchart(radar_fmsb_specific,
           pcol = colors_border_specific,
           axistype = 1,
           caxislabels = c("0", "25", "50", "75", "100"),
           calcex = 0.8,
           cglcol = "grey80",
           axislabcol = "black",
           pfcol = NA,
           plwd = 5,
           plty = 2,
           cglty = 1,
           title = "",
           vlabels = rep("", ncol(radar_data_specific)),
           vlcex = 1)

mtext("B", side = 3, line = 4, adj = -0.1, cex = 1.5, font = 2)

n_vars_specific <- ncol(radar_data_specific)
angles_specific <- seq(pi / 2, pi / 2 + 2 * pi, length.out = n_vars_specific + 1)[1:n_vars_specific]
text(x = 1.3 * cos(angles_specific), y = 1.3 * sin(angles_specific),
     labels = colnames(radar_data_specific),
     col = label_colors_specific,
     cex = 1.3,
     font = 2)

dev.off()
# %%############################################################################# Origin and Status Comparisons
aroma_indices <- 2:37

stats_status <- combined_data %>%
  filter(STATUS %in% c("Cultivar", "Breeding Material")) %>%
  select(STATUS, all_of(aroma_indices)) %>%
  pivot_longer(cols = -STATUS, names_to = "Compound", values_to = "Concentration") %>%
  group_by(Compound) %>%
  summarise(
    p_value = tryCatch(wilcox.test(Concentration ~ STATUS, exact = FALSE)$p.value, error = function(e) NA),
    .groups = "drop"
  ) %>%
  mutate(p_adj = p.adjust(p_value, method = "fdr")) %>%
  filter(p_adj < 0.05) 

if(nrow(stats_status) > 0) {
  plot_data_status <- combined_data %>%
    filter(STATUS %in% c("Cultivar", "Breeding Material")) %>%
    select(STATUS, all_of(stats_status$Compound)) %>%
    pivot_longer(cols = -STATUS, names_to = "Compound", values_to = "Abundance") %>%
    left_join(stats_status, by = "Compound") %>%
    mutate(
      Compound_Clean = if(exists("voc_name_map")) coalesce(voc_name_map[Compound], Compound) else Compound,
      Facet_Label = paste0(Compound_Clean, "\n(p.adj = ", format.pval(p_adj, digits = 2), ")"),
      Comparison = "Status"
    )
} else {
  plot_data_status <- NULL
}

get_sig_plot_data <- function(df, scenario_name) {
  
  chem_matrix <- df %>%
    select(where(is.numeric)) %>%
    select(-matches("Year|Harvest|Flowering|Petal|Chl|Berry|Days|Rep|cUnit|Order|L\\.|a\\.|b\\.|C\\.|h")) %>%
    select(-starts_with("total_"))
  
  chem_names <- colnames(chem_matrix)
  results_list <- list()
  
  for(chem in chem_names) {
    k_vals <- df %>% filter(Population == "Kentville") %>% pull(chem) %>% na.omit()
    e_vals <- df %>% filter(Population == "External") %>% pull(chem) %>% na.omit()
    
    if(length(k_vals) >= 3 && length(e_vals) >= 3) {
      pval <- tryCatch(wilcox.test(k_vals, e_vals)$p.value, error = function(e) NA)
      if(!is.na(pval)) results_list[[chem]] <- data.frame(Compound = chem, P_Value = pval)
    }
  }
  
  if(length(results_list) > 0) {
    full_stats <- bind_rows(results_list) %>% mutate(FDR = p.adjust(P_Value, method = "fdr")) %>% filter(FDR < 0.05)
    
    if(nrow(full_stats) > 0) {
      df %>%
        select(Population, all_of(full_stats$Compound)) %>%
        pivot_longer(cols = -Population, names_to = "Compound", values_to = "Abundance") %>%
        left_join(full_stats, by = "Compound") %>%
        mutate(
          Compound_Clean = if(exists("voc_name_map")) coalesce(voc_name_map[Compound], Compound) else Compound,
          Facet_Label = paste0(Compound_Clean, "\n(p.adj = ", format.pval(FDR, digits = 2), ")"),
          Comparison = scenario_name
        )
    } else NULL
  } else NULL
}

df_breeding <- combined_data %>%
  filter(STATUS == "Breeding Material") %>%
  mutate(Population = ifelse(ORIGIN.x == "Canada" & str_detect(NARRATIVE, "Nova Scotia|Kentville"), "Kentville", "External"))

df_cultivar <- combined_data %>%
  filter(STATUS == "Cultivar") %>%
  mutate(Population = ifelse(ORIGIN.x == "Canada" & (str_detect(NARRATIVE, "Nova Scotia|Kentville") | `sClone!` %in% c("Micmac", "Evangeline")), "Kentville", "External"))

plot_data_breeding <- get_sig_plot_data(df_breeding, "Breeding Material")
plot_data_cultivar <- get_sig_plot_data(df_cultivar, "Cultivars")

create_individual_plots <- function(data, x_col, colors) {
  if(is.null(data) || nrow(data) == 0) return(list())
  
  data$X_VAR <- data[[x_col]]
  plot_list <- list()
  
  for(f_label in unique(data$Facet_Label)) {
    sub_data <- data[data$Facet_Label == f_label, ]
    
    p <- ggplot(sub_data, aes(x = X_VAR, y = Abundance, fill = X_VAR)) +
      geom_boxplot(width = 0.4, outlier.shape = NA, alpha = 0.2, size = 0.5, color = "grey40") +
      geom_quasirandom(alpha = 0.8, size = 2.0, cex = 2, shape = 16, color = "black") +
      facet_wrap(~ Facet_Label, scales = "free") +
      scale_y_continuous(expand = expansion(mult = c(0.05, 0.2))) +
      scale_fill_manual(values = colors) +
      theme_bw(base_size = 18) +
      labs(y = "Abundance (ng/g)", x = NULL) +
      theme(
        legend.position = "none",
        axis.text.x = element_text(face = "bold", size = 18),
        strip.text = element_text(face = "bold", size = 18),
        strip.background = element_rect(fill = "grey95"),
        panel.grid.minor = element_blank()
      )
    
    plot_list[[f_label]] <- p
  }
  
  return(plot_list)
}

cols_pop <- c("Kentville" = "#E69F00", "External" = "#004949")
cols_stat <- c("Breeding Material" = "#CC79A7", "Cultivar" = "#0072B2")

p1_list <- create_individual_plots(plot_data_breeding, "Population", cols_pop)
p2_list <- create_individual_plots(plot_data_cultivar, "Population", cols_pop)
p3_list <- create_individual_plots(plot_data_status,   "STATUS",     cols_stat)

all_plots <- c(p1_list, p2_list, p3_list)

if(length(all_plots) > 0) {
  
  final_figure <- wrap_plots(all_plots, nrow = 1) + 
    plot_annotation(tag_levels = 'A') & 
    theme(plot.tag = element_text(size = 18, face = "bold"))
  
  print(final_figure)
  
  ggsave("strawberry_aroma_ALL_comparisons.png", final_figure, width = 18, height = 6, dpi = 300)
  
} else {
  print("No significant results found in any comparison.")
}


# %%############################################################################# Agronomic Correlations
aroma_cols <- c(analysis_cols, "total_Aroma_Compounds")
agronomic_cols <- c("Harvest_Date", "Berry_Wt")
color_cols <- c("L.", "a.")

aroma_data <- combined_data[, aroma_cols, drop = FALSE]
agronomic_data <- combined_data[, c(agronomic_cols, color_cols), drop = FALSE]

aroma_data <- as.data.frame(lapply(aroma_data, as.numeric))
agronomic_data <- as.data.frame(lapply(agronomic_data, as.numeric))

cross_cor <- cor(aroma_data, agronomic_data, use = "pairwise.complete.obs")

cor_results <- rcorr(as.matrix(aroma_data), as.matrix(agronomic_data))

p_matrix <- cor_results$P[rownames(cross_cor), colnames(cross_cor)]

n_tests <- ncol(aroma_data) * ncol(agronomic_data)

p_values_vector <- as.vector(p_matrix)
p_adj_vector <- p.adjust(p_values_vector, method = "BH", n = n_tests)
p_adj_matrix <- matrix(p_adj_vector, nrow = nrow(p_matrix), ncol = ncol(p_matrix))

dimnames(p_adj_matrix) <- dimnames(p_matrix)

library(gridExtra)
library(cowplot)

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
      Agro_Value = agronomic_data[[raw_agro_name]]
    )
    
    x_center <- mean(range(temp_df$Aroma_Value, na.rm = TRUE))
    stat_label <- paste0("r = ", r_val, " | p-FDR = ", p_val)
    
    p_individual <- ggplot(temp_df, aes(x = Aroma_Value, y = Agro_Value)) +
        geom_point(alpha = 0.6, size = 2) +
        geom_smooth(method = "lm", color = plot_color, se = FALSE, linewidth = 1) +
        annotate("text", x = Inf, y = Inf, label = stat_label, 
           vjust = 1.5, hjust = 1.1, fontface = "bold", size = 5) +
        theme_bw() +
        theme(
              axis.title = element_text(size = 16, face = "bold"),
              axis.text = element_text(size = 12),
              panel.grid.minor = element_blank()
              ) +
  labs(
    x = paste(display_aroma, "(Abundance)"),
    y = clean_agro_name
  )
    
    if (r_raw > 0) {
      pos_plots[[length(pos_plots) + 1]] <- p_individual
    } else {
      neg_plots[[length(neg_plots) + 1]] <- p_individual
    }
  }
  
  png("/Users/clayelzinga/Desktop/plots/strawberry/aroma_agronomic_clean_names.png", 
      width = 18, height = 11, units = "in", res = 300)
  
  combined_plots <- c(pos_plots, list(NULL), neg_plots)
  
  final_plot <- plot_grid(
    plotlist = combined_plots, 
    ncol = 3, 
    nrow = 2,
    labels = c("A", "B", "", "C", "D", "E"), 
    align = "hv", 
    axis = "tblr"
  )
  
  print(final_plot)
  
  dev.off()
  print("Success: Plot created with equally sized panels, grouped correlations, and inner text.")

} else {
  print("No correlations met the FDR significance threshold.")
}

# %%############################################################################# first linear model 
safe_name <- function(x) gsub("[^A-Za-z0-9._-]", "_", gsub("%","pct_", x))

combined_data_STATUS <- combined_data %>%
  filter(STATUS == "Cultivar") %>%
  mutate(YEAR_ORIGIN = as.numeric(YEAR_ORIGIN))

model_summaries <- aroma_cols %>%
  set_names() %>%
  map_dfr(function(aroma) {
    df <- combined_data_STATUS %>%
      transmute(
        YEAR_ORIGIN,
        aroma_value = as.numeric(.data[[aroma]])
      ) %>%
      drop_na(YEAR_ORIGIN, aroma_value)

    year_var <- stats::var(df$YEAR_ORIGIN)
    
    if (nrow(df) < 3 || is.na(year_var) || year_var == 0) {
      return(tibble(
        compound = aroma,
        estimate = NA_real_,
        raw_p_value = NA_real_,
        r_squared = NA_real_
      ))
    }

    fit <- lm(aroma_value ~ YEAR_ORIGIN, data = df)
    
    fit_tidy <- tidy(fit) %>% filter(term == "YEAR_ORIGIN")
    fit_glance <- glance(fit)

    tibble(
      compound = aroma,
      estimate = fit_tidy$estimate,
      raw_p_value = fit_tidy$p.value,
      r_squared = fit_glance$r.squared
    )
  })

final_results <- model_summaries %>%
  drop_na(raw_p_value) %>%
  mutate(
    fdr_p_value = p.adjust(raw_p_value, method = "fdr")
  ) %>%
  arrange(raw_p_value) %>%
  select(compound, estimate, r_squared, raw_p_value, fdr_p_value)

print(final_results)

# %%############################################################################# finding covariates for the linear model
potential_covariates <- c(
  "Flowering_Date", "Petal_Number", "Chl..June.", "Chl..July.", 
  "Chl..Aug.", "Harvest_Date", "Days_Ripe", "Berry_Wt", 
  "L.", "a.", "b.", "C.", "h"
)

covariate_testing <- map_dfr(aroma_cols, function(aroma) {
  map_dfr(potential_covariates, function(cov) {
    
    df <- combined_data_STATUS %>%
      select(aroma_value = all_of(aroma), cov_value = all_of(cov)) %>%
      drop_na()
    
    if (nrow(df) < 10 || stats::var(df$cov_value) == 0) {
      return(tibble(
        compound = aroma, 
        covariate = cov, 
        r_squared = NA_real_
      ))
    }
    
    fit <- lm(aroma_value ~ cov_value, data = df)
    
    tibble(
      compound = aroma,
      covariate = cov,
      r_squared = glance(fit)$r.squared
    )
  })
})

top_global_covariates <- covariate_testing %>%
  drop_na(r_squared) %>%
  group_by(covariate) %>%
  summarise(
    mean_r_squared = mean(r_squared),
    max_r_squared = max(r_squared)
  ) %>%
  arrange(desc(mean_r_squared))

print(top_global_covariates)


# %%############################################################################# testing with top two covariates
combined_data_STATUS <- combined_data %>%
  filter(STATUS == "Cultivar") %>%
  mutate(
    YEAR_ORIGIN = as.numeric(YEAR_ORIGIN),
    Harvest_Date = as.numeric(Harvest_Date),
    Berry_Wt = as.numeric(Berry_Wt)
  )

model_summaries_complex <- aroma_cols %>%
  set_names() %>%
  map_dfr(function(aroma) {
    df <- combined_data_STATUS %>%
      transmute(
        YEAR_ORIGIN,
        Harvest_Date,
        Berry_Wt,
        aroma_value = as.numeric(.data[[aroma]])
      ) %>%
      drop_na(YEAR_ORIGIN, Harvest_Date, Berry_Wt, aroma_value)

    year_var <- stats::var(df$YEAR_ORIGIN)
    
    if (nrow(df) < 4 || is.na(year_var) || year_var == 0) {
      return(tibble(
        compound = aroma,
        estimate = NA_real_,
        raw_p_value = NA_real_,
        r_squared = NA_real_
      ))
    }

    fit <- lm(aroma_value ~ YEAR_ORIGIN + Harvest_Date + Berry_Wt, data = df)
    
    fit_tidy <- tidy(fit) %>% filter(term == "YEAR_ORIGIN")
    fit_glance <- glance(fit)

    tibble(
      compound = aroma,
      estimate = fit_tidy$estimate,
      raw_p_value = fit_tidy$p.value,
      r_squared = fit_glance$r.squared
    )
  })

final_complex_results <- model_summaries_complex %>%
  drop_na(raw_p_value) %>%
  mutate(
    fdr_p_value = p.adjust(raw_p_value, method = "fdr")
  ) %>%
  arrange(raw_p_value) %>%
  select(compound, estimate, r_squared, raw_p_value, fdr_p_value)

print(final_complex_results)
# %%#############################################################################

