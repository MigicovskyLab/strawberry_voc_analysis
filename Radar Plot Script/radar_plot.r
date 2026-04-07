library(tidyverse)
library(readxl)
library(fmsb)
library(scales)

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

aov_data_means_totals <- aov_data_means

agronomic_data <- read_csv("strawberry_agronomic_data.csv")

name_map <- c("CA 42.8-16 Tioga parent" = "CA 42.8‑16 Tioga parent", "F. x ananassa NC 98-22-4" = "F. x ananassa NC 98‑22‑4", "Annapolis (Kentville)" = "K-Annapolis", "Bounty (Kentville)" = "K-Bounty", "Brunswick (Kentville)" = "K-Brunswick", "Cabot (Kentville)" = "K-Cabot", "Cavendish (Kentville)" = "K-Cavendish", "Evangeline (Kentville)" = "K-Evangeline", "Honeoye (Kentville)" = "K-Honeoye", "Jewel (Kentville)" = "K-Jewel", "Kent (Kentville)" = "K-Kent", "Laurel (Kentville)" = "K-Laurel", "Lila (Kentville)" = "K-Lila", "Mira (Kentville)" = "K-Mira", "Sable (Kentville)" = "K-Sable", "Valley Sunset (Kentville)" = "K-Valley Sunset", "Veestar (Kentville)" = "K-Veestar", "Wendy (Kentville)" = "K-Wendy", "NW 90054-37" = "NW 90054‑37")

agronomic_data <- agronomic_data %>% mutate(Plant_Name_clean = dplyr::recode(Plant_Name, !!!name_map))

combined_data <- left_join(aov_data_means_totals, agronomic_data, by = c("sClone!" = "Plant_Name_clean")) %>%
  select(-X, -Y, -Z) %>%
  mutate(across(c(`L.`, `a.`, `b.`, `C.`, `h`), ~ ifelse(`sClone!` %in% c("Weisse Anasa", "White D"), NA, .)))

variety_col <- "sClone!"

target_compounds <- c("Butanoic_acid_butyl_ester", "Acetic_acid_butyl_ester", "Linalool", "%3_2H_Furanone_4_methoxy_2_5_dim", "%2_3H_Furanone_5_hexyldihydro")
other_aroma_compounds <- c("Acetic_acid_hexyl_ester", "Butanoic_acid_2_methyl_ethyl_est")
all_plot_compounds <- c(target_compounds, other_aroma_compounds)

clean_names <- c("Butanoic_acid_butyl_ester" = "Butyl Butyrate", "Acetic_acid_butyl_ester" = "Butyl Acetate", "Linalool" = "Linalool", "%3_2H_Furanone_4_methoxy_2_5_dim" = "Mesifurane", "%2_3H_Furanone_5_hexyldihydro" = "γ-Decalactone", "Acetic_acid_hexyl_ester" = "Hexyl Acetate", "Butanoic_acid_2_methyl_ethyl_est" = "Ethyl 2-Methylbutyrate")

combined_data <- combined_data %>%
  mutate(!!sym(variety_col) := ifelse(!!sym(variety_col) == "F. x ananassa 90537-3", "F. x a 90537-3", !!sym(variety_col)))

top_producers_list <- list()

for (compound in target_compounds) {
  clean_compound_data <- combined_data %>% select(!!sym(variety_col), !!sym(compound)) %>% drop_na()
  threshold <- quantile(clean_compound_data[[compound]], probs = 0.985)
  top_varieties <- clean_compound_data %>% filter(!!sym(compound) >= threshold) %>% pull(!!sym(variety_col))
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

specific_varieties <- c("K-Jewel", "K-Honeoye", "K-Cavendish", "K-Wendy", "K-Valley Sunset", "Monterey", "Fronteras", "Cabrillo", "Albion", "Tillamook", "Hood", "Rainier", "Shuksan", "Puget Reliance", "Puget Crimson", "Totem", "Annapolis", "Itasca", "Jewel", "Honeoye", "Earliglow", "Allstar", "Cavendish", "AC Wendy", "Wendy", "AC Valley Sunset", "Valley Sunset", "Chandler", "Camarosa", "Sweet Charlie", "San Andreas", "Sweet Ann", "Camino Real", "Flavourfest", "Strawberry Festival", "Florida Radiance", "Sweet Sensation")

radar_data_specific <- combined_data %>%
  select(!!sym(variety_col), all_of(all_plot_compounds)) %>%
  mutate(across(all_of(all_plot_compounds), rescale)) %>%
  filter(!!sym(variety_col) %in% specific_varieties) %>%
  mutate(across(all_of(all_plot_compounds), ~replace_na(., 0))) %>%
  column_to_rownames(var = variety_col)

colnames(radar_data_specific) <- clean_names[colnames(radar_data_specific)]
colnames(radar_data_specific) <- str_wrap(colnames(radar_data_specific), width = 10)

radar_fmsb_specific <- rbind(rep(1, ncol(radar_data_specific)), rep(0, ncol(radar_data_specific)), radar_data_specific)

colors_border_specific <- rep(alpha("grey20", 0.5), nrow(radar_data_specific))
colors_border_specific[rownames(radar_data_specific) == "Itasca"] <- "#D55E00"
colors_border_specific[rownames(radar_data_specific) == "Sweet Charlie"] <- "#0072B2"

line_widths_specific <- rep(3, nrow(radar_data_specific))
line_widths_specific[rownames(radar_data_specific) %in% c("Itasca", "Sweet Charlie")] <- 5

label_colors <- c(rep("black", length(target_compounds)), rep("#ff3333", length(other_aroma_compounds)))
label_colors_specific <- label_colors[seq_len(ncol(radar_data_specific))]

png("combined_radar_panels_dash.png", width = 12, height = 7, units = "in", res = 300)
par(mfrow = c(1, 2), mar = c(9, 3, 6, 3), xpd = TRUE, pty = "s")

radarchart(radar_fmsb, axistype = 1, pcol = colors_border, caxislabels = c(0, 25, 50, 75, 100), calcex = 0.8, pfcol = NA, axislabcol = "black", plwd = 5, plty = 2, cglcol = "grey80", cglty = 1, title = "", vlabels = rep("", ncol(radar_data)), vlcex = 1)
mtext("A", side = 3, line = 4, adj = -0.1, cex = 1.5, font = 2)
n_vars <- ncol(radar_data)
angles <- seq(pi / 2, pi / 2 + 2 * pi, length.out = n_vars + 1)[1:n_vars]
text(x = 1.3 * cos(angles), y = 1.3 * sin(angles), labels = colnames(radar_data), col = label_colors, cex = 1.3, font = 2)

italicized_labels <- lapply(rownames(radar_data), function(x) { if (x == "F. x a 90537-3") bquote(italic("F. x a") ~ "90537-3") else x })
legend(x = "bottom", legend = as.expression(italicized_labels), inset = -0.25, title = "Top Producers", horiz = FALSE, ncol = 2, bty = "n", pch = 20, col = colors_border, text.col = "black", cex = 1.1, pt.cex = 2)

radarchart(radar_fmsb_specific, pcol = colors_border_specific, axistype = 1, caxislabels = c("0", "25", "50", "75", "100"), calcex = 0.8, cglcol = "grey80", axislabcol = "black", pfcol = NA, plwd = line_widths_specific, plty = 2, cglty = 1, title = "", vlabels = rep("", ncol(radar_data_specific)), vlcex = 1)
mtext("B", side = 3, line = 4, adj = -0.1, cex = 1.5, font = 2)
n_vars_specific <- ncol(radar_data_specific)
angles_specific <- seq(pi / 2, pi / 2 + 2 * pi, length.out = n_vars_specific + 1)[1:n_vars_specific]
text(x = 1.3 * cos(angles_specific), y = 1.3 * sin(angles_specific), labels = colnames(radar_data_specific), col = label_colors_specific, cex = 1.3, font = 2)
legend(x = "bottom", legend = c("Itasca", "Sweet Charlie"), inset = -0.25, title = "Selected Varieties", horiz = FALSE, ncol = 2, bty = "n", pch = 20, col = c("#D55E00", "#0072B2"), text.col = "black", cex = 1.1, pt.cex = 2)
dev.off()