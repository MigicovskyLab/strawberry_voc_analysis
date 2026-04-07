library(tidyverse)
library(readxl)
library(ggbeeswarm)
library(patchwork)

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

aroma_indices <- 2:37
stats_status <- combined_data %>%
  filter(STATUS %in% c("Cultivar", "Breeding Material")) %>%
  select(STATUS, all_of(aroma_indices)) %>%
  pivot_longer(cols = -STATUS, names_to = "Compound", values_to = "Concentration") %>%
  group_by(Compound) %>%
  summarise(p_value = tryCatch(wilcox.test(Concentration ~ STATUS, exact = FALSE)$p.value, error = function(e) NA), .groups = "drop") %>%
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
  chem_matrix <- df %>% select(where(is.numeric)) %>% select(-matches("Year|Harvest|Flowering|Petal|Chl|Berry|Days|Rep|cUnit|Order|L\\.|a\\.|b\\.|C\\.|h")) %>% select(-starts_with("total_"))
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
      df %>% select(Population, all_of(full_stats$Compound)) %>% pivot_longer(cols = -Population, names_to = "Compound", values_to = "Abundance") %>% left_join(full_stats, by = "Compound") %>% mutate(Compound_Clean = if(exists("voc_name_map")) coalesce(voc_name_map[Compound], Compound) else Compound, Facet_Label = paste0(Compound_Clean, "\n(p.adj = ", format.pval(FDR, digits = 2), ")"), Comparison = scenario_name)
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
    labels_with_n <- sub_data %>% group_by(X_VAR) %>% summarise(n = sum(!is.na(Abundance)), .groups = "drop") %>% mutate(label = paste0(gsub("Breeding Material", "Breeding\nMaterial", X_VAR), "\n(n=", n, ")"))
    label_map <- setNames(labels_with_n$label, labels_with_n$X_VAR)
    p <- ggplot(sub_data, aes(x = X_VAR, y = Abundance, fill = X_VAR)) + geom_boxplot(width = 0.4, outlier.shape = NA, alpha = 0.2, size = 0.5, color = "grey40") + geom_quasirandom(alpha = 0.8, size = 2.0, shape = 16, color = "black") + facet_wrap(~ Facet_Label, scales = "free") + scale_y_continuous(expand = expansion(mult = c(0.05, 0.2))) + scale_fill_manual(values = colors) + scale_x_discrete(labels = label_map) + theme_bw(base_size = 28) + labs(y = expression(Abundance~(ng~g^-1)), x = NULL) + theme(legend.position = "none", axis.text.x = element_text(face = "bold", size = 28), strip.text = element_text(face = "bold", size = 28), strip.background = element_rect(fill = "grey95"), panel.grid.minor = element_blank())
    plot_list[[f_label]] <- p
  }
  return(plot_list)
}

cols_pop <- c("Kentville" = "#E69F00", "External" = "#004949")
cols_stat <- c("Breeding Material" = "#CC79A7", "Cultivar" = "#0072B2")

p1_list <- create_individual_plots(plot_data_breeding, "Population", cols_pop)
p2_list <- create_individual_plots(plot_data_cultivar, "Population", cols_pop)
p3_list <- create_individual_plots(plot_data_status,   "STATUS",     cols_stat)

all_plots <- c(p2_list, p3_list, p1_list)

if(length(all_plots) > 0) {
  final_figure <- wrap_plots(all_plots, nrow = 1) + plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(size = 28, face = "bold"))
  ggsave("strawberry_aroma_ALL_comparisons.png", final_figure, width = 18, height = 6, dpi = 300)
}