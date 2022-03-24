
# See data
# https://fdaaa.trialstracker.net/?status%5B%5D=overdue&status%5B%5D=overdue-cancelled&status%5B%5D=ongoing&status%5B%5D=reported&status%5B%5D=reported-late

# Download all data
# https://fdaaa.trialstracker.net/api/trials.csv?draw=1&columns%5B0%5D%5Bdata%5D=status&columns%5B0%5D%5Bname%5D=status&columns%5B0%5D%5Bsearchable%5D=true&columns%5B0%5D%5Borderable%5D=true&columns%5B0%5D%5Bsearch%5D%5Bvalue%5D=&columns%5B0%5D%5Bsearch%5D%5Bregex%5D=false&columns%5B1%5D%5Bdata%5D=sponsor_name&columns%5B1%5D%5Bname%5D=sponsor__name&columns%5B1%5D%5Bsearchable%5D=true&columns%5B1%5D%5Borderable%5D=true&columns%5B1%5D%5Bsearch%5D%5Bvalue%5D=&columns%5B1%5D%5Bsearch%5D%5Bregex%5D=false&columns%5B2%5D%5Bdata%5D=registry_id&columns%5B2%5D%5Bname%5D=registry_id&columns%5B2%5D%5Bsearchable%5D=true&columns%5B2%5D%5Borderable%5D=true&columns%5B2%5D%5Bsearch%5D%5Bvalue%5D=&columns%5B2%5D%5Bsearch%5D%5Bregex%5D=false&columns%5B3%5D%5Bdata%5D=title&columns%5B3%5D%5Bname%5D=title&columns%5B3%5D%5Bsearchable%5D=true&columns%5B3%5D%5Borderable%5D=true&columns%5B3%5D%5Bsearch%5D%5Bvalue%5D=&columns%5B3%5D%5Bsearch%5D%5Bregex%5D=false&columns%5B4%5D%5Bdata%5D=completion_date&columns%5B4%5D%5Bname%5D=completion_date&columns%5B4%5D%5Bsearchable%5D=true&columns%5B4%5D%5Borderable%5D=true&columns%5B4%5D%5Bsearch%5D%5Bvalue%5D=&columns%5B4%5D%5Bsearch%5D%5Bregex%5D=false&columns%5B5%5D%5Bdata%5D=days_late&columns%5B5%5D%5Bname%5D=days_late&columns%5B5%5D%5Bsearchable%5D=true&columns%5B5%5D%5Borderable%5D=true&columns%5B5%5D%5Bsearch%5D%5Bvalue%5D=&columns%5B5%5D%5Bsearch%5D%5Bregex%5D=false&order%5B0%5D%5Bcolumn%5D=4&order%5B0%5D%5Bdir%5D=desc&start=0&search%5Bvalue%5D=&search%5Bregex%5D=false&status%5B%5D=overdue&status%5B%5D=overdue-cancelled&status%5B%5D=reported-late&format=csv


# Libraries ---------------------------------------------------------------

packages <- c("cli", "dplyr", "DT", "ggplot", "readr", "plotly", "purrr", "tidyr", "tidytext")
lapply(packages, require , character.only = TRUE)
source("R/proportion_overdue.R")


# Process data ------------------------------------------------------------

DF = read_csv("data/trials.csv")
# names(DF)
# DT::datatable(DF)



# Most common words -------------------------------------------------------


DF_words <- DF %>% unnest_tokens(output = word, input = title)
DF_words <- DF_words %>% anti_join(stop_words, by = "word")
DF_wordcounts <- DF_words %>% count(word, sort = TRUE)
# DF_wordcounts %>% View()


# Sponsors ----------------------------------------------------------------

# Calculate proportion of overdue trials by sponsor
DATA_sponsors = DF %>% 
  filter(! status %in% c("ongoing", "overdue-cancelled")) %>% 
  count(status, sponsor_name) %>% 
  pivot_wider(names_from = status, values_from = n) %>% 
  mutate(total_n = overdue + reported + `reported-late`,
         PROP_overdue = round(overdue / total_n, 2)) %>% 
  arrange(desc(PROP_overdue)) 
  
# Plot
PLOT_sponsors = 
  DATA_sponsors %>% 
  ggplot(aes(total_n, PROP_overdue, label = sponsor_name)) + 
  geom_hline(yintercept = .248, linetype = "dashed") + 
  geom_point(aes(size = total_n), show.legend = FALSE) +
  ggrepel::geom_label_repel(size = 3, max.overlaps = 5, nudge_x = 10, nudge_y = 0.02) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "white")) + 
  # scale_x_log10() +
  labs(title = "Propotion of overdue trials by Sponsor",
       x = "Total trials",
       y = "Proportion of overdue trials",
       caption = "Excluding ongoing and cancelled trials")


PLOT_sponsors
ggsave("output/PLOT_sponsors.png", PLOT_sponsors, height = 9, width = 16)
# plotly::ggplotly(PLOT_sponsors)



# Proportion overdue ------------------------------------------------------
# proportion_overdue("cancer")

terms = c("Adenocarcinoma", "AIDS", "Alzheimer's", "Bladder", "Brain", "Breast", "Cancer", "Carcinoma", "Cardiovascular", "Cell", "Chemotherapy", "Children", "Chronic", "Cognitive", "Colorectal", "COVID", "Depression", "Diabetes", "Eye", "Fibrosis", "Heart", "HIV", "Hypertension", "Influenza", "Kidney", "Leukemia", "Liver", "Lymphoma", "Melanoma", "Men", "Myeloma", "Migraine", "Opioid", "Ovarian", "Pain", "Pancreatic", "Postmenopausal", "Prostate", "Psoriasis", "Radiotherapy", "SARS", "Sclerosis", "Sleep", "Stroke", "Transplantation", "Women")

# Calculate proportion overdue for all the terms
DF_ALL = map(terms, proportion_overdue)


# Join all TABLEs
DF_ALL_TABLE = 1:length(DF_ALL) %>% map_dfr(~ DF_ALL[[.x]]$TABLE)
# DF_ALL_DF_filtered = 1:length(DF_ALL) %>% map_dfr(~ DF_ALL[[.x]]$DF_filtered)

# Create plot
PLOT_proportion_overdue = 
  DF_ALL_TABLE %>% 
  filter(key_present == TRUE) %>% 
  ggplot(aes(total_n, PROP_overdue, size = total_n, label = key_word)) + 
  geom_hline(yintercept = .248, linetype = "dashed") + 
  geom_point() +
  ggrepel::geom_label_repel(size = 3, max.overlaps = 5) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "white")) + 
  labs(title = "Propotion of overdue trials by keyword",
       x = "Total trials",
       y = "Proportion of overdue trials",
       caption = "Excluding ongoing and cancelled trials")

PLOT_proportion_overdue

ggsave("output/PLOT_proportion_overdue.png", PLOT_proportion_overdue, height = 9, width = 16)
