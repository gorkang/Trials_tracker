
proportion_overdue <- function(key_word) {
  # key_word = "breast cancer"
  
  DF_filtered = DF %>% 
    filter(! status %in% c("ongoing", "overdue-cancelled")) %>% 
    mutate(key_present = grepl(paste0("\\b", key_word, "\\b"), title, ignore.case = TRUE),
           key_word = key_word) %>% 
    select(key_word, key_present, everything())
  
  TABLE = DF_filtered %>% 
    count(status, key_present) %>% 
    pivot_wider(names_from = status, values_from = n) %>% 
    mutate(key_word = key_word,
           total_n = overdue + reported + `reported-late`,
           PROP_overdue = overdue / total_n) %>% 
    select(key_word, key_present, everything())
  
  cli::cli_alert(paste0(key_word, ": ", paste(round(TABLE$PROP_overdue, 2), collapse = " vs ")))
  return(list(DF_filtered = DF_filtered,
              TABLE = TABLE))
  
}
