
library(tidyverse)

df <- readr::read_delim("data/cleaned.csv")


# Creating ki_usage indicator based on Q4 and Q10 instead of Q3 because:
# - Q3 ("I actively use AI applications") was not mandatory and is subjective
# - Q4 ("Areas where I use AI") and Q10 ("AI applications I use") were mandatory
#   and provide better behavioral indicators through concrete usage examples
# - Using both Q4 and Q10 provides cross-validation of AI usage
# - Removing inconsistent cases where:
#   * All Q4 options are FALSE but one of Q10 was selected
#   * All Q10 options are FALSE but one of Q4 was selected
df <- df %>% 
  mutate(
    ki_usage = if_any(c(starts_with("Q4_") & !matches("open$"),
                        starts_with("Q10_") & !matches("open$")),
                      ~ . == TRUE)
  ) %>%
  filter(
    !(if_all(starts_with("Q4_") & !matches("open$"), ~ . == FALSE) & 
      if_any(starts_with("Q10_") & !matches("open$"), ~ . == TRUE)),
    !(if_all(starts_with("Q10_") & !matches("open$"), ~ . == FALSE) & 
      if_any(starts_with("Q4_") & !matches("open$"), ~ . == TRUE))
  )

# Creating binary variable for Q9 
df <- df %>%
  mutate(
    Q9_binary = if_else(Q9 > 50, TRUE, FALSE, missing = NA)
  )

# Creating binary variable for Q17 
df <- df %>%
  mutate(
    Q17_binary = if_else(Q9 > 50, TRUE, FALSE, missing = NA)
  )


llm_apps_list <- c(
  "ChatGPT (kostenlose Version)", 
  "ChatGPT (kostenpflichtige Version)", 
  "Claude", 
  "Gemini (Google)", 
  "Perplexity"
)


df <- df %>%
  mutate(
    llm_usage = if_else(
      rowSums(sapply(1:nrow(Q10_Options_map), function(i) {
        Q10_Options_map$label[i] %in% llm_apps_list & get(Q10_Options_map$question[i]) == TRUE
      })) > 0,
      1, 0
    )
  )

readr::write_csv(df, "data/final.csv")