library(tidyverse)
library(here)
source(here("helpers.R"))
source(here("config.R"))

df <- readr::read_delim("data/results.csv", delim = ";")

df <- df %>%
  rename(id = ID)

df <- df %>%
  rename(date = Datum) %>%
  mutate(date = ymd(date))

df <- df %>%
  rename_with(~ str_replace_all(., "Frage ", "Q"), everything())

# Q3: Bool
df <- process_bool(df, 3, Q3_Text, positive_val = "Ja")

# Q4 - Multiple Choice
df <- process_mult_open(df, 4, Q4_Text, Q4_Options, booleanize = TRUE)

# Q5 - Rank
df <- process_mult_open(df, 5, Q5_Text, Q5_Options, booleanize = FALSE)

# Q6 - Likert Slider
df <- process_slider_ot(df, 6, Q6_Text)

# Q7 - Likert Slider
df <- process_slider_ot(df, 7, Q7_Text)

# Q8 - Multiple Likert Sliders
df <- process_mult_open(df, 8, Q8_Text, Q8_Options,
  booleanize = FALSE,
  open_col_text = paste0("Eingabefeld von ", Q8_Text, " - Sonstiges und zwar:")
)

# Q9 - Likert Slider
df <- process_slider_ot(df, 9, Q9_Text)

# Q10 - Multiple Choice
df <- process_mult_open(df, 10, Q10_Text, Q10_Options,
  booleanize = TRUE,
  open_col_text = "Eingabe für Sonstige und zwar:",
  other_col_text = "Sonstige und zwar:"
)

# Q11 - Open
df <- process_slider_ot(df, 11, Q11_Text)

# Q12 - Rank
df <- process_mult_open(df, 12, Q12_Text, Q12_Options,
  booleanize = FALSE,
  open_col_text = "Eingabe für Sonstiges und zwar"
)

# Q13 - Likert Slider -> No NA, 50 can mean no answer
df <- process_slider_ot(df, 13, Q13_Text)

# Q15 - Likert Slider -> No NA, 50 can mean no answer
df <- process_slider_ot(df, 14, Q14_Text)

# Q15 - Open
df <- process_slider_ot(df, 15, Q15_Text)

# Q16 - Multiple Choice
df <- process_mult_open(df, 16, Q16_Text, Q16_Options,
  booleanize = TRUE,
  open_col_text = "", other_col_text = ""
)

# Q17 - Likert Slider
df <- process_slider_ot(df, 17, Q17_Text)

# Q18 - Multiple Choice
df <- process_mult_open(
  df, 18, Q18_Text, Q18_Options,
  booleanize = TRUE, open_col_text = "Eingabe für Sonstiges:",
  other_col_text = "Sonstiges:"
)

df <- df %>%
  select(id, date, matches("^Q[0-9](_|$)"))

readr::write_csv(df, "data/cleaned.csv")

