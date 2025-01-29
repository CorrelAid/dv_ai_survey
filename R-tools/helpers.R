#' Data Processing Helper Functions
#'
#' This file contains a set of functions designed to process survey data.
#' The functions transform data columns based on different question types,
# #' including bool, rank, multiple choice, sliders, and single choice questions.
#' Each function takes a DataFrame as input, processes specific columns based
#' on the question type, and returns the transformed DataFrame.
#'


# Function to process boolean columns based on a given question and positive value.
#
# @param df DataFrame to be processed.
# @param question_number Integer representing the question number.
# @param question_text Character string of the question text.
# @param positive_val The value considered as "positive" for converting the column into TRUE/FALSE.
#
# @return DataFrame with the booleanized column based on the question.
process_bool <- function(df, question_number, question_text, positive_val) {
  question_name <- paste0("Q", question_number)
  old_question_col <- paste0(question_name, " - ", question_text)

  df <- df %>%
    dplyr::rename(!!question_name := old_question_col) %>%
    dplyr::mutate(!!question_name :=
      !!dplyr::sym(question_name) == positive_val)

  return(df)
}


#' Process Questions with multiple options that can include open text fields
#'
#' This function processes a dataframe containing multiple-choice questions, multiple likert sliders or rank questions, with an option for open-ended responses.
#' It also supports converting multiple-choice answers to boolean values and mapping answer options to numerical values.
#'
#' @param df Dataframe containing the survey data.
#' @param question_number The question number for which multiple-choice answers are being processed.
#' @param question_text The text of the question being processed (not used in the function, included for clarity).
#' @param options A vector of answer options for the multiple-choice question.
#' @param booleanize A logical value (default: FALSE) indicating whether to convert the multiple-choice responses to boolean (TRUE/FALSE) values.
#' @param open_col_text The text to look for in the open-ended column. Default is "Eingabe für Sonstiges und zwar:".
#' @param other_col_text The text for another open-ended column. Default is "Sonstiges und zwar:".
#'
#' @return A dataframe with the processed multiple-choice and open-ended columns.
process_mult_open <- function(
    df, question_number, question_text, options,
    booleanize = FALSE, open_col_text = "Eingabe für Sonstiges und zwar:",
    other_col_text = "Sonstiges und zwar:") {
  # Create the question name based on the question number
  question_name <- paste0("Q", question_number)

  # Handle open-ended columns
  if (open_col_text != "" && other_col_text != "") {
    question_open_col <- paste0("Q", question_number, " - ", open_col_text)
    new_question_open_col <- paste0("Q", question_number, "_open")

    df <- df %>%
      dplyr::rename(!!new_question_open_col := question_open_col)
  }

  # If booleanize is TRUE, convert multiple choice columns to logical (TRUE/FALSE)
 if (booleanize) {
  df <- df %>%
    dplyr::mutate(across(
      .cols = dplyr::starts_with(question_name) & !dplyr::matches("_open"),
      .fns = ~ ifelse(is.na(.), FALSE, TRUE)
    ))
}

  # Map options to numerical values and rename columns accordingly
  options_mapping <- setNames(1:length(options), options)

  # Function to transform column names based on options mapping
  # causes the number after the _ to belong to the index in the options list
  transform_names <- function(col_names) {
    sapply(col_names, function(col) {
      match <- names(options_mapping)[sapply(
        names(options_mapping),
        function(option) grepl(option, col, fixed = TRUE)
      )]
      if (length(match) > 0) {
        return(paste0(question_name, "_", options_mapping[match[1]]))
      }
      return(col)
    })
  }

  # Rename columns that start with the question name
  df <- df %>%
    dplyr::rename_with(
      transform_names,
      dplyr::starts_with(paste0(question_name, " "))
    )

  # Remove unnecessary columns
  columns_to_keep <- if (open_col_text != "" && other_col_text != "") {
    c(new_question_open_col)
  } else {
    character(0)
  }

  df <- df %>%
    dplyr::select(
      -dplyr::starts_with(paste0(question_name, " ")),
      dplyr::all_of(columns_to_keep)
    )

  return(df)
}


#' Process Single Choice Questions
#'
#' This function processes single-choice questions in a dataframe, merging multiple columns
#' that start with the question identifier into one column, preserving the first non-missing value.
#'
#' @param df Dataframe containing the survey data.
#' @param question_number The question number for which single-choice answers are being processed.
#'
#' @return A dataframe with the processed single-choice columns.
process_sc <- function(df, question_number) {
  # Create the question name based on the question number
  question_name <- paste0("Q", question_number)

  df <- df %>%
    dplyr::mutate(!!question_name :=
      dplyr::coalesce(!!!dplyr::select(
        .,
        dplyr::starts_with(question_name)
      ))) %>%
    dplyr::select(-dplyr::starts_with(question_name), question_name)

  return(df)
}


#' Process Slider and Open-ended Questions
#'
#' This function processes slider-style and  open text questions in a dataframe. 
#' It renames the column corresponding to the slider question based on the given question number.
#'
#' @param df Dataframe containing the survey data.
#' @param question_number The question number for the slider open text field.
#' @param question_text The exact text of the slider question as it appears in the dataframe.
#' 
#' @return A dataframe with the renamed slider and open-ended question column.
process_slider_ot <- function(df, question_number, question_text) {
  # Create the question name based on the question number
  question_name <- paste0("Q", question_number)
  old_question_col <- paste0(question_name, " - ", question_text)

  # Rename the column to a simplified question name
  df <- df %>%
    dplyr::rename(!!question_name := old_question_col)

  return(df)
}


# Function to clean Options vector
clean_options <- function(options) {
  # Remove text inside parentheses
  cleaned_options <- gsub("\\s?\\(.*\\)", "", options)
  
  # Replace the last value with "Sonstiges"
  cleaned_options[length(cleaned_options)] <- "Sonstiges"
  
  return(cleaned_options)
}


# ggplot(df, aes(x = Q9, y= Q9)) + 
#   geom_boxplot(fill = primary_color, color = "black") + 
#   labs(
#     title = "KI: Hype oder Mehrwert für das Ehrenamt?",
#     x = "Einschätzung",
#     y = NULL
#   ) +
#    theme(
#     axis.text.y = element_blank(), 
#     axis.ticks.y = element_blank()
#   ) +
#   annotate("text", x = 0, y = -0.5, label = "Hype", color = "black") +
#   annotate("text", x = 98, y = -0.5, label = "Mehrwert", color = "black") +
#   stat_summary(
#     fun = "mean", 
#     geom = "point", 
#     shape = 23, 
#     size = 4, 
#     color = "red", 
#     fill = "red"
#   )


find_strong_correlations <- function(data, column_selection, options_map = NULL, correlation_threshold = 0.5, p_value_threshold = 0.05) {
  selected_data <- data %>% select({{ column_selection }})

  strong_correlations <- list()

  for (i in 1:(ncol(selected_data) - 1)) {
    for (j in (i + 1):ncol(selected_data)) {
      crosstable <- table(selected_data[[i]], selected_data[[j]])

      correlation_test <- cor.test(as.numeric(selected_data[[i]]), as.numeric(selected_data[[j]]), method = "pearson")
      phi_correlation <- correlation_test$estimate
      p_value <- correlation_test$p.value

      # Check for strong correlations with p-value significance
      if (!is.na(phi_correlation) && abs(phi_correlation) > correlation_threshold && p_value < p_value_threshold) {
        question_1_label <- if (!is.null(options_map)) {
          options_map$label[options_map$question == names(selected_data)[i]]
        } else {
          names(selected_data)[i]
        }
        question_2_label <- if (!is.null(options_map)) {
          options_map$label[options_map$question == names(selected_data)[j]]
        } else {
          names(selected_data)[j]
        }

        strong_correlations[[paste0(names(selected_data)[i], " vs ", names(selected_data)[j])]] <- list(
          Crosstable = crosstable,
          Phi_Correlation = round(phi_correlation, 2),
          P_Value = round(p_value, 4),
          Question_Names = list(
            Q1 = question_1_label,
            Q2 = question_2_label
          )
        )
      }
    }
  }
  
  if (length(strong_correlations) > 0) {
    for (pair in names(strong_correlations)) {
      cat("\n", pair, "\n")
      cat(strong_correlations[[pair]]$Question_Names$Q1 , " vs. ", strong_correlations[[pair]]$Question_Names$Q2)
      print(strong_correlations[[pair]]$Crosstable)
      cat("Phi Correlation:", strong_correlations[[pair]]$Phi_Correlation, "\n")
      cat("P-Value:", strong_correlations[[pair]]$P_Value, "\n")
    }
  } else {
    cat("No strong correlations found.\n")
  }
}