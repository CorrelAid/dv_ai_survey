source("renv/activate.R")

setHook("rstudio.sessionInit", function(newSession) {
  if (requireNamespace("styler", quietly = TRUE)) {
    indent <- rstudioapi::readRStudioPreference("num_spaces_for_tab", 2L)
    
    options(
      styler.addins_style_transformer = sprintf(
        "styler::tidyverse_style(indent_by = %s)", indent
      )
    )
  }
}, action = "append")
