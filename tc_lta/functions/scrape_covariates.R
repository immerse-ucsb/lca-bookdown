# scrape_covariates.R
library(MplusAutomation)
library(dplyr)

# Read Mplus output
mplus_output <- readModels("3step/c4_step3.out")
if (is.null(mplus_output$output)) {
  stop("Error: Mplus output could not be read.")
}

# Extract log-odds
logodds_list <- list()
for (i in 1:3) {
  alt_params <- mplus_output$parameters$unstandardized.alt[[i]]
  if (!is.null(alt_params)) {
    logodds_list[[i]] <- alt_params[1:12, ]
  }
}
logodds_params <- do.call(rbind, logodds_list)

# Parse odds ratios
output_text <- mplus_output$output
odds_header <- grep("^\\s*ODDS\\s+RATIO\\s+FOR\\s+THE\\s+ALTERNATIVE\\s+PARAMETERIZATIONS", output_text, ignore.case = TRUE)[1]
ci_start <- grep("^\\s*CONFIDENCE\\s+INTERVALS", output_text, ignore.case = TRUE)[1]
param_starts <- grep("^\\s*Parameterization using Reference Class", output_text)
odds_sections <- param_starts[param_starts > odds_header & param_starts < ci_start]
standardized_start <- grep("^\\s*STANDARDIZED MODEL RESULTS", output_text, ignore.case = TRUE)[1]

odds_data <- list()
for (i in 1:3) {
  start_idx <- odds_sections[i] + 2
  end_idx <- if (i < 3) odds_sections[i + 1] - 1 else standardized_start - 1
  section_lines <- output_text[start_idx:end_idx]
  section_lines <- section_lines[nchar(trimws(section_lines)) > 0]
  
  section_data <- data.frame(
    paramHeader = character(),
    param = character(),
    est = numeric(),
    se = numeric(),
    lower_2.5ci = numeric(),
    upper_2.5ci = numeric(),
    stringsAsFactors = FALSE
  )
  current_paramHeader <- ""
  for (line in section_lines) {
    parts <- unlist(strsplit(trimws(line), "\\s+"))
    if (length(parts) >= 2 && grepl("ON$", parts[1])) {
      current_paramHeader <- parts[1]
      next
    }
    if (length(parts) >= 5 && parts[1] %in% c("WHITE", "FIRSTGEN", "LATINX", "ASIAN")) {
      numeric_check <- all(sapply(parts[2:5], function(x) suppressWarnings(!is.na(as.numeric(x)))))
      if (numeric_check) {
        section_data <- rbind(section_data, data.frame(
          paramHeader = current_paramHeader,
          param = parts[1],
          est = as.numeric(parts[2]),
          se = as.numeric(parts[3]),
          lower_2.5ci = as.numeric(parts[4]),
          upper_2.5ci = as.numeric(parts[5]),
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  odds_data[[i]] <- section_data
}

odds_params_partial <- do.call(rbind, odds_data)

odds_params <- logodds_params
odds_params$est <- exp(logodds_params$est)
odds_params$se <- NA
odds_params$lower_2.5ci <- NA
odds_params$upper_2.5ci <- NA

for (i in 1:3) {
  start_row <- (i - 1) * 12 + 1
  end_row <- i * 12
  if (nrow(odds_data[[i]]) == 12) {
    odds_params[start_row:end_row, c("est", "se", "lower_2.5ci", "upper_2.5ci")] <- 
      odds_data[[i]][, c("est", "se", "lower_2.5ci", "upper_2.5ci")]
  } else {
    warning(paste("odds_data[[", i, "]] has", nrow(odds_data[[i]]), "rows, expected 12"))
  }
}

scraped_data <- logodds_params
scraped_data$odds_est <- odds_params$est
scraped_data$odds_se <- odds_params$se
scraped_data$odds_lower_2.5ci <- odds_params$lower_2.5ci
scraped_data$odds_upper_2.5ci <- odds_params$upper_2.5ci