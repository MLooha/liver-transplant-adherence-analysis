################################################################################
# Liver transplant adherence study
# Reproducible analysis script
#
# Analyses:
#   - questionnaire score reconstruction and quality checks
#   - descriptive and psychometric analyses
#   - unadjusted and adjusted association analyses
#   - sensitivity analyses
#   - exploratory grouped nested cross-validation
#
# The participant-level dataset is not included in the public repository.
################################################################################

###### 00. CONFIGURATION #######################################################

options(
  stringsAsFactors = FALSE,
  scipen = 999,
  warn = 1,
  dplyr.summarise.inform = FALSE
)

SEED <- 20260727L
set.seed(SEED)

# Paths can be supplied with environment variables. The defaults assume that
# the script is run from the repository root.
DATA_FILE <- Sys.getenv(
  "ANALYSIS_DATA_FILE",
  unset = file.path("data", "Nazari_Liver_Transplant_Clean.sav")
)
OUTPUT_ROOT <- Sys.getenv(
  "ANALYSIS_OUTPUT_DIR",
  unset = "results"
)

# The supplied analytic file must contain item responses already keyed in the
# scoring direction used by the study. Set this to FALSE only after listing the
# reverse-keyed items below.
ITEMS_ALREADY_KEYED <- TRUE
ATQ_REVERSE_ITEMS <- character()
NEO_REVERSE_ITEMS <- character()
ITEM_MINIMUM <- 0
ITEM_MAXIMUM <- 4

QUICK_TEST <- FALSE
BOOTSTRAP_REPETITIONS <- if (QUICK_TEST) 200L else 5000L
ML_REGRESSION_ENABLED <- TRUE
ML_CLASSIFICATION_ENABLED <- TRUE
SHAP_ENABLED <- TRUE

OUTER_FOLDS <- 5L
OUTER_REPEATS <- if (QUICK_TEST) 2L else 10L
INNER_FOLDS <- 5L
INNER_REPEATS <- 1L
TUNING_GRID_SIZE <- if (QUICK_TEST) 4L else 20L
FINAL_TUNING_REPEATS <- if (QUICK_TEST) 1L else 5L
SHAP_SIMULATIONS <- if (QUICK_TEST) 20L else 250L
CLASSIFICATION_THRESHOLD <- 0.50

# Use one worker by default for deterministic repository runs. A larger value
# can be supplied through ANALYSIS_WORKERS.
PARALLEL_WORKERS <- suppressWarnings(
  as.integer(Sys.getenv("ANALYSIS_WORKERS", unset = "1"))
)
if (!is.finite(PARALLEL_WORKERS) || PARALLEL_WORKERS < 1L) {
  PARALLEL_WORKERS <- 1L
}

# Row-level outputs and fitted workflow objects may contain participant-level
# information. They are disabled for public-repository runs.
EXPORT_ROW_LEVEL_OUTPUTS <- FALSE
SAVE_FITTED_MODELS <- FALSE

FIGURE_DPI_PNG <- 400
FIGURE_DPI_TIFF <- 600
BASE_FONT_FAMILY <- "sans"

###### 01. PACKAGES ############################################################

required_packages <- c(
  "haven", "dplyr", "tidyr", "purrr", "stringr", "forcats", "tibble",
  "ggplot2", "patchwork", "ragg", "openxlsx", "digest", "fs", "psych",
  "broom", "sandwich", "lmtest", "performance", "betareg", "boot", "MASS",
  "tidymodels", "doParallel", "foreach", "glmnet", "ranger", "xgboost",
  "kernlab", "fastshap"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0L) {
  stop(
    "Missing required packages: ",
    paste(missing_packages, collapse = ", "),
    ". Install the recorded package versions before running the analysis."
  )
}

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(forcats)
  library(tibble)
  library(ggplot2)
  library(tidymodels)
})

tidymodels::tidymodels_prefer(quiet = TRUE)

###### 02. OUTPUT DIRECTORIES AND LOGGING ######################################

section_names <- c(
  "00_Run_Documentation",
  "01_Data_Quality",
  "02_Descriptive_Analysis",
  "03_Psychometrics",
  "04_Primary_Associations",
  "05_Adjusted_Models",
  "06_Sensitivity_Analyses",
  "07_ML_Regression",
  "08_ML_Classification"
)

section_paths <- stats::setNames(
  file.path(OUTPUT_ROOT, section_names),
  section_names
)

fs::dir_create(OUTPUT_ROOT, recurse = TRUE)
purrr::walk(
  section_paths,
  function(path) {
    fs::dir_create(path, recurse = TRUE)
    fs::dir_create(file.path(path, "Tables"), recurse = TRUE)
    fs::dir_create(file.path(path, "Figures"), recurse = TRUE)
    fs::dir_create(file.path(path, "Objects"), recurse = TRUE)
  }
)

LOG_FILE <- file.path(
  section_paths[["00_Run_Documentation"]],
  "analysis_run_log.txt"
)

log_message <- function(...) {
  message_text <- paste0(...)
  line <- paste0(
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    " | ",
    message_text,
    "\n"
  )
  cat(line, file = LOG_FILE, append = TRUE)
  message(message_text)
  invisible(message_text)
}

cat(
  paste0(
    "Liver transplant adherence analysis\n",
    "Run started: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), "\n",
    "Seed: ", SEED, "\n",
    "Input file: ", basename(DATA_FILE), "\n",
    "Output directory: ", OUTPUT_ROOT, "\n\n"
  ),
  file = LOG_FILE
)

###### 03. REUSABLE STYLING AND EXPORT FUNCTIONS ###############################

palette_journal <- c(
  navy = "#17365D",
  blue = "#2F75B5",
  sky = "#56B4E9",
  teal = "#008B8B",
  green = "#009E73",
  gold = "#E69F00",
  vermillion = "#D55E00",
  purple = "#7B2CBF",
  grey = "#6B7280",
  pale_blue = "#D9EAF7",
  pale_red = "#FCE4D6"
)

theme_journal <- function(base_size = 11) {
  ggplot2::theme_minimal(
    base_size = base_size,
    base_family = BASE_FONT_FAMILY
  ) +
    ggplot2::theme(
      plot.title = element_text(
        face = "bold",
        size = base_size + 3,
        color = palette_journal[["navy"]],
        hjust = 0
      ),
      plot.subtitle = element_text(
        size = base_size,
        color = palette_journal[["grey"]],
        margin = margin(b = 10)
      ),
      plot.caption = element_text(
        size = base_size - 2,
        color = palette_journal[["grey"]],
        hjust = 0
      ),
      axis.title = element_text(face = "bold", color = "#1F2937"),
      axis.text = element_text(color = "#1F2937"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#E5E7EB", linewidth = 0.35),
      legend.title = element_text(face = "bold"),
      legend.position = "bottom",
      strip.text = element_text(face = "bold", color = palette_journal[["navy"]]),
      plot.margin = margin(10, 14, 10, 10)
    )
}

save_journal_figure <- function(
    plot_object,
    directory,
    file_stem,
    width = 8,
    height = 6
) {
  fs::dir_create(directory, recurse = TRUE)

  png_path <- file.path(directory, paste0(file_stem, ".png"))
  tiff_path <- file.path(directory, paste0(file_stem, ".tiff"))

  tryCatch(
    {
      ggplot2::ggsave(
        filename = png_path,
        plot = plot_object,
        device = ragg::agg_png,
        width = width,
        height = height,
        units = "in",
        dpi = FIGURE_DPI_PNG,
        background = "white"
      )
    },
    error = function(error_condition) {
      ggplot2::ggsave(
        filename = png_path,
        plot = plot_object,
        device = "png",
        width = width,
        height = height,
        units = "in",
        dpi = FIGURE_DPI_PNG,
        bg = "white"
      )
    }
  )

  ggplot2::ggsave(
    filename = tiff_path,
    plot = plot_object,
    device = "tiff",
    width = width,
    height = height,
    units = "in",
    dpi = FIGURE_DPI_TIFF,
    compression = "lzw",
    bg = "white"
  )

  invisible(c(png = png_path, tiff = tiff_path))
}

excel_safe_frame <- function(data_frame) {
  output <- as.data.frame(data_frame, stringsAsFactors = FALSE)
  output[] <- lapply(
    output,
    function(column) {
      if (is.factor(column)) {
        column <- as.character(column)
      }
      if (is.numeric(column)) {
        column[!is.finite(column)] <- NA_real_
      }
      column
    }
  )
  output
}

safe_sheet_name <- function(name, existing_names = character()) {
  candidate <- stringr::str_replace_all(name, "[:\\\\/?*\\[\\]]", "_")
  candidate <- stringr::str_sub(candidate, 1L, 31L)
  if (!candidate %in% existing_names) {
    return(candidate)
  }
  index <- 2L
  repeat {
    suffix <- paste0("_", index)
    trial <- paste0(
      stringr::str_sub(candidate, 1L, 31L - nchar(suffix)),
      suffix
    )
    if (!trial %in% existing_names) {
      return(trial)
    }
    index <- index + 1L
  }
}

calculate_excel_widths <- function(data_frame) {
  if (ncol(data_frame) == 0L) {
    return(12)
  }
  purrr::map_dbl(
    seq_along(data_frame),
    function(column_index) {
      values <- c(
        names(data_frame)[column_index],
        as.character(data_frame[[column_index]])
      )
      values[is.na(values)] <- ""
      longest <- max(nchar(values), na.rm = TRUE)
      min(45, max(10, longest + 2))
    }
  )
}

write_excel_report <- function(
    file_path,
    sheets,
    report_title,
    report_note = NULL
) {
  workbook <- openxlsx::createWorkbook(
    creator = "Liver Transplant Adherence Study"
  )

  title_style <- openxlsx::createStyle(
    fontName = BASE_FONT_FAMILY,
    fontSize = 16,
    fontColour = "#FFFFFF",
    fgFill = palette_journal[["navy"]],
    textDecoration = "bold",
    halign = "left",
    valign = "center"
  )
  note_style <- openxlsx::createStyle(
    fontName = BASE_FONT_FAMILY,
    fontSize = 10,
    fontColour = "#4B5563",
    fgFill = "#F3F4F6",
    wrapText = TRUE,
    valign = "top"
  )
  header_style <- openxlsx::createStyle(
    fontName = BASE_FONT_FAMILY,
    fontSize = 10,
    fontColour = "#FFFFFF",
    fgFill = palette_journal[["blue"]],
    textDecoration = "bold",
    halign = "center",
    valign = "center",
    wrapText = TRUE,
    border = "Bottom",
    borderColour = "#17365D"
  )
  body_style <- openxlsx::createStyle(
    fontName = BASE_FONT_FAMILY,
    fontSize = 10,
    valign = "top",
    wrapText = TRUE
  )

  created_names <- character()

  purrr::iwalk(
    sheets,
    function(sheet_data, requested_name) {
      sheet_name <- safe_sheet_name(requested_name, created_names)
      created_names <<- c(created_names, sheet_name)
      openxlsx::addWorksheet(
        workbook,
        sheetName = sheet_name,
        gridLines = FALSE,
        tabColour = palette_journal[["blue"]]
      )

      data_to_write <- excel_safe_frame(sheet_data)
      table_columns <- max(1L, ncol(data_to_write))
      title_end_column <- min(max(table_columns, 4L), 12L)

      openxlsx::mergeCells(
        workbook,
        sheet = sheet_name,
        cols = 1:title_end_column,
        rows = 1
      )
      openxlsx::writeData(
        workbook,
        sheet = sheet_name,
        x = report_title,
        startRow = 1,
        startCol = 1
      )
      openxlsx::addStyle(
        workbook,
        sheet = sheet_name,
        style = title_style,
        rows = 1,
        cols = 1:title_end_column,
        gridExpand = TRUE
      )
      openxlsx::setRowHeights(workbook, sheet_name, rows = 1, heights = 28)

      start_row <- 3L
      if (!is.null(report_note) && nzchar(report_note)) {
        openxlsx::mergeCells(
          workbook,
          sheet = sheet_name,
          cols = 1:title_end_column,
          rows = 3
        )
        openxlsx::writeData(
          workbook,
          sheet = sheet_name,
          x = report_note,
          startRow = 3,
          startCol = 1
        )
        openxlsx::addStyle(
          workbook,
          sheet = sheet_name,
          style = note_style,
          rows = 3,
          cols = 1:title_end_column,
          gridExpand = TRUE
        )
        openxlsx::setRowHeights(workbook, sheet_name, rows = 3, heights = 42)
        start_row <- 5L
      }

      if (ncol(data_to_write) == 0L) {
        data_to_write <- data.frame(Message = "No output was generated.")
      }

      if (nrow(data_to_write) == 0L) {
        openxlsx::writeData(
          workbook,
          sheet = sheet_name,
          x = data_to_write,
          startRow = start_row,
          startCol = 1,
          headerStyle = header_style
        )
        openxlsx::writeData(
          workbook,
          sheet = sheet_name,
          x = "No rows met the reporting criteria.",
          startRow = start_row + 1L,
          startCol = 1
        )
      } else {
        openxlsx::writeDataTable(
          workbook,
          sheet = sheet_name,
          x = data_to_write,
          startRow = start_row,
          startCol = 1,
          tableStyle = "TableStyleMedium2",
          withFilter = TRUE
        )
        openxlsx::addStyle(
          workbook,
          sheet = sheet_name,
          style = header_style,
          rows = start_row,
          cols = seq_len(ncol(data_to_write)),
          gridExpand = TRUE,
          stack = TRUE
        )
        openxlsx::addStyle(
          workbook,
          sheet = sheet_name,
          style = body_style,
          rows = (start_row + 1L):(start_row + nrow(data_to_write)),
          cols = seq_len(ncol(data_to_write)),
          gridExpand = TRUE,
          stack = TRUE
        )
      }

      openxlsx::freezePane(
        workbook,
        sheet = sheet_name,
        firstActiveRow = start_row + 1L,
        firstActiveCol = 1L
      )
      openxlsx::setColWidths(
        workbook,
        sheet = sheet_name,
        cols = seq_len(ncol(data_to_write)),
        widths = calculate_excel_widths(data_to_write)
      )
      openxlsx::pageSetup(
        workbook,
        sheet = sheet_name,
        orientation = "landscape",
        fitToWidth = 1,
        fitToHeight = 0
      )
    }
  )

  openxlsx::saveWorkbook(workbook, file_path, overwrite = TRUE)
  invisible(file_path)
}

format_p_value <- function(p_value) {
  dplyr::case_when(
    is.na(p_value) ~ NA_character_,
    p_value < 0.001 ~ "<0.001",
    TRUE ~ sprintf("%.3f", p_value)
  )
}

mean_ci <- function(values, confidence = 0.95) {
  values <- values[is.finite(values)]
  n_value <- length(values)
  if (n_value < 2L) {
    return(c(mean = mean(values), lower = NA_real_, upper = NA_real_))
  }
  mean_value <- mean(values)
  standard_error <- stats::sd(values) / sqrt(n_value)
  critical_value <- stats::qt(
    1 - (1 - confidence) / 2,
    df = n_value - 1
  )
  c(
    mean = mean_value,
    lower = mean_value - critical_value * standard_error,
    upper = mean_value + critical_value * standard_error
  )
}

wilson_interval <- function(successes, total, confidence = 0.95) {
  if (total == 0L) {
    return(c(lower = NA_real_, upper = NA_real_))
  }
  z_value <- stats::qnorm(1 - (1 - confidence) / 2)
  proportion <- successes / total
  denominator <- 1 + z_value^2 / total
  center <- (
    proportion + z_value^2 / (2 * total)
  ) / denominator
  half_width <- (
    z_value *
      sqrt(
        proportion * (1 - proportion) / total +
          z_value^2 / (4 * total^2)
      )
  ) / denominator
  c(
    lower = max(0, center - half_width),
    upper = min(1, center + half_width)
  )
}

###### 04. VARIABLE DEFINITIONS ################################################

atq_items <- sprintf("ATQ%02d", 1:40)
neo_items <- sprintf("NEO%02d", 1:60)

atq_domains <- list(
  Persistence = sprintf("ATQ%02d", 1:9),
  Participation = sprintf("ATQ%02d", 10:16),
  Adaptability = sprintf("ATQ%02d", 17:23),
  Integration = sprintf("ATQ%02d", 24:28),
  Sticking = sprintf("ATQ%02d", 29:32),
  Commitment = sprintf("ATQ%02d", 33:37),
  Execution_Certainty = sprintf("ATQ%02d", 38:40)
)

neo_domains <- list(
  Neuroticism = sprintf(
    "NEO%02d",
    c(1, 6, 11, 16, 21, 26, 31, 36, 41, 46, 51, 56)
  ),
  Extraversion = sprintf(
    "NEO%02d",
    c(2, 7, 12, 17, 22, 27, 32, 37, 42, 47, 52, 57)
  ),
  Openness = sprintf(
    "NEO%02d",
    c(3, 8, 13, 18, 23, 28, 33, 38, 43, 48, 53, 58)
  ),
  Agreeableness = sprintf(
    "NEO%02d",
    c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59)
  ),
  Conscientiousness = sprintf(
    "NEO%02d",
    c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60)
  )
)

atq_score_variables <- c(
  "ATQ_Total_Pct",
  "ATQ_Persistence_Pct",
  "ATQ_Participation_Pct",
  "ATQ_Adaptability_Pct",
  "ATQ_Integration_Pct",
  "ATQ_Sticking_Pct",
  "ATQ_Commitment_Pct",
  "ATQ_Execution_Certainty_Pct"
)

atq_raw_variables <- c(
  "ATQ_Total_Raw",
  paste0("ATQ_", names(atq_domains), "_Raw")
)

neo_score_variables <- c(
  "NEO_Neuroticism",
  "NEO_Extraversion",
  "NEO_Openness",
  "NEO_Agreeableness",
  "NEO_Conscientiousness"
)

categorical_variables <- c(
  "Age_Group",
  "Sex",
  "Education_Level",
  "Time_Since_Transplant",
  "Marital_Status",
  "Substance_Use",
  "Alcohol_Use",
  "Medication_Count_Category",
  "ATQ_Level",
  "Potential_Duplicate_Flag",
  "Duplicate_Cluster",
  "QC_Flag"
)

required_variables <- unique(c(
  "Study_ID",
  "Source_Row",
  categorical_variables,
  atq_items,
  atq_raw_variables,
  atq_score_variables,
  neo_items,
  neo_score_variables,
  "NEO_Missing_Items"
))

personality_labels <- c(
  NEO_Neuroticism = "Neuroticism",
  NEO_Extraversion = "Extraversion",
  NEO_Openness = "Openness",
  NEO_Agreeableness = "Agreeableness",
  NEO_Conscientiousness = "Conscientiousness"
)

adherence_labels <- c(
  ATQ_Total_Pct = "Total adherence",
  ATQ_Persistence_Pct = "Persistence",
  ATQ_Participation_Pct = "Participation",
  ATQ_Adaptability_Pct = "Adaptability",
  ATQ_Integration_Pct = "Integration",
  ATQ_Sticking_Pct = "Sticking to treatment",
  ATQ_Commitment_Pct = "Commitment",
  ATQ_Execution_Certainty_Pct = "Execution certainty"
)

###### 05. IMPORT, SCORE RECONSTRUCTION, AND RECODING ##########################

if (!file.exists(DATA_FILE)) {
  stop(
    "The SPSS data file was not found: ",
    DATA_FILE,
    ". Set ANALYSIS_DATA_FILE or place the file in the data directory."
  )
}

log_message("Reading the SPSS dataset.")
data_raw <- haven::read_sav(DATA_FILE, user_na = FALSE)

missing_required_variables <- setdiff(required_variables, names(data_raw))
if (length(missing_required_variables) > 0L) {
  stop(
    "Required variables are absent from the SPSS file: ",
    paste(missing_required_variables, collapse = ", ")
  )
}

data_numeric <- data_raw |>
  dplyr::mutate(
    dplyr::across(
      dplyr::all_of(required_variables),
      function(value) suppressWarnings(as.numeric(value))
    )
  )

imported_score_data <- data_numeric |>
  dplyr::select(
    dplyr::all_of(
      unique(c(atq_raw_variables, atq_score_variables, neo_score_variables))
    )
  )

reverse_score_items <- function(data_frame, item_names) {
  if (length(item_names) == 0L) {
    return(data_frame)
  }
  unknown_items <- setdiff(item_names, names(data_frame))
  if (length(unknown_items) > 0L) {
    stop("Unknown reverse-keyed items: ", paste(unknown_items, collapse = ", "))
  }
  data_frame |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(item_names),
        function(value) ITEM_MINIMUM + ITEM_MAXIMUM - value
      )
    )
}

if (!ITEMS_ALREADY_KEYED) {
  if (length(ATQ_REVERSE_ITEMS) + length(NEO_REVERSE_ITEMS) == 0L) {
    stop(
      "ITEMS_ALREADY_KEYED is FALSE, but no reverse-keyed items were supplied."
    )
  }
  data_numeric <- reverse_score_items(data_numeric, ATQ_REVERSE_ITEMS)
  data_numeric <- reverse_score_items(data_numeric, NEO_REVERSE_ITEMS)
}

reconstruct_questionnaire_scores <- function(data_frame) {
  output <- data_frame

  output$ATQ_Total_Raw <- rowSums(output[atq_items], na.rm = FALSE)
  output$ATQ_Total_Pct <- output$ATQ_Total_Raw / (length(atq_items) * 4) * 100

  for (domain_name in names(atq_domains)) {
    domain_items <- atq_domains[[domain_name]]
    raw_name <- paste0("ATQ_", domain_name, "_Raw")
    percent_name <- paste0("ATQ_", domain_name, "_Pct")
    output[[raw_name]] <- rowSums(output[domain_items], na.rm = FALSE)
    output[[percent_name]] <- (
      output[[raw_name]] / (length(domain_items) * 4) * 100
    )
  }

  for (domain_name in names(neo_domains)) {
    score_name <- paste0("NEO_", domain_name)
    output[[score_name]] <- rowSums(
      output[neo_domains[[domain_name]]],
      na.rm = FALSE
    )
  }

  output
}

data_scored <- reconstruct_questionnaire_scores(data_numeric)

data_analysis <- data_scored |>
  dplyr::mutate(
    Record_ID = dplyr::row_number(),
    Potential_Duplicate_Flag = dplyr::coalesce(Potential_Duplicate_Flag, 0),
    Duplicate_Cluster = dplyr::coalesce(Duplicate_Cluster, 0),
    Age_Group_Factor = factor(
      Age_Group,
      levels = 1:4,
      labels = c("15-30", "31-45", "46-60", "61-80"),
      ordered = TRUE
    ),
    Sex_Factor = factor(
      Sex,
      levels = c(1, 2),
      labels = c("Male", "Female")
    ),
    Education_Factor = factor(
      Education_Level,
      levels = 1:3,
      labels = c(
        "Below high school diploma",
        "High school to bachelor's degree",
        "Master's degree or higher"
      ),
      ordered = TRUE
    ),
    Time_Since_Transplant_Factor = factor(
      Time_Since_Transplant,
      levels = 1:3,
      labels = c("1-2 years", "2-3 years", ">3 years"),
      ordered = TRUE
    ),
    Marital_Status_Factor = factor(
      Marital_Status,
      levels = 1:3,
      labels = c("Married", "Single", "Divorced or widowed")
    ),
    Substance_Use_Factor = factor(
      Substance_Use,
      levels = c(0, 1),
      labels = c("No", "Yes")
    ),
    Alcohol_Use_Factor = factor(
      Alcohol_Use,
      levels = c(0, 1),
      labels = c("No", "Yes")
    ),
    Medication_Count_Factor = factor(
      Medication_Count_Category,
      levels = 1:3,
      labels = c("1-5", "5-10", ">10"),
      ordered = TRUE
    ),
    ATQ_Level_Factor = factor(
      ATQ_Level,
      levels = 1:4,
      labels = c("Poor", "Moderate", "Good", "Very good"),
      ordered = TRUE
    ),
    Married_Binary = factor(
      ifelse(Marital_Status == 1, "Married", "Not married"),
      levels = c("Married", "Not married")
    ),
    ATQ_High = factor(
      ifelse(ATQ_Total_Pct >= 75, "Very_Good", "Below_Very_Good"),
      levels = c("Very_Good", "Below_Very_Good")
    ),
    CV_Group = ifelse(
      Duplicate_Cluster > 0,
      paste0("PotentialDuplicateCluster_", Duplicate_Cluster),
      paste0("IndependentRecord_", Record_ID)
    )
  )

if (anyNA(data_analysis$Study_ID)) {
  stop("Study_ID contains missing values.")
}
if (anyDuplicated(data_analysis$Study_ID) > 0L) {
  stop("Study_ID is not unique.")
}
if (anyNA(data_analysis$CV_Group)) {
  stop("CV_Group contains missing values.")
}
if (any(
  data_analysis$Potential_Duplicate_Flag == 1 &
    data_analysis$Duplicate_Cluster <= 0,
  na.rm = TRUE
)) {
  stop("A duplicate-flagged record has no positive Duplicate_Cluster value.")
}

if (nrow(data_analysis) != 130L) {
  warning(
    "The reference analytic file contains 130 records; the current file contains ",
    nrow(data_analysis),
    "."
  )
}

dataset_sha256 <- digest::digest(
  object = DATA_FILE,
  algo = "sha256",
  file = TRUE,
  serialize = FALSE
)

log_message(
  "Data imported: ",
  nrow(data_analysis),
  " records and ",
  ncol(data_analysis),
  " columns after score reconstruction and recoding."
)

###### 06. RUN CONFIGURATION RECORD ############################################

run_configuration <- tibble::tibble(
  Setting = c(
    "Run timestamp",
    "R version",
    "Input file",
    "Input SHA-256",
    "Output directory",
    "Random seed",
    "Items already keyed",
    "Bootstrap repetitions",
    "Outer cross-validation",
    "Inner cross-validation",
    "Tuning grid size",
    "Classification threshold",
    "Row-level outputs exported",
    "Fitted models saved",
    "Quick test mode"
  ),
  Value = c(
    format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    R.version.string,
    basename(DATA_FILE),
    dataset_sha256,
    OUTPUT_ROOT,
    as.character(SEED),
    as.character(ITEMS_ALREADY_KEYED),
    as.character(BOOTSTRAP_REPETITIONS),
    paste0(OUTER_FOLDS, "-fold x ", OUTER_REPEATS, " repeats"),
    paste0(INNER_FOLDS, "-fold x ", INNER_REPEATS, " repeat(s)"),
    as.character(TUNING_GRID_SIZE),
    as.character(CLASSIFICATION_THRESHOLD),
    as.character(EXPORT_ROW_LEVEL_OUTPUTS),
    as.character(SAVE_FITTED_MODELS),
    as.character(QUICK_TEST)
  )
)

write_excel_report(
  file_path = file.path(
    section_paths[["00_Run_Documentation"]],
    "Tables",
    "00_Run_Configuration.xlsx"
  ),
  sheets = list(Run_Configuration = run_configuration),
  report_title = "Analysis Run Configuration",
  report_note = paste(
    "The public repository should contain code and documentation only.",
    "Participant-level data, row-level predictions, and fitted model objects",
    "must not be committed."
  )
)

###### 07. DATA-QUALITY AND SCORE-INTEGRITY AUDIT ##############################

missingness_table <- tibble::tibble(
  Variable = names(data_analysis),
  Missing_N = purrr::map_int(
    data_analysis,
    function(column) sum(is.na(column))
  )
) |>
  dplyr::mutate(
    Missing_Percent = 100 * Missing_N / nrow(data_analysis)
  ) |>
  dplyr::filter(Missing_N > 0L) |>
  dplyr::arrange(dplyr::desc(Missing_Percent), Variable)

if (nrow(missingness_table) == 0L) {
  missingness_table <- tibble::tibble(
    Variable = "None",
    Missing_N = 0L,
    Missing_Percent = 0
  )
}

atq_item_matrix <- as.matrix(data_analysis[atq_items])
neo_item_matrix <- as.matrix(data_analysis[neo_items])

range_checks <- dplyr::bind_rows(
  tibble::tibble(
    Check = "ATQ item responses outside 0-4",
    Violations = sum(
      !is.na(atq_item_matrix) &
        (atq_item_matrix < ITEM_MINIMUM | atq_item_matrix > ITEM_MAXIMUM)
    )
  ),
  tibble::tibble(
    Check = "NEO-FFI item responses outside 0-4",
    Violations = sum(
      !is.na(neo_item_matrix) &
        (neo_item_matrix < ITEM_MINIMUM | neo_item_matrix > ITEM_MAXIMUM)
    )
  ),
  tibble::tibble(
    Check = "ATQ_Total_Pct outside 0-100",
    Violations = sum(
      data_analysis$ATQ_Total_Pct < 0 |
        data_analysis$ATQ_Total_Pct > 100,
      na.rm = TRUE
    )
  ),
  tibble::tibble(
    Check = "Nonunique Study_ID",
    Violations = sum(duplicated(data_analysis$Study_ID))
  ),
  tibble::tibble(
    Check = "Rows flagged as potential duplicates",
    Violations = sum(
      data_analysis$Potential_Duplicate_Flag == 1,
      na.rm = TRUE
    )
  ),
  tibble::tibble(
    Check = "Rows with incomplete NEO-FFI responses",
    Violations = sum(data_analysis$NEO_Missing_Items > 0, na.rm = TRUE)
  )
)

score_validation <- purrr::map_dfr(
  names(imported_score_data),
  function(score_name) {
    difference <- data_analysis[[score_name]] - imported_score_data[[score_name]]
    tibble::tibble(
      Score = score_name,
      Maximum_Absolute_Difference = ifelse(
        all(is.na(difference)),
        NA_real_,
        max(abs(difference), na.rm = TRUE)
      ),
      Records_With_Difference = sum(abs(difference) > 1e-8, na.rm = TRUE),
      Missingness_Matches = all(
        is.na(data_analysis[[score_name]]) ==
          is.na(imported_score_data[[score_name]])
      )
    )
  }
)

qc_flag_table <- data_analysis |>
  dplyr::count(QC_Flag, name = "N") |>
  dplyr::mutate(
    QC_Status = dplyr::recode(
      as.character(QC_Flag),
      `0` = "No identified issue",
      `1` = "Potential duplicate identifier match",
      `2` = "Incomplete NEO-FFI response"
    ),
    Percent = 100 * N / sum(N)
  ) |>
  dplyr::select(QC_Flag, QC_Status, N, Percent)

quality_sheets <- list(
  Range_and_Integrity = range_checks,
  Missingness = missingness_table,
  Score_Reconstruction = score_validation,
  QC_Flags = qc_flag_table
)

if (EXPORT_ROW_LEVEL_OUTPUTS) {
  quality_sheets$Potential_Duplicates <- data_analysis |>
    dplyr::filter(Potential_Duplicate_Flag == 1) |>
    dplyr::select(
      Record_ID,
      Duplicate_Cluster,
      QC_Flag,
      ATQ_Total_Pct,
      dplyr::all_of(neo_score_variables)
    ) |>
    dplyr::arrange(Duplicate_Cluster, Record_ID)
}

write_excel_report(
  file_path = file.path(
    section_paths[["01_Data_Quality"]],
    "Tables",
    "01_Data_Quality_Audit.xlsx"
  ),
  sheets = quality_sheets,
  report_title = "Data-Quality and Score-Integrity Audit",
  report_note = paste(
    "Questionnaire scores are reconstructed from item responses.",
    "Imported score variables are used only for agreement checks."
  )
)

qc_plot <- qc_flag_table |>
  dplyr::mutate(
    QC_Status = forcats::fct_reorder(QC_Status, N)
  ) |>
  ggplot2::ggplot(
    ggplot2::aes(x = QC_Status, y = N, fill = QC_Status)
  ) +
  ggplot2::geom_col(width = 0.68, show.legend = FALSE) +
  ggplot2::geom_text(
    ggplot2::aes(label = paste0(N, " (", sprintf("%.1f", Percent), "%)")),
    hjust = -0.08,
    size = 3.6,
    family = BASE_FONT_FAMILY
  ) +
  ggplot2::coord_flip(clip = "off") +
  ggplot2::scale_fill_manual(
    values = c(
      "No identified issue" = palette_journal[["teal"]],
      "Potential duplicate identifier match" = palette_journal[["gold"]],
      "Incomplete NEO-FFI response" = palette_journal[["vermillion"]]
    )
  ) +
  ggplot2::scale_y_continuous(
    expand = ggplot2::expansion(mult = c(0, 0.18))
  ) +
  ggplot2::labs(
    title = "Data-quality status of the analytic sample",
    subtitle = paste0("N = ", nrow(data_analysis)),
    x = NULL,
    y = "Number of records"
  ) +
  theme_journal()

save_journal_figure(
  qc_plot,
  file.path(section_paths[["01_Data_Quality"]], "Figures"),
  "Figure_QC_Status",
  width = 8.5,
  height = 4.8
)

###### 08. DESCRIPTIVE ANALYSIS ################################################

categorical_factor_map <- c(
  Age_Group_Factor = "Age group, years",
  Sex_Factor = "Sex",
  Education_Factor = "Education",
  Time_Since_Transplant_Factor = "Time since transplantation",
  Marital_Status_Factor = "Marital status",
  Substance_Use_Factor = "History of substance use",
  Alcohol_Use_Factor = "History of alcohol use",
  Medication_Count_Factor = "Number of medications",
  ATQ_Level_Factor = "Adherence category"
)

categorical_descriptives <- purrr::imap_dfr(
  categorical_factor_map,
  function(display_label, variable_name) {
    variable_values <- data_analysis[[variable_name]]
    valid_n <- sum(!is.na(variable_values))
    counts <- table(variable_values, useNA = "no")
    purrr::map_dfr(
      names(counts),
      function(level_name) {
        count_value <- as.integer(counts[[level_name]])
        interval <- wilson_interval(count_value, valid_n)
        tibble::tibble(
          Characteristic = display_label,
          Level = level_name,
          N = count_value,
          Percent = 100 * count_value / valid_n,
          CI_95_Lower_Percent = 100 * interval[["lower"]],
          CI_95_Upper_Percent = 100 * interval[["upper"]],
          Missing_N = sum(is.na(variable_values))
        )
      }
    )
  }
)

numeric_descriptives <- purrr::map_dfr(
  c(atq_score_variables, neo_score_variables),
  function(variable_name) {
    values <- data_analysis[[variable_name]]
    valid_values <- values[is.finite(values)]
    mean_interval <- mean_ci(valid_values)
    tibble::tibble(
      Variable = variable_name,
      Label = dplyr::coalesce(
        unname(adherence_labels[variable_name]),
        unname(personality_labels[variable_name]),
        variable_name
      ),
      N = length(valid_values),
      Missing_N = sum(is.na(values)),
      Mean = mean(valid_values),
      SD = stats::sd(valid_values),
      Mean_CI_95_Lower = mean_interval[["lower"]],
      Mean_CI_95_Upper = mean_interval[["upper"]],
      Median = stats::median(valid_values),
      Q1 = stats::quantile(valid_values, 0.25, names = FALSE),
      Q3 = stats::quantile(valid_values, 0.75, names = FALSE),
      Minimum = min(valid_values),
      Maximum = max(valid_values),
      Skewness = psych::skew(valid_values),
      Kurtosis = psych::kurtosi(valid_values),
      Shapiro_W = ifelse(
        length(valid_values) >= 3L,
        stats::shapiro.test(valid_values)$statistic,
        NA_real_
      ),
      Shapiro_P = ifelse(
        length(valid_values) >= 3L,
        stats::shapiro.test(valid_values)$p.value,
        NA_real_
      )
    )
  }
)

item_descriptives <- purrr::map_dfr(
  c(atq_items, neo_items),
  function(variable_name) {
    values <- data_analysis[[variable_name]]
    valid_values <- values[is.finite(values)]
    tibble::tibble(
      Instrument = ifelse(
        stringr::str_starts(variable_name, "ATQ"),
        "ATQ",
        "NEO-FFI"
      ),
      Item = variable_name,
      N = length(valid_values),
      Missing_N = sum(is.na(values)),
      Mean = mean(valid_values),
      SD = stats::sd(valid_values),
      Median = stats::median(valid_values),
      Q1 = stats::quantile(valid_values, 0.25, names = FALSE),
      Q3 = stats::quantile(valid_values, 0.75, names = FALSE),
      Minimum = min(valid_values),
      Maximum = max(valid_values),
      Floor_Percent = 100 * mean(valid_values == 0),
      Ceiling_Percent = 100 * mean(valid_values == 4)
    )
  }
)

scale_range_map <- tibble::tribble(
  ~Variable, ~Theoretical_Minimum, ~Theoretical_Maximum,
  "ATQ_Total_Pct", 0, 100,
  "ATQ_Persistence_Pct", 0, 100,
  "ATQ_Participation_Pct", 0, 100,
  "ATQ_Adaptability_Pct", 0, 100,
  "ATQ_Integration_Pct", 0, 100,
  "ATQ_Sticking_Pct", 0, 100,
  "ATQ_Commitment_Pct", 0, 100,
  "ATQ_Execution_Certainty_Pct", 0, 100,
  "NEO_Neuroticism", 0, 48,
  "NEO_Extraversion", 0, 48,
  "NEO_Openness", 0, 48,
  "NEO_Agreeableness", 0, 48,
  "NEO_Conscientiousness", 0, 48
)

floor_ceiling_table <- scale_range_map |>
  dplyr::rowwise() |>
  dplyr::mutate(
    Valid_N = sum(!is.na(data_analysis[[Variable]])),
    Floor_N = sum(
      data_analysis[[Variable]] == Theoretical_Minimum,
      na.rm = TRUE
    ),
    Ceiling_N = sum(
      data_analysis[[Variable]] == Theoretical_Maximum,
      na.rm = TRUE
    ),
    Floor_Percent = 100 * Floor_N / Valid_N,
    Ceiling_Percent = 100 * Ceiling_N / Valid_N
  ) |>
  dplyr::ungroup()

write_excel_report(
  file_path = file.path(
    section_paths[["02_Descriptive_Analysis"]],
    "Tables",
    "02_Descriptive_Analysis.xlsx"
  ),
  sheets = list(
    Table_1_Categorical = categorical_descriptives,
    Scale_Summaries = numeric_descriptives,
    Item_Descriptives = item_descriptives,
    Floor_and_Ceiling = floor_ceiling_table
  ),
  report_title = "Descriptive Analysis",
  report_note = paste(
    "Categorical variables are summarized as n (%) with Wilson 95% CIs.",
    "Scale scores are summarized using both parametric and robust statistics."
  )
)

atq_distribution_plot <- ggplot2::ggplot(
  data_analysis,
  ggplot2::aes(x = ATQ_Total_Pct)
) +
  ggplot2::geom_histogram(
    ggplot2::aes(y = after_stat(density)),
    bins = 18,
    fill = palette_journal[["blue"]],
    color = "white",
    linewidth = 0.35,
    alpha = 0.9
  ) +
  ggplot2::geom_density(
    color = palette_journal[["vermillion"]],
    linewidth = 1.1
  ) +
  ggplot2::geom_vline(
    xintercept = mean(data_analysis$ATQ_Total_Pct, na.rm = TRUE),
    color = palette_journal[["navy"]],
    linewidth = 0.8,
    linetype = "dashed"
  ) +
  ggplot2::geom_vline(
    xintercept = stats::median(data_analysis$ATQ_Total_Pct, na.rm = TRUE),
    color = palette_journal[["gold"]],
    linewidth = 0.8,
    linetype = "dotdash"
  ) +
  ggplot2::scale_x_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 10)
  ) +
  ggplot2::labs(
    title = "Distribution of overall treatment adherence",
    subtitle = paste0(
      "Mean = ",
      sprintf("%.1f", mean(data_analysis$ATQ_Total_Pct, na.rm = TRUE)),
      "; SD = ",
      sprintf("%.1f", stats::sd(data_analysis$ATQ_Total_Pct, na.rm = TRUE)),
      "; median = ",
      sprintf("%.1f", stats::median(data_analysis$ATQ_Total_Pct, na.rm = TRUE))
    ),
    x = "ATQ total standardized score (0-100)",
    y = "Density",
    caption = "Dashed navy line: mean; gold dot-dash line: median."
  ) +
  theme_journal()

atq_domain_long <- data_analysis |>
  dplyr::select(Study_ID, dplyr::all_of(atq_score_variables[-1])) |>
  tidyr::pivot_longer(
    cols = -Study_ID,
    names_to = "Domain",
    values_to = "Score"
  ) |>
  dplyr::mutate(
    Domain = factor(
      adherence_labels[Domain],
      levels = unname(adherence_labels[atq_score_variables[-1]])
    )
  )

atq_domain_summary <- atq_domain_long |>
  dplyr::group_by(Domain) |>
  dplyr::summarise(
    N = sum(!is.na(Score)),
    Mean = mean(Score, na.rm = TRUE),
    SD = stats::sd(Score, na.rm = TRUE),
    SE = SD / sqrt(N),
    CI_Lower = Mean - stats::qt(0.975, N - 1) * SE,
    CI_Upper = Mean + stats::qt(0.975, N - 1) * SE,
    .groups = "drop"
  )

atq_domain_plot <- ggplot2::ggplot(
  atq_domain_summary,
  ggplot2::aes(x = Domain, y = Mean, group = 1)
) +
  ggplot2::geom_ribbon(
    ggplot2::aes(ymin = CI_Lower, ymax = CI_Upper, group = 1),
    fill = palette_journal[["pale_blue"]],
    alpha = 0.8
  ) +
  ggplot2::geom_line(
    color = palette_journal[["navy"]],
    linewidth = 0.9
  ) +
  ggplot2::geom_point(
    color = palette_journal[["teal"]],
    size = 3
  ) +
  ggplot2::scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20)
  ) +
  ggplot2::labs(
    title = "Treatment-adherence domain profile",
    subtitle = "Means with 95% confidence intervals",
    x = NULL,
    y = "Standardized domain score (0-100)"
  ) +
  theme_journal() +
  ggplot2::theme(
    axis.text.x = element_text(angle = 28, hjust = 1)
  )

neo_long <- data_analysis |>
  dplyr::select(Study_ID, dplyr::all_of(neo_score_variables)) |>
  tidyr::pivot_longer(
    cols = -Study_ID,
    names_to = "Trait",
    values_to = "Score"
  ) |>
  dplyr::mutate(
    Trait = factor(
      personality_labels[Trait],
      levels = unname(personality_labels)
    )
  )

neo_distribution_plot <- ggplot2::ggplot(
  neo_long,
  ggplot2::aes(x = Trait, y = Score, fill = Trait)
) +
  ggplot2::geom_violin(
    trim = FALSE,
    alpha = 0.55,
    color = NA
  ) +
  ggplot2::geom_boxplot(
    width = 0.18,
    outlier.shape = NA,
    fill = "white",
    color = palette_journal[["navy"]],
    linewidth = 0.45
  ) +
  ggplot2::scale_fill_manual(
    values = c(
      palette_journal[["vermillion"]],
      palette_journal[["blue"]],
      palette_journal[["gold"]],
      palette_journal[["green"]],
      palette_journal[["purple"]]
    )
  ) +
  ggplot2::scale_y_continuous(limits = c(0, 48), breaks = seq(0, 48, 8)) +
  ggplot2::labs(
    title = "Distribution of NEO-FFI personality-domain scores",
    subtitle = "Violin distributions with median and interquartile range",
    x = NULL,
    y = "NEO-FFI score (0-48)"
  ) +
  theme_journal() +
  ggplot2::theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 22, hjust = 1)
  )

adherence_level_plot <- categorical_descriptives |>
  dplyr::filter(Characteristic == "Adherence category") |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = factor(Level, levels = c("Poor", "Moderate", "Good", "Very good")),
      y = Percent,
      fill = Level
    )
  ) +
  ggplot2::geom_col(width = 0.7, show.legend = FALSE) +
  ggplot2::geom_errorbar(
    ggplot2::aes(
      ymin = CI_95_Lower_Percent,
      ymax = CI_95_Upper_Percent
    ),
    width = 0.15,
    linewidth = 0.55,
    color = "#374151"
  ) +
  ggplot2::geom_text(
    ggplot2::aes(label = paste0(N, " (", sprintf("%.1f", Percent), "%)")),
    vjust = -0.6,
    size = 3.5,
    family = BASE_FONT_FAMILY
  ) +
  ggplot2::scale_fill_manual(
    values = c(
      Poor = palette_journal[["vermillion"]],
      Moderate = palette_journal[["gold"]],
      Good = palette_journal[["sky"]],
      `Very good` = palette_journal[["teal"]]
    )
  ) +
  ggplot2::scale_y_continuous(
    expand = ggplot2::expansion(mult = c(0, 0.08))
  ) +
  ggplot2::labs(
    title = "Distribution of treatment-adherence categories",
    subtitle = "Percentages with Wilson 95% confidence intervals",
    x = NULL,
    y = "Participants (%)"
  ) +
  theme_journal()

save_journal_figure(
  atq_distribution_plot,
  file.path(section_paths[["02_Descriptive_Analysis"]], "Figures"),
  "Figure_ATQ_Total_Distribution",
  width = 8,
  height = 5.4
)
save_journal_figure(
  atq_domain_plot,
  file.path(section_paths[["02_Descriptive_Analysis"]], "Figures"),
  "Figure_ATQ_Domain_Profile",
  width = 9.5,
  height = 5.5
)
save_journal_figure(
  neo_distribution_plot,
  file.path(section_paths[["02_Descriptive_Analysis"]], "Figures"),
  "Figure_NEO_Domain_Distributions",
  width = 9.5,
  height = 5.5
)
save_journal_figure(
  adherence_level_plot,
  file.path(section_paths[["02_Descriptive_Analysis"]], "Figures"),
  "Figure_Adherence_Categories",
  width = 8,
  height = 5.4
)


###### 09. PSYCHOMETRIC ANALYSIS ###############################################

safe_reliability_analysis <- function(
    data_frame,
    item_names,
    scale_name,
    instrument_name
) {
  item_data <- data_frame |>
    dplyr::select(dplyr::all_of(item_names)) |>
    as.data.frame()

  conventional_alpha <- tryCatch(
    psych::alpha(
      item_data,
      check.keys = FALSE,
      warnings = FALSE,
      na.rm = TRUE,
      use = "pairwise"
    ),
    error = function(error_condition) NULL
  )

  polychoric_result <- tryCatch(
    psych::polychoric(
      item_data,
      correct = 0.5,
      global = FALSE
    ),
    error = function(error_condition) NULL
  )

  ordinal_alpha <- NA_real_
  omega_total <- NA_real_

  if (!is.null(polychoric_result)) {
    smoothed_matrix <- tryCatch(
      psych::cor.smooth(polychoric_result$rho),
      error = function(error_condition) polychoric_result$rho
    )
    ordinal_alpha <- tryCatch(
      psych::alpha(
        smoothed_matrix,
        n.obs = nrow(item_data),
        check.keys = FALSE,
        warnings = FALSE
      )$total$raw_alpha,
      error = function(error_condition) NA_real_
    )
    omega_total <- tryCatch(
      psych::omega(
        smoothed_matrix,
        n.obs = nrow(item_data),
        nfactors = 1,
        plot = FALSE,
        warnings = FALSE,
        title = paste(instrument_name, scale_name)
      )$omega.tot,
      error = function(error_condition) NA_real_
    )
  }

  if (is.null(conventional_alpha)) {
    summary_row <- tibble::tibble(
      Instrument = instrument_name,
      Scale = scale_name,
      Items = length(item_names),
      N = sum(stats::complete.cases(item_data)),
      Cronbach_Alpha = NA_real_,
      Standardized_Alpha = NA_real_,
      Ordinal_Alpha = ordinal_alpha,
      McDonald_Omega_Total = omega_total,
      Mean_Interitem_Correlation = NA_real_
    )
    item_rows <- tibble::tibble(
      Instrument = character(),
      Scale = character(),
      Item = character(),
      Corrected_Item_Total_Correlation = numeric(),
      Alpha_If_Item_Deleted = numeric()
    )
  } else {
    summary_row <- tibble::tibble(
      Instrument = instrument_name,
      Scale = scale_name,
      Items = length(item_names),
      N = sum(stats::complete.cases(item_data)),
      Cronbach_Alpha = conventional_alpha$total$raw_alpha,
      Standardized_Alpha = conventional_alpha$total$std.alpha,
      Ordinal_Alpha = ordinal_alpha,
      McDonald_Omega_Total = omega_total,
      Mean_Interitem_Correlation = conventional_alpha$total$average_r
    )
    item_rows <- tibble::tibble(
      Instrument = instrument_name,
      Scale = scale_name,
      Item = rownames(conventional_alpha$item.stats),
      Corrected_Item_Total_Correlation = conventional_alpha$item.stats$r.drop,
      Alpha_If_Item_Deleted = conventional_alpha$alpha.drop$raw_alpha
    )
  }

  list(summary = summary_row, items = item_rows)
}

reliability_definitions <- c(
  list(ATQ_Total = atq_items),
  purrr::set_names(atq_domains, paste0("ATQ_", names(atq_domains))),
  purrr::set_names(neo_domains, paste0("NEO_", names(neo_domains)))
)

reliability_results <- purrr::imap(
  reliability_definitions,
  function(item_names, scale_name) {
    instrument_name <- ifelse(
      stringr::str_starts(scale_name, "ATQ"),
      "ATQ",
      "NEO-FFI"
    )
    safe_reliability_analysis(
      data_analysis,
      item_names,
      scale_name,
      instrument_name
    )
  }
)

reliability_summary <- purrr::map_dfr(reliability_results, "summary")
reliability_item_diagnostics <- purrr::map_dfr(reliability_results, "items")

write_excel_report(
  file_path = file.path(
    section_paths[["03_Psychometrics"]],
    "Tables",
    "03_Psychometric_Analysis.xlsx"
  ),
  sheets = list(
    Reliability = reliability_summary,
    Item_Diagnostics = reliability_item_diagnostics
  ),
  report_title = "Psychometric Analysis of ATQ and NEO-FFI",
  report_note = paste(
    "Reliability estimates assume that all item responses have already been",
    "keyed in the intended scoring direction. No item-level CFA is included",
    "because it was not part of the executed analysis."
  )
)

reliability_plot_data <- reliability_summary |>
  dplyr::select(
    Instrument,
    Scale,
    Cronbach_Alpha,
    Ordinal_Alpha,
    McDonald_Omega_Total
  ) |>
  tidyr::pivot_longer(
    cols = c(
      Cronbach_Alpha,
      Ordinal_Alpha,
      McDonald_Omega_Total
    ),
    names_to = "Reliability_Estimator",
    values_to = "Estimate"
  ) |>
  dplyr::mutate(
    Reliability_Estimator = dplyr::recode(
      Reliability_Estimator,
      Cronbach_Alpha = "Cronbach's alpha",
      Ordinal_Alpha = "Ordinal alpha",
      McDonald_Omega_Total = "McDonald's omega total"
    ),
    Scale = forcats::fct_rev(factor(Scale, levels = unique(Scale)))
  )

reliability_plot <- ggplot2::ggplot(
  reliability_plot_data,
  ggplot2::aes(
    x = Estimate,
    y = Scale,
    color = Reliability_Estimator,
    shape = Reliability_Estimator
  )
) +
  ggplot2::geom_vline(
    xintercept = 0.70,
    linetype = "dashed",
    color = palette_journal[["grey"]],
    linewidth = 0.6
  ) +
  ggplot2::geom_point(
    position = ggplot2::position_dodge(width = 0.48),
    size = 2.6,
    na.rm = TRUE
  ) +
  ggplot2::facet_grid(
    rows = ggplot2::vars(Instrument),
    scales = "free_y",
    space = "free_y"
  ) +
  ggplot2::scale_color_manual(
    values = c(
      "Cronbach's alpha" = palette_journal[["blue"]],
      "Ordinal alpha" = palette_journal[["gold"]],
      "McDonald's omega total" = palette_journal[["teal"]]
    )
  ) +
  ggplot2::scale_x_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.1)
  ) +
  ggplot2::labs(
    title = "Internal-consistency estimates by questionnaire domain",
    subtitle = "The dashed line at 0.70 is a conventional reference, not a universal pass/fail rule",
    x = "Reliability estimate",
    y = NULL,
    color = NULL,
    shape = NULL
  ) +
  theme_journal()

save_journal_figure(
  reliability_plot,
  file.path(section_paths[["03_Psychometrics"]], "Figures"),
  "Figure_Reliability_Estimates",
  width = 10,
  height = 8
)

###### 10. PRIMARY PERSONALITY-ADHERENCE ASSOCIATIONS ##########################

bootstrap_spearman <- function(
    data_frame,
    predictor,
    outcome,
    repetitions = BOOTSTRAP_REPETITIONS,
    seed = SEED
) {
  pair_data <- data_frame |>
    dplyr::select(
      CV_Group,
      Predictor = dplyr::all_of(predictor),
      Outcome = dplyr::all_of(outcome)
    ) |>
    tidyr::drop_na()

  correlation_test <- suppressWarnings(
    stats::cor.test(
      pair_data$Predictor,
      pair_data$Outcome,
      method = "spearman",
      exact = FALSE
    )
  )

  cluster_rows <- split(seq_len(nrow(pair_data)), pair_data$CV_Group)
  cluster_table <- data.frame(
    Cluster = names(cluster_rows),
    stringsAsFactors = FALSE
  )

  bootstrap_statistic <- function(data, indices) {
    sampled_clusters <- data$Cluster[indices]
    sampled_rows <- unlist(cluster_rows[sampled_clusters], use.names = FALSE)
    suppressWarnings(
      stats::cor(
        pair_data$Predictor[sampled_rows],
        pair_data$Outcome[sampled_rows],
        method = "spearman",
        use = "complete.obs"
      )
    )
  }

  set.seed(seed)
  bootstrap_object <- boot::boot(
    data = cluster_table,
    statistic = bootstrap_statistic,
    R = repetitions
  )
  bootstrap_values <- as.numeric(bootstrap_object$t)
  bootstrap_values <- bootstrap_values[is.finite(bootstrap_values)]

  confidence_interval <- if (length(bootstrap_values) >= 20L) {
    stats::quantile(
      bootstrap_values,
      probabilities = c(0.025, 0.975),
      na.rm = TRUE,
      names = FALSE
    )
  } else {
    c(NA_real_, NA_real_)
  }

  tibble::tibble(
    Predictor = predictor,
    Outcome = outcome,
    N = nrow(pair_data),
    Clusters = length(cluster_rows),
    Spearman_Rho = unname(correlation_test$estimate),
    CI_95_Lower = confidence_interval[1],
    CI_95_Upper = confidence_interval[2],
    P_Value = correlation_test$p.value
  )
}

association_grid <- tidyr::expand_grid(
  Predictor = neo_score_variables,
  Outcome = atq_score_variables
) |>
  dplyr::mutate(Analysis_Index = dplyr::row_number())

log_message(
  "Estimating ",
  nrow(association_grid),
  " personality-adherence correlations with cluster bootstrap intervals."
)

personality_adherence_associations <- purrr::pmap_dfr(
  association_grid,
  function(Predictor, Outcome, Analysis_Index) {
    bootstrap_spearman(
      data_frame = data_analysis,
      predictor = Predictor,
      outcome = Outcome,
      repetitions = BOOTSTRAP_REPETITIONS,
      seed = SEED + Analysis_Index
    )
  }
) |>
  dplyr::mutate(
    Predictor_Label = unname(personality_labels[Predictor]),
    Outcome_Label = unname(adherence_labels[Outcome]),
    Family = ifelse(
      Outcome == "ATQ_Total_Pct",
      "Primary: overall adherence",
      "Secondary: adherence domains"
    ),
    FDR_P_Value_All_40 = stats::p.adjust(P_Value, method = "BH"),
    FDR_P_Value_Within_Family = ave(
      P_Value,
      Family,
      FUN = function(value) stats::p.adjust(value, method = "BH")
    ),
    P_Value_Formatted = format_p_value(P_Value),
    FDR_P_Value_Formatted = format_p_value(FDR_P_Value_All_40)
  ) |>
  dplyr::arrange(
    factor(Outcome, levels = atq_score_variables),
    factor(Predictor, levels = neo_score_variables)
  )

primary_personality_associations <- personality_adherence_associations |>
  dplyr::filter(Outcome == "ATQ_Total_Pct") |>
  dplyr::mutate(
    Primary_FDR_P_Value = stats::p.adjust(P_Value, method = "BH"),
    Primary_FDR_P_Formatted = format_p_value(Primary_FDR_P_Value),
    Significant_Primary_FDR = Primary_FDR_P_Value < 0.05
  )

correlation_matrix_all_scores <- data_analysis |>
  dplyr::select(
    dplyr::all_of(c(neo_score_variables, atq_score_variables))
  ) |>
  stats::cor(use = "pairwise.complete.obs", method = "spearman") |>
  as.data.frame() |>
  tibble::rownames_to_column("Variable_1") |>
  tidyr::pivot_longer(
    cols = -Variable_1,
    names_to = "Variable_2",
    values_to = "Spearman_Rho"
  )

write_excel_report(
  file_path = file.path(
    section_paths[["04_Primary_Associations"]],
    "Tables",
    "04_Personality_Adherence_Associations.xlsx"
  ),
  sheets = list(
    Primary_NEO_vs_Total = primary_personality_associations,
    All_NEO_by_ATQ = personality_adherence_associations,
    Full_Score_Correlation_Matrix = correlation_matrix_all_scores
  ),
  report_title = "Personality Traits and Treatment Adherence",
  report_note = paste(
    "Spearman correlations are reported with cluster-bootstrap 95% intervals.",
    "Potential duplicate clusters are resampled as units. Benjamini-Hochberg",
    "adjustment is reported separately for the five primary tests and the",
    "complete 40-test matrix."
  )
)

association_heatmap_data <- personality_adherence_associations |>
  dplyr::mutate(
    Predictor_Label = factor(
      Predictor_Label,
      levels = rev(unname(personality_labels))
    ),
    Outcome_Label = factor(
      Outcome_Label,
      levels = unname(adherence_labels)
    ),
    Annotation = paste0(
      sprintf("%.2f", Spearman_Rho),
      ifelse(FDR_P_Value_All_40 < 0.001, "***",
        ifelse(FDR_P_Value_All_40 < 0.01, "**",
          ifelse(FDR_P_Value_All_40 < 0.05, "*", "")
        )
      )
    )
  )

association_heatmap <- ggplot2::ggplot(
  association_heatmap_data,
  ggplot2::aes(
    x = Outcome_Label,
    y = Predictor_Label,
    fill = Spearman_Rho
  )
) +
  ggplot2::geom_tile(color = "white", linewidth = 0.8) +
  ggplot2::geom_text(
    ggplot2::aes(label = Annotation),
    size = 3.4,
    family = BASE_FONT_FAMILY,
    color = "#111827"
  ) +
  ggplot2::scale_fill_gradient2(
    low = "#B2182B",
    mid = "white",
    high = "#2166AC",
    midpoint = 0,
    limits = c(-1, 1),
    breaks = seq(-1, 1, 0.25)
  ) +
  ggplot2::labs(
    title = "Personality-adherence association matrix",
    subtitle = "Spearman rho; asterisks use FDR adjustment across all 40 tests",
    x = NULL,
    y = NULL,
    fill = "Spearman\nrho",
    caption = "* q<0.05; ** q<0.01; *** q<0.001."
  ) +
  theme_journal() +
  ggplot2::theme(
    axis.text.x = element_text(angle = 35, hjust = 1),
    panel.grid = element_blank()
  )

primary_forest_data <- primary_personality_associations |>
  dplyr::mutate(
    Predictor_Label = factor(
      Predictor_Label,
      levels = rev(unname(personality_labels))
    )
  )

primary_forest_plot <- ggplot2::ggplot(
  primary_forest_data,
  ggplot2::aes(
    x = Spearman_Rho,
    y = Predictor_Label,
    color = Significant_Primary_FDR
  )
) +
  ggplot2::geom_vline(
    xintercept = 0,
    color = palette_journal[["grey"]],
    linewidth = 0.6,
    linetype = "dashed"
  ) +
  ggplot2::geom_errorbarh(
    ggplot2::aes(xmin = CI_95_Lower, xmax = CI_95_Upper),
    height = 0.18,
    linewidth = 0.75
  ) +
  ggplot2::geom_point(size = 3) +
  ggplot2::scale_color_manual(
    values = c(
      `TRUE` = palette_journal[["teal"]],
      `FALSE` = palette_journal[["grey"]]
    ),
    labels = c(
      `TRUE` = "Primary FDR q<0.05",
      `FALSE` = "Primary FDR q>=0.05"
    )
  ) +
  ggplot2::scale_x_continuous(
    limits = c(-1, 1),
    breaks = seq(-1, 1, 0.2)
  ) +
  ggplot2::labs(
    title = "Associations between personality traits and overall adherence",
    subtitle = "Spearman correlations with cluster-bootstrap 95% intervals",
    x = "Spearman rho",
    y = NULL,
    color = NULL
  ) +
  theme_journal()

save_journal_figure(
  association_heatmap,
  file.path(section_paths[["04_Primary_Associations"]], "Figures"),
  "Figure_Personality_ATQ_Heatmap",
  width = 11,
  height = 6.5
)
save_journal_figure(
  primary_forest_plot,
  file.path(section_paths[["04_Primary_Associations"]], "Figures"),
  "Figure_Primary_NEO_ATQ_Forest",
  width = 8.2,
  height = 5.2
)

###### 11. UNADJUSTED DEMOGRAPHIC AND CLINICAL ASSOCIATIONS ####################

demographic_test_map <- tibble::tribble(
  ~Variable, ~Display_Label, ~Type,
  "Age_Group_Factor", "Age group", "Ordinal",
  "Sex_Factor", "Sex", "Nominal",
  "Education_Factor", "Education level", "Ordinal",
  "Time_Since_Transplant_Factor", "Time since transplantation", "Ordinal",
  "Marital_Status_Factor", "Marital status", "Nominal",
  "Substance_Use_Factor", "History of substance use", "Binary",
  "Alcohol_Use_Factor", "History of alcohol use", "Binary",
  "Medication_Count_Factor", "Medication-count category", "Ordinal"
)

rank_biserial_manual <- function(outcome, group) {
  complete <- stats::complete.cases(outcome, group)
  outcome <- outcome[complete]
  group <- droplevels(factor(group[complete]))
  if (nlevels(group) != 2L) {
    return(NA_real_)
  }
  rank_values <- rank(outcome, ties.method = "average")
  first_group <- levels(group)[1]
  n_first <- sum(group == first_group)
  n_second <- sum(group != first_group)
  u_first <- sum(rank_values[group == first_group]) -
    n_first * (n_first + 1) / 2
  2 * u_first / (n_first * n_second) - 1
}

demographic_test_results <- purrr::pmap_dfr(
  demographic_test_map,
  function(Variable, Display_Label, Type) {
    pair_data <- data_analysis |>
      dplyr::transmute(
        Outcome = ATQ_Total_Pct,
        Group = .data[[Variable]]
      ) |>
      tidyr::drop_na()

    group_factor <- droplevels(factor(pair_data$Group))
    number_of_groups <- nlevels(group_factor)

    if (number_of_groups == 2L) {
      test_object <- stats::wilcox.test(
        pair_data$Outcome ~ group_factor,
        exact = FALSE,
        conf.int = FALSE
      )
      effect_size <- rank_biserial_manual(
        pair_data$Outcome,
        group_factor
      )
      effect_name <- paste0(
        "Rank-biserial correlation; positive means ",
        levels(group_factor)[1],
        " has higher ranks"
      )
      statistic_value <- unname(test_object$statistic)
    } else {
      test_object <- stats::kruskal.test(
        pair_data$Outcome ~ group_factor
      )
      h_value <- unname(test_object$statistic)
      effect_size <- max(
        0,
        (h_value - number_of_groups + 1) /
          (nrow(pair_data) - number_of_groups)
      )
      effect_name <- "Epsilon-squared"
      statistic_value <- h_value
    }

    trend_result <- if (Type == "Ordinal") {
      numeric_group <- as.numeric(group_factor)
      trend_test <- suppressWarnings(
        stats::cor.test(
          numeric_group,
          pair_data$Outcome,
          method = "spearman",
          exact = FALSE
        )
      )
      c(
        rho = unname(trend_test$estimate),
        p = trend_test$p.value
      )
    } else {
      c(rho = NA_real_, p = NA_real_)
    }

    tibble::tibble(
      Variable = Variable,
      Characteristic = Display_Label,
      N = nrow(pair_data),
      Groups = number_of_groups,
      Test = ifelse(
        number_of_groups == 2L,
        "Wilcoxon rank-sum",
        "Kruskal-Wallis"
      ),
      Statistic = statistic_value,
      P_Value = test_object$p.value,
      Effect_Size_Name = effect_name,
      Effect_Size = effect_size,
      Ordinal_Trend_Spearman_Rho = trend_result[["rho"]],
      Ordinal_Trend_P_Value = trend_result[["p"]]
    )
  }
) |>
  dplyr::mutate(
    FDR_P_Value = stats::p.adjust(P_Value, method = "BH"),
    Trend_FDR_P_Value = stats::p.adjust(
      Ordinal_Trend_P_Value,
      method = "BH"
    ),
    P_Value_Formatted = format_p_value(P_Value),
    FDR_P_Value_Formatted = format_p_value(FDR_P_Value)
  )

group_descriptive_results <- purrr::pmap_dfr(
  demographic_test_map,
  function(Variable, Display_Label, Type) {
    data_analysis |>
      dplyr::transmute(
        Group = as.character(.data[[Variable]]),
        ATQ_Total_Pct = ATQ_Total_Pct
      ) |>
      dplyr::filter(!is.na(Group)) |>
      dplyr::group_by(Group) |>
      dplyr::summarise(
        Characteristic = Display_Label,
        N = sum(!is.na(ATQ_Total_Pct)),
        Mean = mean(ATQ_Total_Pct, na.rm = TRUE),
        SD = stats::sd(ATQ_Total_Pct, na.rm = TRUE),
        Median = stats::median(ATQ_Total_Pct, na.rm = TRUE),
        Q1 = stats::quantile(
          ATQ_Total_Pct,
          0.25,
          na.rm = TRUE,
          names = FALSE
        ),
        Q3 = stats::quantile(
          ATQ_Total_Pct,
          0.75,
          na.rm = TRUE,
          names = FALSE
        ),
        .groups = "drop"
      ) |>
      dplyr::mutate(Variable = Variable, .before = 1)
  }
)

pairwise_demographic_results <- purrr::pmap_dfr(
  demographic_test_map,
  function(Variable, Display_Label, Type) {
    pair_data <- data_analysis |>
      dplyr::transmute(
        Outcome = ATQ_Total_Pct,
        Group = .data[[Variable]]
      ) |>
      tidyr::drop_na() |>
      dplyr::mutate(Group = droplevels(factor(Group)))

    if (nlevels(pair_data$Group) < 3L) {
      return(tibble::tibble())
    }

    level_pairs <- utils::combn(
      levels(pair_data$Group),
      2,
      simplify = FALSE
    )
    raw_results <- purrr::map_dfr(
      level_pairs,
      function(level_pair) {
        subset_data <- pair_data |>
          dplyr::filter(Group %in% level_pair) |>
          dplyr::mutate(Group = droplevels(Group))
        test_object <- stats::wilcox.test(
          Outcome ~ Group,
          data = subset_data,
          exact = FALSE
        )
        tibble::tibble(
          Variable = Variable,
          Characteristic = Display_Label,
          Group_1 = level_pair[1],
          Group_2 = level_pair[2],
          N = nrow(subset_data),
          Rank_Biserial = rank_biserial_manual(
            subset_data$Outcome,
            subset_data$Group
          ),
          P_Value = test_object$p.value
        )
      }
    )
    raw_results |>
      dplyr::mutate(
        Pairwise_FDR_P_Value = stats::p.adjust(P_Value, method = "BH")
      )
  }
)

write_excel_report(
  file_path = file.path(
    section_paths[["04_Primary_Associations"]],
    "Tables",
    "04_Demographic_and_Clinical_Associations.xlsx"
  ),
  sheets = list(
    Omnibus_Tests = demographic_test_results,
    Group_Descriptives = group_descriptive_results,
    Pairwise_Comparisons = pairwise_demographic_results
  ),
  report_title = "Unadjusted Associations With Overall Treatment Adherence",
  report_note = paste(
    "Omnibus tests are adjusted across eight prespecified objectives using",
    "the Benjamini-Hochberg method. Ordinal trend tests are reported",
    "separately and should not replace the omnibus comparison."
  )
)

###### 12. MULTIVARIABLE, HIERARCHICAL, AND ROBUST MODELS ######################

covariate_formula_terms <- c(
  "scale(Age_Group)",
  "Sex_Factor",
  "scale(Education_Level)",
  "scale(Time_Since_Transplant)",
  "Married_Binary",
  "Substance_Use_Factor",
  "Alcohol_Use_Factor",
  "scale(Medication_Count_Category)"
)

personality_formula_terms <- neo_score_variables

block_1_formula <- stats::as.formula(
  paste(
    "ATQ_Total_Pct ~",
    paste(covariate_formula_terms, collapse = " + ")
  )
)

block_2_formula <- stats::as.formula(
  paste(
    "ATQ_Total_Pct ~",
    paste(
      c(covariate_formula_terms, personality_formula_terms),
      collapse = " + "
    )
  )
)

model_data <- data_analysis |>
  dplyr::select(
    ATQ_Total_Pct,
    Age_Group,
    Sex_Factor,
    Education_Level,
    Time_Since_Transplant,
    Married_Binary,
    Substance_Use_Factor,
    Alcohol_Use_Factor,
    Medication_Count_Category,
    dplyr::all_of(neo_score_variables),
    Record_ID,
    CV_Group,
    Potential_Duplicate_Flag,
    Duplicate_Cluster
  )

complete_model_data <- model_data |>
  tidyr::drop_na()

fit_hc3_model <- function(formula, data_frame, model_name) {
  fitted_model <- stats::lm(formula, data = data_frame)
  robust_covariance <- sandwich::vcovHC(fitted_model, type = "HC3")
  robust_matrix <- lmtest::coeftest(
    fitted_model,
    vcov. = robust_covariance
  )
  robust_degrees_freedom <- stats::df.residual(fitted_model)
  critical_value <- stats::qt(0.975, df = robust_degrees_freedom)

  coefficient_table <- tibble::tibble(
    Model = model_name,
    Term = rownames(robust_matrix),
    Estimate = robust_matrix[, 1],
    Robust_SE_HC3 = robust_matrix[, 2],
    Test_Statistic = robust_matrix[, 3],
    P_Value = robust_matrix[, 4],
    CI_95_Lower = Estimate - critical_value * Robust_SE_HC3,
    CI_95_Upper = Estimate + critical_value * Robust_SE_HC3,
    Personality_FDR_P_Value = NA_real_
  )

  personality_rows <- coefficient_table$Term %in% neo_score_variables
  coefficient_table$Personality_FDR_P_Value[personality_rows] <-
    stats::p.adjust(
      coefficient_table$P_Value[personality_rows],
      method = "BH"
    )
  coefficient_table <- coefficient_table |>
    dplyr::mutate(P_Value_Formatted = format_p_value(P_Value))

  model_summary <- tibble::tibble(
    Model = model_name,
    N = stats::nobs(fitted_model),
    Parameters = length(stats::coef(fitted_model)),
    R_Squared = summary(fitted_model)$r.squared,
    Adjusted_R_Squared = summary(fitted_model)$adj.r.squared,
    Residual_SD = summary(fitted_model)$sigma,
    AIC = stats::AIC(fitted_model),
    BIC = stats::BIC(fitted_model)
  )

  list(
    object = fitted_model,
    coefficients = coefficient_table,
    summary = model_summary,
    covariance = robust_covariance
  )
}

block_1_fit <- fit_hc3_model(
  block_1_formula,
  complete_model_data,
  "Block 1: background variables"
)
block_2_fit <- fit_hc3_model(
  block_2_formula,
  complete_model_data,
  "Block 2: background plus personality"
)

standardized_model_data <- complete_model_data |>
  dplyr::mutate(
    ATQ_Total_Pct = as.numeric(scale(ATQ_Total_Pct)),
    dplyr::across(
      dplyr::all_of(neo_score_variables),
      function(value) as.numeric(scale(value))
    )
  )

standardized_fit <- fit_hc3_model(
  block_2_formula,
  standardized_model_data,
  "Standardized outcome and NEO domains"
)

robust_personality_block_test <- function(model_result, terms) {
  coefficient_names <- names(stats::coef(model_result$object))
  included_terms <- intersect(terms, coefficient_names)
  if (length(included_terms) == 0L) {
    return(tibble::tibble(
      Test = "Joint HC3 test of personality block",
      Numerator_DF = NA_integer_,
      Denominator_DF = stats::df.residual(model_result$object),
      F_Statistic = NA_real_,
      P_Value = NA_real_
    ))
  }

  beta <- stats::coef(model_result$object)[included_terms]
  covariance <- model_result$covariance[included_terms, included_terms, drop = FALSE]
  wald_chisq <- as.numeric(
    t(beta) %*% qr.solve(covariance, beta)
  )
  numerator_df <- length(included_terms)
  f_statistic <- wald_chisq / numerator_df
  denominator_df <- stats::df.residual(model_result$object)

  tibble::tibble(
    Test = "Joint HC3 test of personality block",
    Numerator_DF = numerator_df,
    Denominator_DF = denominator_df,
    F_Statistic = f_statistic,
    P_Value = stats::pf(
      f_statistic,
      df1 = numerator_df,
      df2 = denominator_df,
      lower.tail = FALSE
    )
  )
}

personality_block_test <- robust_personality_block_test(
  block_2_fit,
  neo_score_variables
) |>
  dplyr::mutate(P_Value_Formatted = format_p_value(P_Value))

cluster_bootstrap_delta_r_squared <- function(
    data_frame,
    repetitions = BOOTSTRAP_REPETITIONS,
    seed = SEED + 1000L
) {
  cluster_rows <- split(seq_len(nrow(data_frame)), data_frame$CV_Group)
  cluster_table <- data.frame(
    Cluster = names(cluster_rows),
    stringsAsFactors = FALSE
  )

  statistic <- function(data, indices) {
    sampled_clusters <- data$Cluster[indices]
    sampled_rows <- unlist(cluster_rows[sampled_clusters], use.names = FALSE)
    bootstrap_data <- data_frame[sampled_rows, , drop = FALSE]
    fit_1 <- tryCatch(
      stats::lm(block_1_formula, data = bootstrap_data),
      error = function(error_condition) NULL
    )
    fit_2 <- tryCatch(
      stats::lm(block_2_formula, data = bootstrap_data),
      error = function(error_condition) NULL
    )
    if (is.null(fit_1) || is.null(fit_2)) {
      return(NA_real_)
    }
    summary(fit_2)$r.squared - summary(fit_1)$r.squared
  }

  set.seed(seed)
  bootstrap_object <- boot::boot(
    cluster_table,
    statistic = statistic,
    R = repetitions
  )
  values <- as.numeric(bootstrap_object$t)
  values <- values[is.finite(values)]

  tibble::tibble(
    N = nrow(data_frame),
    Clusters = length(cluster_rows),
    Observed_Delta_R_Squared = (
      block_2_fit$summary$R_Squared - block_1_fit$summary$R_Squared
    ),
    Bootstrap_Mean_Delta_R_Squared = mean(values),
    Bootstrap_CI_95_Lower = stats::quantile(values, 0.025, names = FALSE),
    Bootstrap_CI_95_Upper = stats::quantile(values, 0.975, names = FALSE),
    Bootstrap_Repetitions_Valid = length(values)
  )
}

delta_r_squared_summary <- cluster_bootstrap_delta_r_squared(
  complete_model_data
)

model_diagnostics <- dplyr::bind_rows(
  tibble::tibble(
    Diagnostic = "Breusch-Pagan heteroscedasticity test",
    Statistic = unname(lmtest::bptest(block_2_fit$object)$statistic),
    P_Value = lmtest::bptest(block_2_fit$object)$p.value
  ),
  tibble::tibble(
    Diagnostic = "Ramsey RESET functional-form test",
    Statistic = unname(
      lmtest::resettest(
        block_2_fit$object,
        power = 2:3,
        type = "fitted"
      )$statistic
    ),
    P_Value = lmtest::resettest(
      block_2_fit$object,
      power = 2:3,
      type = "fitted"
    )$p.value
  ),
  tibble::tibble(
    Diagnostic = "Shapiro-Wilk test of residuals",
    Statistic = unname(
      stats::shapiro.test(stats::residuals(block_2_fit$object))$statistic
    ),
    P_Value = stats::shapiro.test(
      stats::residuals(block_2_fit$object)
    )$p.value
  )
) |>
  dplyr::mutate(P_Value_Formatted = format_p_value(P_Value))

collinearity_table <- tryCatch(
  as.data.frame(
    performance::check_collinearity(block_2_fit$object)
  ),
  error = function(error_condition) {
    tibble::tibble(
      Message = paste(
        "Collinearity diagnostics could not be computed:",
        conditionMessage(error_condition)
      )
    )
  }
)

influence_table <- broom::augment(block_2_fit$object) |>
  dplyr::mutate(
    Record_Index = dplyr::row_number(),
    Cook_Threshold = 4 / nrow(complete_model_data),
    Influential_Cook = .cooksd > Cook_Threshold,
    High_Leverage = .hat > (
      2 * length(stats::coef(block_2_fit$object)) /
        nrow(complete_model_data)
    )
  ) |>
  dplyr::select(
    Record_Index,
    .fitted,
    .resid,
    .std.resid,
    .hat,
    .cooksd,
    Cook_Threshold,
    Influential_Cook,
    High_Leverage
  )

robust_rlm_fit <- MASS::rlm(
  block_2_formula,
  data = complete_model_data,
  method = "MM",
  maxit = 200
)

robust_rlm_coefficients <- broom::tidy(robust_rlm_fit) |>
  dplyr::mutate(Model = "Robust MM regression", .before = 1)

beta_data <- complete_model_data |>
  dplyr::mutate(ATQ_Beta = ATQ_Total_Pct / 100)

beta_boundary_adjusted <- any(
  beta_data$ATQ_Beta <= 0 | beta_data$ATQ_Beta >= 1
)

if (beta_boundary_adjusted) {
  beta_sample_size <- nrow(beta_data)
  beta_data <- beta_data |>
    dplyr::mutate(
      ATQ_Beta = (
        ATQ_Beta * (beta_sample_size - 1) + 0.5
      ) / beta_sample_size
    )
}

beta_formula <- stats::update(block_2_formula, ATQ_Beta ~ .)

beta_regression_fit <- tryCatch(
  betareg::betareg(
    beta_formula,
    data = beta_data,
    link = "logit"
  ),
  error = function(error_condition) NULL
)

if (is.null(beta_regression_fit)) {
  beta_regression_coefficients <- tibble::tibble(
    Message = "Beta-regression sensitivity model failed."
  )
  beta_regression_summary <- tibble::tibble(
    Message = "Beta-regression sensitivity model failed."
  )
} else {
  beta_regression_coefficients <- broom::tidy(
    beta_regression_fit,
    conf.int = TRUE
  )
  beta_regression_summary <- tibble::tibble(
    N = stats::nobs(beta_regression_fit),
    Boundary_Adjustment_Applied = beta_boundary_adjusted,
    Log_Likelihood = as.numeric(stats::logLik(beta_regression_fit)),
    AIC = stats::AIC(beta_regression_fit),
    BIC = stats::BIC(beta_regression_fit),
    Pseudo_R_Squared = summary(beta_regression_fit)$pseudo.r.squared
  )
}

all_hc3_coefficients <- dplyr::bind_rows(
  block_1_fit$coefficients,
  block_2_fit$coefficients,
  standardized_fit$coefficients
)

all_model_summaries <- dplyr::bind_rows(
  block_1_fit$summary,
  block_2_fit$summary,
  standardized_fit$summary
)

write_excel_report(
  file_path = file.path(
    section_paths[["05_Adjusted_Models"]],
    "Tables",
    "05_Adjusted_and_Hierarchical_Models.xlsx"
  ),
  sheets = list(
    HC3_Coefficients = all_hc3_coefficients,
    Model_Summaries = all_model_summaries,
    Personality_Block_HC3_Test = personality_block_test,
    Bootstrap_Delta_R2 = delta_r_squared_summary,
    Diagnostics = model_diagnostics,
    Collinearity = collinearity_table,
    Influence = influence_table,
    Robust_MM_Regression = robust_rlm_coefficients,
    Beta_Regression = beta_regression_coefficients,
    Beta_Model_Summary = beta_regression_summary
  ),
  report_title = "Adjusted Models of Overall Treatment Adherence",
  report_note = paste(
    "The primary adjusted model uses HC3 standard errors.",
    "The personality block is evaluated with a joint HC3 Wald test, and",
    "delta R-squared is summarized with a cluster bootstrap."
  )
)

standardized_personality_coefficients <- standardized_fit$coefficients |>
  dplyr::filter(Term %in% neo_score_variables) |>
  dplyr::mutate(
    Trait = factor(
      personality_labels[Term],
      levels = rev(unname(personality_labels))
    ),
    Significant = Personality_FDR_P_Value < 0.05
  )

adjusted_coefficient_plot <- ggplot2::ggplot(
  standardized_personality_coefficients,
  ggplot2::aes(
    x = Estimate,
    y = Trait,
    color = Significant
  )
) +
  ggplot2::geom_vline(
    xintercept = 0,
    color = palette_journal[["grey"]],
    linewidth = 0.6,
    linetype = "dashed"
  ) +
  ggplot2::geom_errorbarh(
    ggplot2::aes(xmin = CI_95_Lower, xmax = CI_95_Upper),
    height = 0.17,
    linewidth = 0.75
  ) +
  ggplot2::geom_point(size = 3) +
  ggplot2::scale_color_manual(
    values = c(
      `TRUE` = palette_journal[["teal"]],
      `FALSE` = palette_journal[["grey"]]
    ),
    guide = "none"
  ) +
  ggplot2::labs(
    title = "Adjusted associations of personality traits with adherence",
    subtitle = "Standardized coefficients with HC3 95% confidence intervals",
    x = "Standardized coefficient",
    y = NULL
  ) +
  theme_journal()

diagnostic_augmented <- broom::augment(block_2_fit$object)

residual_fitted_plot <- ggplot2::ggplot(
  diagnostic_augmented,
  ggplot2::aes(x = .fitted, y = .std.resid)
) +
  ggplot2::geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = palette_journal[["grey"]]
  ) +
  ggplot2::geom_point(
    color = palette_journal[["blue"]],
    alpha = 0.7,
    size = 2
  ) +
  ggplot2::geom_smooth(
    method = "loess",
    se = TRUE,
    color = palette_journal[["vermillion"]],
    fill = palette_journal[["pale_red"]],
    linewidth = 0.8
  ) +
  ggplot2::labs(
    title = "Residuals versus fitted values",
    x = "Fitted ATQ total score",
    y = "Standardized residual"
  ) +
  theme_journal()

residual_qq_plot <- ggplot2::ggplot(
  diagnostic_augmented,
  ggplot2::aes(sample = .std.resid)
) +
  ggplot2::stat_qq(
    color = palette_journal[["blue"]],
    alpha = 0.75,
    size = 2
  ) +
  ggplot2::stat_qq_line(
    color = palette_journal[["vermillion"]],
    linewidth = 0.8
  ) +
  ggplot2::labs(
    title = "Normal Q-Q plot of standardized residuals",
    x = "Theoretical quantiles",
    y = "Observed quantiles"
  ) +
  theme_journal()

diagnostic_panel <- residual_fitted_plot + residual_qq_plot +
  patchwork::plot_layout(ncol = 2)

save_journal_figure(
  adjusted_coefficient_plot,
  file.path(section_paths[["05_Adjusted_Models"]], "Figures"),
  "Figure_Adjusted_Personality_Coefficients",
  width = 8.2,
  height = 5.3
)
save_journal_figure(
  diagnostic_panel,
  file.path(section_paths[["05_Adjusted_Models"]], "Figures"),
  "Figure_Adjusted_Model_Diagnostics",
  width = 12,
  height = 5.5
)

###### 13. SENSITIVITY ANALYSES ################################################

one_per_cluster_data <- model_data |>
  dplyr::arrange(Record_ID) |>
  dplyr::mutate(
    Sensitivity_Key = ifelse(
      Duplicate_Cluster > 0,
      paste0("Cluster_", Duplicate_Cluster),
      paste0("Record_", Record_ID)
    )
  ) |>
  dplyr::group_by(Sensitivity_Key) |>
  dplyr::slice_head(n = 1L) |>
  dplyr::ungroup() |>
  dplyr::select(-Sensitivity_Key)

sensitivity_datasets <- list(
  `Primary complete-case analysis` = complete_model_data,
  `Exclude all potential duplicates` = model_data |>
    dplyr::filter(Potential_Duplicate_Flag == 0) |>
    tidyr::drop_na(),
  `Retain one record per flagged cluster` = one_per_cluster_data |>
    tidyr::drop_na()
)

sensitivity_model_results <- purrr::imap(
  sensitivity_datasets,
  function(data_frame, analysis_name) {
    fit_hc3_model(
      block_2_formula,
      data_frame,
      analysis_name
    )
  }
)

sensitivity_coefficients <- sensitivity_model_results |>
  purrr::map_dfr("coefficients")

sensitivity_summaries <- sensitivity_model_results |>
  purrr::map_dfr("summary")

sensitivity_personality_coefficients <- sensitivity_coefficients |>
  dplyr::filter(Term %in% neo_score_variables) |>
  dplyr::mutate(
    Trait = unname(personality_labels[Term])
  )

write_excel_report(
  file_path = file.path(
    section_paths[["06_Sensitivity_Analyses"]],
    "Tables",
    "06_Sensitivity_Analyses.xlsx"
  ),
  sheets = list(
    Personality_Coefficients = sensitivity_personality_coefficients,
    All_Coefficients = sensitivity_coefficients,
    Model_Summaries = sensitivity_summaries
  ),
  report_title = "Sensitivity Analyses for Potential Duplicate Records",
  report_note = paste(
    "The adjusted primary model is a complete-case analysis.",
    "Sensitivity analyses exclude all duplicate-flagged records or retain",
    "one deterministic record per flagged cluster."
  )
)

sensitivity_plot_data <- sensitivity_personality_coefficients |>
  dplyr::mutate(
    Trait = factor(Trait, levels = rev(unname(personality_labels))),
    Model = factor(Model, levels = names(sensitivity_datasets))
  )

sensitivity_plot <- ggplot2::ggplot(
  sensitivity_plot_data,
  ggplot2::aes(
    x = Estimate,
    y = Trait,
    color = Model
  )
) +
  ggplot2::geom_vline(
    xintercept = 0,
    linetype = "dashed",
    color = palette_journal[["grey"]]
  ) +
  ggplot2::geom_errorbarh(
    ggplot2::aes(xmin = CI_95_Lower, xmax = CI_95_Upper),
    position = ggplot2::position_dodge(width = 0.62),
    height = 0.12,
    linewidth = 0.55
  ) +
  ggplot2::geom_point(
    position = ggplot2::position_dodge(width = 0.62),
    size = 2.2
  ) +
  ggplot2::scale_color_manual(
    values = c(
      palette_journal[["navy"]],
      palette_journal[["vermillion"]],
      palette_journal[["teal"]]
    )
  ) +
  ggplot2::labs(
    title = "Robustness of adjusted personality coefficients",
    subtitle = "Alternative handling of potential duplicate clusters",
    x = "Adjusted coefficient in ATQ percentage points",
    y = NULL,
    color = "Analysis set"
  ) +
  theme_journal()

save_journal_figure(
  sensitivity_plot,
  file.path(section_paths[["06_Sensitivity_Analyses"]], "Figures"),
  "Figure_Sensitivity_Personality_Coefficients",
  width = 10.5,
  height = 6.2
)

###### 14. MACHINE-LEARNING DATA AND RESAMPLING DESIGN #########################

ml_predictors <- c(
  "Age_Group_Factor",
  "Sex_Factor",
  "Education_Factor",
  "Time_Since_Transplant_Factor",
  "Marital_Status_Factor",
  "Substance_Use_Factor",
  "Alcohol_Use_Factor",
  "Medication_Count_Factor",
  neo_score_variables
)

ml_base_data <- data_analysis |>
  dplyr::mutate(
    dplyr::across(
      dplyr::all_of(c(
        "Age_Group_Factor",
        "Education_Factor",
        "Time_Since_Transplant_Factor",
        "Medication_Count_Factor"
      )),
      function(value) factor(as.character(value), levels = levels(value))
    )
  ) |>
  dplyr::select(
    Record_ID,
    CV_Group,
    ATQ_Total_Pct,
    ATQ_High,
    dplyr::all_of(ml_predictors)
  )

ml_regression_data <- ml_base_data |>
  dplyr::select(
    Record_ID,
    CV_Group,
    ATQ_Total_Pct,
    dplyr::all_of(ml_predictors)
  )

ml_classification_data <- ml_base_data |>
  dplyr::select(
    Record_ID,
    CV_Group,
    ATQ_High,
    dplyr::all_of(ml_predictors)
  )

make_ml_recipe <- function(data_frame, outcome_name) {
  recipes::recipe(
    stats::as.formula(paste(outcome_name, "~ .")),
    data = data_frame
  ) |>
    recipes::update_role(
      Record_ID,
      CV_Group,
      new_role = "id"
    ) |>
    recipes::step_unknown(
      recipes::all_nominal_predictors(),
      new_level = "Unknown"
    ) |>
    recipes::step_novel(
      recipes::all_nominal_predictors(),
      new_level = "Novel"
    ) |>
    recipes::step_other(
      recipes::all_nominal_predictors(),
      threshold = 0.025,
      other = "Other"
    ) |>
    recipes::step_impute_median(
      recipes::all_numeric_predictors()
    ) |>
    recipes::step_dummy(
      recipes::all_nominal_predictors(),
      one_hot = FALSE
    ) |>
    recipes::step_nzv(
      recipes::all_predictors()
    ) |>
    recipes::step_normalize(
      recipes::all_numeric_predictors()
    )
}

regression_candidates <- list(
  Ordinary_Least_Squares = list(
    specification = parsnip::linear_reg() |>
      parsnip::set_engine("lm"),
    tuned = FALSE,
    grid = NULL
  ),
  Elastic_Net = list(
    specification = parsnip::linear_reg(
      penalty = tune::tune(),
      mixture = tune::tune()
    ) |>
      parsnip::set_engine("glmnet"),
    tuned = TRUE,
    grid = dials::grid_space_filling(
      dials::penalty(range = c(-5, 1)),
      dials::mixture(range = c(0, 1)),
      size = TUNING_GRID_SIZE
    )
  ),
  Random_Forest = list(
    specification = parsnip::rand_forest(
      mtry = tune::tune(),
      min_n = tune::tune(),
      trees = 1200L
    ) |>
      parsnip::set_engine(
        "ranger",
        importance = "permutation",
        num.threads = 1L
      ) |>
      parsnip::set_mode("regression"),
    tuned = TRUE,
    grid = dials::grid_space_filling(
      dials::mtry(range = c(1L, 12L)),
      dials::min_n(range = c(3L, 30L)),
      size = TUNING_GRID_SIZE
    )
  ),
  XGBoost = list(
    specification = parsnip::boost_tree(
      trees = 1200L,
      tree_depth = tune::tune(),
      learn_rate = tune::tune(),
      min_n = tune::tune(),
      loss_reduction = tune::tune(),
      sample_size = tune::tune()
    ) |>
      parsnip::set_engine(
        "xgboost",
        nthread = 1L,
        verbosity = 0
      ) |>
      parsnip::set_mode("regression"),
    tuned = TRUE,
    grid = dials::grid_space_filling(
      dials::tree_depth(range = c(1L, 5L)),
      dials::learn_rate(range = c(-4, -0.5)),
      dials::min_n(range = c(3L, 25L)),
      dials::loss_reduction(range = c(-5, 1)),
      dials::sample_prop(range = c(0.55, 1.0)),
      size = TUNING_GRID_SIZE
    )
  ),
  RBF_SVM = list(
    specification = parsnip::svm_rbf(
      cost = tune::tune(),
      rbf_sigma = tune::tune(),
      margin = tune::tune()
    ) |>
      parsnip::set_engine("kernlab") |>
      parsnip::set_mode("regression"),
    tuned = TRUE,
    grid = dials::grid_space_filling(
      dials::cost(range = c(-5, 5)),
      dials::rbf_sigma(range = c(-6, 0)),
      dials::svm_margin(range = c(0.01, 0.30)),
      size = TUNING_GRID_SIZE
    )
  )
)

classification_candidates <- list(
  Logistic_Regression = list(
    specification = parsnip::logistic_reg() |>
      parsnip::set_engine("glm"),
    tuned = FALSE,
    grid = NULL
  ),
  Elastic_Net = list(
    specification = parsnip::logistic_reg(
      penalty = tune::tune(),
      mixture = tune::tune()
    ) |>
      parsnip::set_engine("glmnet"),
    tuned = TRUE,
    grid = dials::grid_space_filling(
      dials::penalty(range = c(-5, 1)),
      dials::mixture(range = c(0, 1)),
      size = TUNING_GRID_SIZE
    )
  ),
  Random_Forest = list(
    specification = parsnip::rand_forest(
      mtry = tune::tune(),
      min_n = tune::tune(),
      trees = 1200L
    ) |>
      parsnip::set_engine(
        "ranger",
        importance = "permutation",
        num.threads = 1L
      ) |>
      parsnip::set_mode("classification"),
    tuned = TRUE,
    grid = dials::grid_space_filling(
      dials::mtry(range = c(1L, 12L)),
      dials::min_n(range = c(3L, 30L)),
      size = TUNING_GRID_SIZE
    )
  ),
  XGBoost = list(
    specification = parsnip::boost_tree(
      trees = 1200L,
      tree_depth = tune::tune(),
      learn_rate = tune::tune(),
      min_n = tune::tune(),
      loss_reduction = tune::tune(),
      sample_size = tune::tune()
    ) |>
      parsnip::set_engine(
        "xgboost",
        nthread = 1L,
        verbosity = 0
      ) |>
      parsnip::set_mode("classification"),
    tuned = TRUE,
    grid = dials::grid_space_filling(
      dials::tree_depth(range = c(1L, 5L)),
      dials::learn_rate(range = c(-4, -0.5)),
      dials::min_n(range = c(3L, 25L)),
      dials::loss_reduction(range = c(-5, 1)),
      dials::sample_prop(range = c(0.55, 1.0)),
      size = TUNING_GRID_SIZE
    )
  ),
  RBF_SVM = list(
    specification = parsnip::svm_rbf(
      cost = tune::tune(),
      rbf_sigma = tune::tune()
    ) |>
      parsnip::set_engine("kernlab") |>
      parsnip::set_mode("classification"),
    tuned = TRUE,
    grid = dials::grid_space_filling(
      dials::cost(range = c(-5, 5)),
      dials::rbf_sigma(range = c(-6, 0)),
      size = TUNING_GRID_SIZE
    )
  )
)


parallel_cluster <- NULL
if (
  PARALLEL_WORKERS > 1L &&
    (ML_REGRESSION_ENABLED || ML_CLASSIFICATION_ENABLED)
) {
  parallel_cluster <- parallel::makePSOCKcluster(PARALLEL_WORKERS)
  doParallel::registerDoParallel(parallel_cluster)
  log_message(
    "Registered ",
    PARALLEL_WORKERS,
    " parallel workers for inner resampling."
  )
}

add_group_stratum <- function(data_frame, outcome_name, mode) {
  if (mode == "classification") {
    group_strata <- data_frame |>
      dplyr::group_by(CV_Group) |>
      dplyr::summarise(
        CV_Stratum = ifelse(
          mean(.data[[outcome_name]] == "Very_Good", na.rm = TRUE) >= 0.5,
          "Very_Good_Group",
          "Below_Very_Good_Group"
        ),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        CV_Stratum = factor(
          CV_Stratum,
          levels = c("Very_Good_Group", "Below_Very_Good_Group")
        )
      )
  } else {
    group_strata <- data_frame |>
      dplyr::group_by(CV_Group) |>
      dplyr::summarise(
        CV_Stratum = mean(.data[[outcome_name]], na.rm = TRUE),
        .groups = "drop"
      )
  }

  data_frame |>
    dplyr::left_join(group_strata, by = "CV_Group")
}

make_grouped_resamples <- function(
    data_frame,
    outcome_name,
    mode,
    folds,
    repeats,
    seed
) {
  resample_data <- add_group_stratum(data_frame, outcome_name, mode)
  set.seed(seed)

  tryCatch(
    rsample::group_vfold_cv(
      data = resample_data,
      group = CV_Group,
      v = folds,
      repeats = repeats,
      strata = CV_Stratum,
      balance = "observations"
    ),
    error = function(error_condition) {
      warning(
        "Stratified grouped resampling failed; grouped resampling without ",
        "stratification was used. Reason: ",
        conditionMessage(error_condition)
      )
      set.seed(seed)
      rsample::group_vfold_cv(
        data = resample_data,
        group = CV_Group,
        v = folds,
        repeats = repeats,
        balance = "observations"
      )
    }
  )
}

extract_repeat_and_fold <- function(resample_table, row_index) {
  if ("id2" %in% names(resample_table)) {
    c(
      Repeat = as.character(resample_table$id[[row_index]]),
      Fold = as.character(resample_table$id2[[row_index]])
    )
  } else {
    c(
      Repeat = "Repeat1",
      Fold = as.character(resample_table$id[[row_index]])
    )
  }
}

parameter_row_to_string <- function(parameter_row) {
  if (is.null(parameter_row) || ncol(parameter_row) == 0L) {
    return("No tuned parameters")
  }
  paste(
    paste0(
      names(parameter_row),
      "=",
      purrr::map_chr(
        parameter_row,
        function(value) {
          if (is.numeric(value)) {
            format(value, digits = 5, scientific = TRUE)
          } else {
            as.character(value)
          }
        }
      )
    ),
    collapse = "; "
  )
}

run_nested_cv <- function(
    data_frame,
    outcome_name,
    candidate_list,
    mode,
    outer_resamples
) {
  prediction_results <- list()
  tuning_results <- list()
  error_results <- list()
  result_index <- 1L
  tuning_index <- 1L
  error_index <- 1L

  for (outer_index in seq_len(nrow(outer_resamples))) {
    split_object <- outer_resamples$splits[[outer_index]]
    training_data <- rsample::analysis(split_object) |>
      dplyr::select(-dplyr::any_of("CV_Stratum"))
    assessment_data <- rsample::assessment(split_object) |>
      dplyr::select(-dplyr::any_of("CV_Stratum"))
    identifiers <- extract_repeat_and_fold(outer_resamples, outer_index)

    inner_seed <- SEED + outer_index * 100L
    inner_resamples <- make_grouped_resamples(
      training_data,
      outcome_name,
      mode,
      folds = INNER_FOLDS,
      repeats = INNER_REPEATS,
      seed = inner_seed
    )

    log_message(
      "Nested CV ",
      mode,
      ": ",
      identifiers[["Repeat"]],
      " / ",
      identifiers[["Fold"]],
      "."
    )

    if (mode == "regression") {
      prediction_results[[result_index]] <- tibble::tibble(
        Model = "Mean_Baseline",
        Repeat = identifiers[["Repeat"]],
        Fold = identifiers[["Fold"]],
        Record_ID = assessment_data$Record_ID,
        Truth = assessment_data[[outcome_name]],
        Prediction = mean(training_data[[outcome_name]], na.rm = TRUE)
      )
    } else {
      positive_prevalence <- mean(
        training_data[[outcome_name]] == "Very_Good",
        na.rm = TRUE
      )
      prediction_results[[result_index]] <- tibble::tibble(
        Model = "Prevalence_Baseline",
        Repeat = identifiers[["Repeat"]],
        Fold = identifiers[["Fold"]],
        Record_ID = assessment_data$Record_ID,
        Truth = factor(
          assessment_data[[outcome_name]],
          levels = c("Very_Good", "Below_Very_Good")
        ),
        Probability_Very_Good = positive_prevalence
      )
    }
    result_index <- result_index + 1L

    for (candidate_name in names(candidate_list)) {
      candidate <- candidate_list[[candidate_name]]
      candidate_seed <- inner_seed + match(candidate_name, names(candidate_list))
      set.seed(candidate_seed)

      recipe_object <- make_ml_recipe(training_data, outcome_name)
      workflow_object <- workflows::workflow() |>
        workflows::add_recipe(recipe_object) |>
        workflows::add_model(candidate$specification)

      fitted_workflow <- tryCatch(
        {
          if (isTRUE(candidate$tuned)) {
            inner_metrics <- if (mode == "regression") {
              yardstick::metric_set(yardstick::rmse, yardstick::mae)
            } else {
              yardstick::metric_set(
                yardstick::roc_auc,
                yardstick::pr_auc,
                yardstick::mn_log_loss
              )
            }

            tuned_object <- tune::tune_grid(
              object = workflow_object,
              resamples = inner_resamples,
              grid = candidate$grid,
              metrics = inner_metrics,
              control = tune::control_grid(
                save_pred = FALSE,
                save_workflow = FALSE,
                allow_par = PARALLEL_WORKERS > 1L,
                verbose = FALSE
              )
            )

            selection_metric <- ifelse(
              mode == "regression",
              "rmse",
              "roc_auc"
            )
            best_parameters <- tune::select_best(
              tuned_object,
              metric = selection_metric
            )

            tuning_results[[tuning_index]] <- tibble::tibble(
              Mode = mode,
              Model = candidate_name,
              Repeat = identifiers[["Repeat"]],
              Fold = identifiers[["Fold"]],
              Best_Parameters = parameter_row_to_string(best_parameters)
            )
            tuning_index <- tuning_index + 1L

            workflows::fit(
              tune::finalize_workflow(workflow_object, best_parameters),
              data = training_data
            )
          } else {
            workflows::fit(workflow_object, data = training_data)
          }
        },
        error = function(error_condition) {
          error_results[[error_index]] <<- tibble::tibble(
            Mode = mode,
            Model = candidate_name,
            Repeat = identifiers[["Repeat"]],
            Fold = identifiers[["Fold"]],
            Error = conditionMessage(error_condition)
          )
          error_index <<- error_index + 1L
          NULL
        }
      )

      if (is.null(fitted_workflow)) {
        next
      }

      if (mode == "regression") {
        predictions <- tryCatch(
          predict(
            fitted_workflow,
            new_data = assessment_data,
            type = "numeric"
          )$.pred,
          error = function(error_condition) NULL
        )
        if (is.null(predictions)) {
          error_results[[error_index]] <- tibble::tibble(
            Mode = mode,
            Model = candidate_name,
            Repeat = identifiers[["Repeat"]],
            Fold = identifiers[["Fold"]],
            Error = "Numeric prediction failed."
          )
          error_index <- error_index + 1L
          next
        }
        prediction_results[[result_index]] <- tibble::tibble(
          Model = candidate_name,
          Repeat = identifiers[["Repeat"]],
          Fold = identifiers[["Fold"]],
          Record_ID = assessment_data$Record_ID,
          Truth = assessment_data[[outcome_name]],
          Prediction = predictions
        )
      } else {
        probabilities <- tryCatch(
          predict(
            fitted_workflow,
            new_data = assessment_data,
            type = "prob"
          )$.pred_Very_Good,
          error = function(error_condition) NULL
        )
        if (is.null(probabilities)) {
          error_results[[error_index]] <- tibble::tibble(
            Mode = mode,
            Model = candidate_name,
            Repeat = identifiers[["Repeat"]],
            Fold = identifiers[["Fold"]],
            Error = "Class-probability prediction failed."
          )
          error_index <- error_index + 1L
          next
        }
        prediction_results[[result_index]] <- tibble::tibble(
          Model = candidate_name,
          Repeat = identifiers[["Repeat"]],
          Fold = identifiers[["Fold"]],
          Record_ID = assessment_data$Record_ID,
          Truth = factor(
            assessment_data[[outcome_name]],
            levels = c("Very_Good", "Below_Very_Good")
          ),
          Probability_Very_Good = probabilities
        )
      }
      result_index <- result_index + 1L
    }
  }

  list(
    predictions = dplyr::bind_rows(prediction_results),
    tuning = dplyr::bind_rows(tuning_results),
    errors = dplyr::bind_rows(error_results)
  )
}

check_prediction_completeness <- function(predictions, expected_records) {
  predictions |>
    dplyr::group_by(Model, Repeat) |>
    dplyr::summarise(
      Predictions = dplyr::n(),
      Unique_Records = dplyr::n_distinct(Record_ID),
      Complete = Predictions == expected_records &
        Unique_Records == expected_records,
      .groups = "drop"
    )
}

summarize_repeated_metrics <- function(metric_table) {
  metric_table |>
    tidyr::pivot_longer(
      cols = -c(Model, Repeat),
      names_to = "Metric",
      values_to = "Estimate"
    ) |>
    dplyr::filter(is.finite(Estimate)) |>
    dplyr::group_by(Model, Metric) |>
    dplyr::summarise(
      Complete_Repeats = dplyr::n(),
      Mean = mean(Estimate),
      SD = stats::sd(Estimate),
      Median = stats::median(Estimate),
      P2_5_Across_Repeats = stats::quantile(
        Estimate,
        0.025,
        names = FALSE
      ),
      P97_5_Across_Repeats = stats::quantile(
        Estimate,
        0.975,
        names = FALSE
      ),
      Minimum = min(Estimate),
      Maximum = max(Estimate),
      .groups = "drop"
    )
}

concordance_correlation <- function(truth, prediction) {
  2 * stats::cov(truth, prediction) /
    (
      stats::var(truth) +
        stats::var(prediction) +
        (mean(truth) - mean(prediction))^2
    )
}

fit_final_candidate <- function(
    data_frame,
    outcome_name,
    candidate_name,
    candidate,
    mode
) {
  set.seed(SEED + 9000L)
  recipe_object <- make_ml_recipe(data_frame, outcome_name)
  workflow_object <- workflows::workflow() |>
    workflows::add_recipe(recipe_object) |>
    workflows::add_model(candidate$specification)

  if (!isTRUE(candidate$tuned)) {
    fitted <- workflows::fit(workflow_object, data = data_frame)
    return(list(
      fit = fitted,
      best_parameters = tibble::tibble(
        Model = candidate_name,
        Best_Parameters = "No tuned parameters"
      ),
      tuning = NULL
    ))
  }

  final_resamples <- make_grouped_resamples(
    data_frame,
    outcome_name,
    mode,
    folds = INNER_FOLDS,
    repeats = FINAL_TUNING_REPEATS,
    seed = SEED + 9000L
  )

  final_metrics <- if (mode == "regression") {
    yardstick::metric_set(yardstick::rmse, yardstick::mae)
  } else {
    yardstick::metric_set(
      yardstick::roc_auc,
      yardstick::pr_auc,
      yardstick::mn_log_loss
    )
  }

  tuned_object <- tune::tune_grid(
    workflow_object,
    resamples = final_resamples,
    grid = candidate$grid,
    metrics = final_metrics,
    control = tune::control_grid(
      save_pred = FALSE,
      save_workflow = FALSE,
      allow_par = PARALLEL_WORKERS > 1L,
      verbose = FALSE
    )
  )

  selection_metric <- ifelse(mode == "regression", "rmse", "roc_auc")
  best_parameters <- tune::select_best(
    tuned_object,
    metric = selection_metric
  )
  finalized_workflow <- tune::finalize_workflow(
    workflow_object,
    best_parameters
  )
  fitted <- workflows::fit(finalized_workflow, data = data_frame)

  list(
    fit = fitted,
    best_parameters = tibble::tibble(
      Model = candidate_name,
      Best_Parameters = parameter_row_to_string(best_parameters)
    ),
    tuning = tuned_object
  )
}

calculate_shap_importance <- function(
    fitted_workflow,
    data_frame,
    outcome_name,
    mode
) {
  predictor_frame <- data_frame |>
    dplyr::select(-dplyr::all_of(outcome_name))

  prediction_wrapper <- if (mode == "regression") {
    function(object, newdata) {
      predict(
        object,
        new_data = newdata,
        type = "numeric"
      )$.pred
    }
  } else {
    function(object, newdata) {
      predict(
        object,
        new_data = newdata,
        type = "prob"
      )$.pred_Very_Good
    }
  }

  set.seed(SEED + 9100L)
  shap_values <- fastshap::explain(
    object = fitted_workflow,
    X = predictor_frame,
    pred_wrapper = prediction_wrapper,
    nsim = SHAP_SIMULATIONS,
    adjust = TRUE
  )

  importance_table <- tibble::tibble(
    Variable = colnames(shap_values),
    Mean_Absolute_SHAP = colMeans(
      abs(as.matrix(shap_values)),
      na.rm = TRUE
    )
  ) |>
    dplyr::filter(!Variable %in% c("Record_ID", "CV_Group")) |>
    dplyr::arrange(dplyr::desc(Mean_Absolute_SHAP)) |>
    dplyr::mutate(
      Relative_Importance_Percent = (
        100 * Mean_Absolute_SHAP / sum(Mean_Absolute_SHAP)
      )
    )

  list(
    importance = importance_table,
    shap_values = shap_values
  )
}

###### 15. NESTED-CV REGRESSION OF CONTINUOUS ADHERENCE ########################

if (ML_REGRESSION_ENABLED) {
  log_message("Starting grouped nested repeated CV for continuous adherence.")

  outer_regression_resamples <- make_grouped_resamples(
    ml_regression_data,
    "ATQ_Total_Pct",
    "regression",
    folds = OUTER_FOLDS,
    repeats = OUTER_REPEATS,
    seed = SEED
  )

  regression_nested_results <- run_nested_cv(
    data_frame = ml_regression_data,
    outcome_name = "ATQ_Total_Pct",
    candidate_list = regression_candidates,
    mode = "regression",
    outer_resamples = outer_regression_resamples
  )

  regression_predictions <- regression_nested_results$predictions
  regression_prediction_completeness <- check_prediction_completeness(
    regression_predictions,
    expected_records = nrow(ml_regression_data)
  )

  complete_regression_keys <- regression_prediction_completeness |>
    dplyr::filter(Complete) |>
    dplyr::select(Model, Repeat)

  regression_predictions_complete <- regression_predictions |>
    dplyr::inner_join(
      complete_regression_keys,
      by = c("Model", "Repeat")
    )

  regression_metrics_by_repeat <- regression_predictions_complete |>
    dplyr::group_by(Model, Repeat) |>
    dplyr::summarise(
      RMSE = sqrt(mean((Truth - Prediction)^2)),
      MAE = mean(abs(Truth - Prediction)),
      R_Squared_Traditional = 1 -
        sum((Truth - Prediction)^2) /
        sum((Truth - mean(Truth))^2),
      CCC = concordance_correlation(Truth, Prediction),
      Bias = mean(Prediction - Truth),
      .groups = "drop"
    )

  regression_performance_summary <- summarize_repeated_metrics(
    regression_metrics_by_repeat
  )

  regression_calibration <- regression_predictions_complete |>
    dplyr::group_by(Model, Repeat) |>
    dplyr::group_modify(
      function(data_subset, group_keys) {
        calibration_fit <- stats::lm(
          Truth ~ Prediction,
          data = data_subset
        )
        tibble::tibble(
          Calibration_Intercept = stats::coef(calibration_fit)[1],
          Calibration_Slope = stats::coef(calibration_fit)[2]
        )
      }
    ) |>
    dplyr::ungroup()

  best_regression_model <- regression_performance_summary |>
    dplyr::filter(
      Metric == "RMSE",
      Model != "Mean_Baseline",
      is.finite(Mean)
    ) |>
    dplyr::arrange(Mean) |>
    dplyr::slice_head(n = 1L) |>
    dplyr::pull(Model)

  if (length(best_regression_model) == 0L) {
    stop("No regression candidate completed a full outer-CV repeat.")
  }

  log_message(
    "Best regression model by mean outer-CV RMSE: ",
    best_regression_model,
    "."
  )

  final_regression_fit <- tryCatch(
    fit_final_candidate(
      data_frame = ml_regression_data,
      outcome_name = "ATQ_Total_Pct",
      candidate_name = best_regression_model,
      candidate = regression_candidates[[best_regression_model]],
      mode = "regression"
    ),
    error = function(error_condition) {
      log_message(
        "Final fit for ",
        best_regression_model,
        " failed: ",
        conditionMessage(error_condition),
        ". Falling back to Ordinary_Least_Squares."
      )
      NULL
    }
  )
  if (is.null(final_regression_fit)) {
    best_regression_model <- "Ordinary_Least_Squares"
    final_regression_fit <- fit_final_candidate(
      data_frame = ml_regression_data,
      outcome_name = "ATQ_Total_Pct",
      candidate_name = best_regression_model,
      candidate = regression_candidates[[best_regression_model]],
      mode = "regression"
    )
  }

  if (SAVE_FITTED_MODELS) {
    saveRDS(
      final_regression_fit$fit,
      file = file.path(
        section_paths[["07_ML_Regression"]],
        "Objects",
        "Final_Regression_Workflow.rds"
      )
    )
  }

  if (SHAP_ENABLED) {
    regression_shap <- tryCatch(
      calculate_shap_importance(
        fitted_workflow = final_regression_fit$fit,
        data_frame = ml_regression_data,
        outcome_name = "ATQ_Total_Pct",
        mode = "regression"
      ),
      error = function(error_condition) {
        log_message(
          "Regression SHAP analysis failed: ",
          conditionMessage(error_condition)
        )
        list(
          importance = tibble::tibble(
            Message = conditionMessage(error_condition)
          ),
          shap_values = NULL
        )
      }
    )
  } else {
    regression_shap <- list(
      importance = tibble::tibble(Message = "SHAP analysis was disabled."),
      shap_values = NULL
    )
  }

  averaged_regression_predictions <- regression_predictions_complete |>
    dplyr::group_by(Model, Record_ID) |>
    dplyr::summarise(
      Truth = dplyr::first(Truth),
      Prediction = mean(Prediction),
      Prediction_SD_Across_Repeats = stats::sd(Prediction),
      Repeated_Predictions = dplyr::n(),
      .groups = "drop"
    )

  regression_sheets <- list(
    Performance_Summary = regression_performance_summary,
    Metrics_by_Repeat = regression_metrics_by_repeat,
    Prediction_Completeness = regression_prediction_completeness,
    Calibration_by_Repeat = regression_calibration,
    Final_Hyperparameters = final_regression_fit$best_parameters,
    SHAP_Importance = regression_shap$importance,
    Tuning_Choices = regression_nested_results$tuning,
    Errors = regression_nested_results$errors
  )
  if (EXPORT_ROW_LEVEL_OUTPUTS) {
    regression_sheets$Averaged_OOF_Predictions <- averaged_regression_predictions
    regression_sheets$All_Outer_Predictions <- regression_predictions
  }

  write_excel_report(
    file_path = file.path(
      section_paths[["07_ML_Regression"]],
      "Tables",
      "07_ML_Regression_Nested_CV.xlsx"
    ),
    sheets = regression_sheets,
    report_title = "Nested Cross-Validated Prediction of Continuous Adherence",
    report_note = paste(
      "Performance is summarized across complete grouped outer-CV repeats.",
      "The 2.5th and 97.5th percentiles describe the empirical distribution",
      "across repeats and are not labelled as confidence intervals."
    )
  )

  regression_rmse_plot <- regression_metrics_by_repeat |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = forcats::fct_reorder(Model, RMSE, .fun = median),
        y = RMSE,
        fill = Model
      )
    ) +
    ggplot2::geom_boxplot(
      width = 0.65,
      outlier.shape = NA,
      alpha = 0.8
    ) +
    ggplot2::geom_jitter(
      width = 0.11,
      alpha = 0.65,
      size = 1.8
    ) +
    ggplot2::scale_fill_manual(
      values = rep(
        c(
          palette_journal[["grey"]],
          palette_journal[["blue"]],
          palette_journal[["teal"]],
          palette_journal[["gold"]],
          palette_journal[["vermillion"]],
          palette_journal[["purple"]]
        ),
        length.out = length(unique(regression_metrics_by_repeat$Model))
      )
    ) +
    ggplot2::labs(
      title = "Repeated outer-cross-validation performance",
      subtitle = paste0(
        OUTER_FOLDS,
        "-fold grouped CV x ",
        OUTER_REPEATS,
        " repeats; lower RMSE is better"
      ),
      x = NULL,
      y = "RMSE in ATQ percentage points"
    ) +
    theme_journal() +
    ggplot2::theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 30, hjust = 1)
    )

  best_regression_oof <- averaged_regression_predictions |>
    dplyr::filter(Model == best_regression_model)

  regression_observed_predicted_plot <- ggplot2::ggplot(
    best_regression_oof,
    ggplot2::aes(x = Prediction, y = Truth)
  ) +
    ggplot2::geom_abline(
      slope = 1,
      intercept = 0,
      color = palette_journal[["grey"]],
      linetype = "dashed",
      linewidth = 0.7
    ) +
    ggplot2::geom_point(
      ggplot2::aes(color = Prediction_SD_Across_Repeats),
      size = 2.5,
      alpha = 0.8
    ) +
    ggplot2::geom_smooth(
      method = "lm",
      se = TRUE,
      color = palette_journal[["navy"]],
      fill = palette_journal[["pale_blue"]],
      linewidth = 0.85
    ) +
    ggplot2::scale_color_gradient(
      low = palette_journal[["teal"]],
      high = palette_journal[["vermillion"]]
    ) +
    ggplot2::coord_equal(xlim = c(0, 100), ylim = c(0, 100)) +
    ggplot2::labs(
      title = paste(
        "Observed versus cross-validated predicted adherence:",
        best_regression_model
      ),
      subtitle = "Predictions are averaged across complete outer-CV repeats",
      x = "Cross-validated predicted ATQ score",
      y = "Observed ATQ score",
      color = "Prediction SD\nacross repeats"
    ) +
    theme_journal()

  save_journal_figure(
    regression_rmse_plot,
    file.path(section_paths[["07_ML_Regression"]], "Figures"),
    "Figure_ML_Regression_RMSE",
    width = 10,
    height = 6
  )
  save_journal_figure(
    regression_observed_predicted_plot,
    file.path(section_paths[["07_ML_Regression"]], "Figures"),
    "Figure_ML_Regression_Observed_vs_Predicted",
    width = 7.5,
    height = 6.5
  )

  if (
    "Mean_Absolute_SHAP" %in% names(regression_shap$importance) &&
      nrow(regression_shap$importance) > 0L
  ) {
    regression_shap_plot <- regression_shap$importance |>
      dplyr::slice_head(n = 15L) |>
      dplyr::mutate(
        Variable = forcats::fct_reorder(Variable, Mean_Absolute_SHAP)
      ) |>
      ggplot2::ggplot(
        ggplot2::aes(x = Mean_Absolute_SHAP, y = Variable)
      ) +
      ggplot2::geom_col(
        fill = palette_journal[["teal"]],
        width = 0.7
      ) +
      ggplot2::labs(
        title = paste(
          "Model-agnostic variable importance:",
          best_regression_model
        ),
        subtitle = "Mean absolute SHAP value from the final full-sample model",
        x = "Mean absolute SHAP value",
        y = NULL
      ) +
      theme_journal()

    save_journal_figure(
      regression_shap_plot,
      file.path(section_paths[["07_ML_Regression"]], "Figures"),
      "Figure_ML_Regression_SHAP",
      width = 8.5,
      height = 6
    )
  }
} else {
  regression_nested_results <- NULL
  log_message("ML regression was disabled.")
}

###### 16. NESTED-CV CLASSIFICATION OF VERY-GOOD ADHERENCE #####################

safe_divide <- function(numerator, denominator) {
  ifelse(denominator == 0, NA_real_, numerator / denominator)
}

classification_repeat_metrics <- function(
    truth,
    probability,
    threshold = CLASSIFICATION_THRESHOLD
) {
  truth <- factor(
    truth,
    levels = c("Very_Good", "Below_Very_Good")
  )
  truth_binary <- as.integer(truth == "Very_Good")
  probability <- pmin(pmax(probability, 1e-6), 1 - 1e-6)
  predicted_binary <- as.integer(probability >= threshold)

  true_positive <- sum(predicted_binary == 1 & truth_binary == 1)
  true_negative <- sum(predicted_binary == 0 & truth_binary == 0)
  false_positive <- sum(predicted_binary == 1 & truth_binary == 0)
  false_negative <- sum(predicted_binary == 0 & truth_binary == 1)

  tibble::tibble(
    ROC_AUC = yardstick::roc_auc_vec(
      truth,
      probability,
      event_level = "first"
    ),
    PR_AUC = yardstick::pr_auc_vec(
      truth,
      probability,
      event_level = "first"
    ),
    Brier_Score = mean((truth_binary - probability)^2),
    Log_Loss = -mean(
      truth_binary * log(probability) +
        (1 - truth_binary) * log(1 - probability)
    ),
    Accuracy = safe_divide(
      true_positive + true_negative,
      length(truth_binary)
    ),
    Sensitivity = safe_divide(
      true_positive,
      true_positive + false_negative
    ),
    Specificity = safe_divide(
      true_negative,
      true_negative + false_positive
    ),
    Balanced_Accuracy = mean(
      c(
        safe_divide(
          true_positive,
          true_positive + false_negative
        ),
        safe_divide(
          true_negative,
          true_negative + false_positive
        )
      ),
      na.rm = TRUE
    ),
    PPV = safe_divide(
      true_positive,
      true_positive + false_positive
    ),
    NPV = safe_divide(
      true_negative,
      true_negative + false_negative
    )
  )
}

if (ML_CLASSIFICATION_ENABLED) {
  log_message(
    "Starting grouped nested repeated CV for very-good adherence classification."
  )

  outer_classification_resamples <- make_grouped_resamples(
    ml_classification_data,
    "ATQ_High",
    "classification",
    folds = OUTER_FOLDS,
    repeats = OUTER_REPEATS,
    seed = SEED
  )

  classification_nested_results <- run_nested_cv(
    data_frame = ml_classification_data,
    outcome_name = "ATQ_High",
    candidate_list = classification_candidates,
    mode = "classification",
    outer_resamples = outer_classification_resamples
  )

  classification_predictions <- classification_nested_results$predictions
  classification_prediction_completeness <- check_prediction_completeness(
    classification_predictions,
    expected_records = nrow(ml_classification_data)
  )

  complete_classification_keys <- classification_prediction_completeness |>
    dplyr::filter(Complete) |>
    dplyr::select(Model, Repeat)

  classification_predictions_complete <- classification_predictions |>
    dplyr::inner_join(
      complete_classification_keys,
      by = c("Model", "Repeat")
    )

  classification_metrics_by_repeat <- classification_predictions_complete |>
    dplyr::group_by(Model, Repeat) |>
    dplyr::group_modify(
      function(data_subset, group_keys) {
        classification_repeat_metrics(
          truth = data_subset$Truth,
          probability = data_subset$Probability_Very_Good,
          threshold = CLASSIFICATION_THRESHOLD
        )
      }
    ) |>
    dplyr::ungroup()

  classification_performance_summary <- summarize_repeated_metrics(
    classification_metrics_by_repeat
  )

  classification_calibration <- classification_predictions_complete |>
    dplyr::group_by(Model, Repeat) |>
    dplyr::group_modify(
      function(data_subset, group_keys) {
        probability <- pmin(
          pmax(data_subset$Probability_Very_Good, 1e-6),
          1 - 1e-6
        )
        outcome_binary <- as.integer(data_subset$Truth == "Very_Good")
        linear_predictor <- stats::qlogis(probability)

        calibration_intercept_fit <- tryCatch(
          stats::glm(
            outcome_binary ~ offset(linear_predictor),
            family = stats::binomial()
          ),
          error = function(error_condition) NULL
        )
        calibration_slope_fit <- tryCatch(
          stats::glm(
            outcome_binary ~ linear_predictor,
            family = stats::binomial()
          ),
          error = function(error_condition) NULL
        )

        tibble::tibble(
          Calibration_Intercept = ifelse(
            is.null(calibration_intercept_fit),
            NA_real_,
            stats::coef(calibration_intercept_fit)[1]
          ),
          Calibration_Slope = ifelse(
            is.null(calibration_slope_fit),
            NA_real_,
            stats::coef(calibration_slope_fit)[2]
          )
        )
      }
    ) |>
    dplyr::ungroup()

  best_classification_model <- classification_performance_summary |>
    dplyr::filter(
      Metric == "ROC_AUC",
      Model != "Prevalence_Baseline",
      is.finite(Mean)
    ) |>
    dplyr::arrange(dplyr::desc(Mean)) |>
    dplyr::slice_head(n = 1L) |>
    dplyr::pull(Model)

  if (length(best_classification_model) == 0L) {
    stop("No classification candidate completed a full outer-CV repeat.")
  }

  log_message(
    "Best classification model by mean outer-CV ROC AUC: ",
    best_classification_model,
    "."
  )

  final_classification_fit <- tryCatch(
    fit_final_candidate(
      data_frame = ml_classification_data,
      outcome_name = "ATQ_High",
      candidate_name = best_classification_model,
      candidate = classification_candidates[[best_classification_model]],
      mode = "classification"
    ),
    error = function(error_condition) {
      log_message(
        "Final fit for ",
        best_classification_model,
        " failed: ",
        conditionMessage(error_condition),
        ". Falling back to Logistic_Regression."
      )
      NULL
    }
  )
  if (is.null(final_classification_fit)) {
    best_classification_model <- "Logistic_Regression"
    final_classification_fit <- fit_final_candidate(
      data_frame = ml_classification_data,
      outcome_name = "ATQ_High",
      candidate_name = best_classification_model,
      candidate = classification_candidates[[best_classification_model]],
      mode = "classification"
    )
  }

  if (SAVE_FITTED_MODELS) {
    saveRDS(
      final_classification_fit$fit,
      file = file.path(
        section_paths[["08_ML_Classification"]],
        "Objects",
        "Final_Classification_Workflow.rds"
      )
    )
  }

  if (SHAP_ENABLED) {
    classification_shap <- tryCatch(
      calculate_shap_importance(
        fitted_workflow = final_classification_fit$fit,
        data_frame = ml_classification_data,
        outcome_name = "ATQ_High",
        mode = "classification"
      ),
      error = function(error_condition) {
        log_message(
          "Classification SHAP analysis failed: ",
          conditionMessage(error_condition)
        )
        list(
          importance = tibble::tibble(
            Message = conditionMessage(error_condition)
          ),
          shap_values = NULL
        )
      }
    )
  } else {
    classification_shap <- list(
      importance = tibble::tibble(Message = "SHAP analysis was disabled."),
      shap_values = NULL
    )
  }

  averaged_classification_predictions <- classification_predictions_complete |>
    dplyr::group_by(Model, Record_ID) |>
    dplyr::summarise(
      Truth = factor(
        dplyr::first(as.character(Truth)),
        levels = c("Very_Good", "Below_Very_Good")
      ),
      Probability_Very_Good = mean(Probability_Very_Good),
      Probability_SD_Across_Repeats = stats::sd(Probability_Very_Good),
      Repeated_Predictions = dplyr::n(),
      .groups = "drop"
    )

  classification_sheets <- list(
    Performance_Summary = classification_performance_summary,
    Metrics_by_Repeat = classification_metrics_by_repeat,
    Prediction_Completeness = classification_prediction_completeness,
    Calibration_by_Repeat = classification_calibration,
    Final_Hyperparameters = final_classification_fit$best_parameters,
    SHAP_Importance = classification_shap$importance,
    Tuning_Choices = classification_nested_results$tuning,
    Errors = classification_nested_results$errors
  )
  if (EXPORT_ROW_LEVEL_OUTPUTS) {
    classification_sheets$Averaged_OOF_Predictions <- averaged_classification_predictions
    classification_sheets$All_Outer_Predictions <- classification_predictions
  }

  write_excel_report(
    file_path = file.path(
      section_paths[["08_ML_Classification"]],
      "Tables",
      "08_ML_Classification_Nested_CV.xlsx"
    ),
    sheets = classification_sheets,
    report_title = "Nested Cross-Validated Classification of Very-Good Adherence",
    report_note = paste(
      "The positive class is ATQ_Total_Pct >= 75 and is exploratory.",
      "Threshold-dependent metrics use the prespecified probability cutoff",
      CLASSIFICATION_THRESHOLD,
      ". Decision-curve analysis is not included because no clinical action",
      "or consequence was prespecified."
    )
  )

  classification_auc_plot <- classification_metrics_by_repeat |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = forcats::fct_reorder(
          Model,
          ROC_AUC,
          .fun = median,
          .desc = TRUE
        ),
        y = ROC_AUC,
        fill = Model
      )
    ) +
    ggplot2::geom_hline(
      yintercept = 0.5,
      linetype = "dashed",
      color = palette_journal[["grey"]]
    ) +
    ggplot2::geom_boxplot(
      width = 0.65,
      outlier.shape = NA,
      alpha = 0.8
    ) +
    ggplot2::geom_jitter(
      width = 0.11,
      alpha = 0.65,
      size = 1.8
    ) +
    ggplot2::scale_fill_manual(
      values = rep(
        c(
          palette_journal[["grey"]],
          palette_journal[["blue"]],
          palette_journal[["teal"]],
          palette_journal[["gold"]],
          palette_journal[["vermillion"]],
          palette_journal[["purple"]]
        ),
        length.out = length(unique(classification_metrics_by_repeat$Model))
      )
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, 0.1)
    ) +
    ggplot2::labs(
      title = "Repeated outer-cross-validation discrimination",
      subtitle = paste0(
        OUTER_FOLDS,
        "-fold grouped CV x ",
        OUTER_REPEATS,
        " repeats"
      ),
      x = NULL,
      y = "ROC AUC"
    ) +
    theme_journal() +
    ggplot2::theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 30, hjust = 1)
    )

  roc_curve_data <- averaged_classification_predictions |>
    dplyr::group_by(Model) |>
    yardstick::roc_curve(
      truth = Truth,
      Probability_Very_Good,
      event_level = "first"
    ) |>
    dplyr::ungroup()

  roc_curve_plot <- ggplot2::ggplot(
    roc_curve_data,
    ggplot2::aes(
      x = 1 - specificity,
      y = sensitivity,
      color = Model
    )
  ) +
    ggplot2::geom_abline(
      slope = 1,
      intercept = 0,
      linetype = "dashed",
      color = palette_journal[["grey"]]
    ) +
    ggplot2::geom_path(linewidth = 0.9) +
    ggplot2::coord_equal() +
    ggplot2::scale_x_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, 0.2)
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, 0.2)
    ) +
    ggplot2::labs(
      title = "Cross-validated ROC curves",
      subtitle = "Probabilities averaged across complete outer-CV repeats",
      x = "1 - Specificity",
      y = "Sensitivity",
      color = "Model"
    ) +
    theme_journal()

  best_classification_oof <- averaged_classification_predictions |>
    dplyr::filter(Model == best_classification_model)

  calibration_plot_data <- best_classification_oof |>
    dplyr::mutate(
      Risk_Group = dplyr::ntile(Probability_Very_Good, 10L),
      Outcome_Binary = as.integer(Truth == "Very_Good")
    ) |>
    dplyr::group_by(Risk_Group) |>
    dplyr::summarise(
      N = dplyr::n(),
      Mean_Predicted = mean(Probability_Very_Good),
      Observed_Proportion = mean(Outcome_Binary),
      .groups = "drop"
    )

  calibration_plot <- ggplot2::ggplot(
    calibration_plot_data,
    ggplot2::aes(
      x = Mean_Predicted,
      y = Observed_Proportion
    )
  ) +
    ggplot2::geom_abline(
      slope = 1,
      intercept = 0,
      linetype = "dashed",
      color = palette_journal[["grey"]]
    ) +
    ggplot2::geom_line(
      color = palette_journal[["navy"]],
      linewidth = 0.85
    ) +
    ggplot2::geom_point(
      ggplot2::aes(size = N),
      color = palette_journal[["teal"]],
      alpha = 0.85
    ) +
    ggplot2::coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
    ggplot2::labs(
      title = paste(
        "Cross-validated calibration:",
        best_classification_model
      ),
      subtitle = "Observed and predicted very-good adherence by probability decile",
      x = "Mean predicted probability",
      y = "Observed proportion",
      size = "Participants"
    ) +
    theme_journal()

  save_journal_figure(
    classification_auc_plot,
    file.path(section_paths[["08_ML_Classification"]], "Figures"),
    "Figure_ML_Classification_ROC_AUC",
    width = 10,
    height = 6
  )
  save_journal_figure(
    roc_curve_plot,
    file.path(section_paths[["08_ML_Classification"]], "Figures"),
    "Figure_ML_Classification_ROC_Curves",
    width = 7.8,
    height = 6.5
  )
  save_journal_figure(
    calibration_plot,
    file.path(section_paths[["08_ML_Classification"]], "Figures"),
    "Figure_ML_Classification_Calibration",
    width = 7.5,
    height = 6.2
  )

  if (
    "Mean_Absolute_SHAP" %in% names(classification_shap$importance) &&
      nrow(classification_shap$importance) > 0L
  ) {
    classification_shap_plot <- classification_shap$importance |>
      dplyr::slice_head(n = 15L) |>
      dplyr::mutate(
        Variable = forcats::fct_reorder(Variable, Mean_Absolute_SHAP)
      ) |>
      ggplot2::ggplot(
        ggplot2::aes(x = Mean_Absolute_SHAP, y = Variable)
      ) +
      ggplot2::geom_col(
        fill = palette_journal[["purple"]],
        width = 0.7
      ) +
      ggplot2::labs(
        title = paste(
          "Model-agnostic variable importance:",
          best_classification_model
        ),
        subtitle = "Mean absolute SHAP value from the final full-sample model",
        x = "Mean absolute SHAP value",
        y = NULL
      ) +
      theme_journal()

    save_journal_figure(
      classification_shap_plot,
      file.path(section_paths[["08_ML_Classification"]], "Figures"),
      "Figure_ML_Classification_SHAP",
      width = 8.5,
      height = 6
    )
  }
} else {
  classification_nested_results <- NULL
  log_message("ML classification was disabled.")
}

###### 17. REPRODUCIBILITY RECORD AND COMPLETION ###############################

if (!is.null(parallel_cluster)) {
  parallel::stopCluster(parallel_cluster)
  foreach::registerDoSEQ()
  log_message("Parallel workers stopped.")
}

package_version_table <- tibble::tibble(
  Package = required_packages,
  Version = purrr::map_chr(
    required_packages,
    function(package_name) {
      as.character(utils::packageVersion(package_name))
    }
  )
)

writeLines(
  capture.output(utils::sessionInfo()),
  con = file.path(
    section_paths[["00_Run_Documentation"]],
    "R_Session_Information.txt"
  )
)

readme_text <- c(
  "LIVER TRANSPLANT ADHERENCE ANALYSIS",
  "",
  paste0("Run completed: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
  paste0("Input file: ", basename(DATA_FILE)),
  paste0("Input SHA-256: ", dataset_sha256),
  paste0("Records: ", nrow(data_analysis)),
  "",
  "The primary outcome is the continuous ATQ total percentage score.",
  "Potential duplicate clusters are kept together during resampling.",
  "Machine-learning analyses are exploratory and internally validated only.",
  "Participant-level data, row-level predictions, and fitted model objects",
  "must not be included in a public repository."
)

writeLines(
  readme_text,
  con = file.path(OUTPUT_ROOT, "README_ANALYSIS_OUTPUT.txt")
)

output_files_generated <- fs::dir_ls(
  OUTPUT_ROOT,
  recurse = TRUE,
  type = "file",
  fail = FALSE
)

output_inventory <- tibble::tibble(
  File = as.character(output_files_generated)
) |>
  dplyr::mutate(
    Relative_Path = fs::path_rel(File, start = OUTPUT_ROOT),
    Extension = tools::file_ext(File),
    Size_KB = file.info(File)$size / 1024
  ) |>
  dplyr::select(Relative_Path, Extension, Size_KB) |>
  dplyr::bind_rows(
    tibble::tibble(
      Relative_Path = file.path(
        "00_Run_Documentation",
        "Tables",
        "00_Reproducibility_Record.xlsx"
      ),
      Extension = "xlsx",
      Size_KB = NA_real_
    )
  ) |>
  dplyr::distinct(Relative_Path, .keep_all = TRUE) |>
  dplyr::arrange(Relative_Path)

final_run_summary <- tibble::tibble(
  Item = c(
    "Run completed",
    "Input records",
    "Adjusted-model complete cases",
    "Potential duplicate records",
    "Records with incomplete NEO-FFI data",
    "Primary outcome",
    "Primary personality tests",
    "Secondary personality-by-domain tests",
    "ML regression",
    "ML classification",
    "Row-level outputs exported",
    "Fitted models saved"
  ),
  Result = c(
    format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    as.character(nrow(data_analysis)),
    as.character(nrow(complete_model_data)),
    as.character(sum(data_analysis$Potential_Duplicate_Flag == 1)),
    as.character(sum(data_analysis$NEO_Missing_Items > 0)),
    "ATQ_Total_Pct",
    "5 NEO domains versus ATQ total",
    "5 NEO domains x 8 ATQ outcomes = 40 tests",
    ifelse(ML_REGRESSION_ENABLED, "Completed", "Disabled"),
    ifelse(ML_CLASSIFICATION_ENABLED, "Completed", "Disabled"),
    as.character(EXPORT_ROW_LEVEL_OUTPUTS),
    as.character(SAVE_FITTED_MODELS)
  )
)

write_excel_report(
  file_path = file.path(
    section_paths[["00_Run_Documentation"]],
    "Tables",
    "00_Reproducibility_Record.xlsx"
  ),
  sheets = list(
    Final_Run_Summary = final_run_summary,
    Package_Versions = package_version_table,
    Output_Inventory = output_inventory
  ),
  report_title = "Reproducibility Record",
  report_note = paste(
    "Package versions and session information should be retained with the",
    "repository release. A renv lockfile should be generated from the same",
    "environment used for the final analysis run."
  )
)

log_message(
  "Analysis completed. Generated ",
  nrow(output_inventory),
  " output files."
)

cat(
  "\n",
  paste(rep("=", 78), collapse = ""),
  "\nANALYSIS COMPLETED\n",
  "Output directory: ",
  OUTPUT_ROOT,
  "\n",
  paste(rep("=", 78), collapse = ""),
  "\n",
  sep = ""
)
