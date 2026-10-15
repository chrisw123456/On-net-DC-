#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
project_dir <- if (length(file_arg)) {
  dirname(normalizePath(sub("^--file=", "", file_arg[[1]])))
} else {
  normalizePath(getwd())
}

app_dir <- file.path(project_dir, "app")
site_dir <- file.path(project_dir, "site")

if (!requireNamespace("shinylive", quietly = TRUE)) {
  stop("Install the shinylive package before building: install.packages('shinylive')")
}
if (!file.exists(file.path(app_dir, "app.R"))) {
  stop("Expected app/app.R under ", project_dir)
}

# Privacy/deployment guard: workbook data must never enter the project or export.
workbooks <- list.files(
  project_dir,
  pattern = "\\.(xlsx|xls|xlsm|xlsb)$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)
if (length(workbooks)) {
  stop(
    "Workbook files are not allowed in the Shinylive project. Remove: ",
    paste(workbooks, collapse = ", ")
  )
}

unlink(site_dir, recursive = TRUE, force = TRUE)
shinylive::export(app_dir, site_dir, quiet = FALSE)
file.create(file.path(site_dir, ".nojekyll"))

exported_workbooks <- list.files(
  site_dir,
  pattern = "\\.(xlsx|xls|xlsm|xlsb)$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)
if (length(exported_workbooks)) {
  stop("Build verification failed: the static site contains an Excel workbook.")
}

message("Shinylive site created at: ", normalizePath(site_dir))
