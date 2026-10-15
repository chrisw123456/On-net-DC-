# On_net_Modeling_App_v8 — Shinylive deployment

This project converts the R Shiny app into a static **Shinylive** site. R runs in the visitor's browser through WebAssembly; no Shiny server is required.

## Workbook handling

The Excel workbook is deliberately **not included** in this repository or the generated `site/` directory.

At runtime, the user must:

1. Open the published site over HTTPS (or use localhost for local testing).
2. Choose an `.xlsx` or `.xls` workbook in **Modeling workbook**.
3. Enter the worksheet name (default: `main`).
4. Select **Load workbook**.
5. Enter the proposed-site inputs and select **Match proposed site**.

The app reads `input$model_file$datapath`, using the original uploaded filename only to select the correct Excel parser. The workbook remains in the browser session's temporary filesystem. Reloading or closing the page clears the session, so the workbook must be selected again.

## What changed for Shinylive

- Removed the legacy local-file fallback and made workbook upload mandatory.
- Added `.xlsx` / `.xls` validation and parser selection that works even when Shiny gives the temporary upload path no file extension.
- Removed the duplicated source blocks from the original file.
- Replaced the native XGBoost secondary model with a deterministic, browser-compatible ridge model implemented in base R. This preserves predicted-revenue, validation, ranking, and growth-scenario outputs without requiring a native package that may not be available as a WebAssembly binary.
- Added build-time checks that reject Excel workbooks anywhere in the project or exported site.
- Added a GitHub Pages workflow.

The ridge result is not numerically equivalent to the former XGBoost result. Comparable-facility matching, workbook filtering, benchmark calculations, 4MC calculations, and the uploaded-workbook workflow remain intact.

## Build locally

Install R 4.1 or newer, then run:

```r
install.packages(c(
  "shinylive", "shiny", "DT", "readxl", "dplyr", "tidyr",
  "readr", "tibble", "ggplot2", "scales", "httpuv"
))
```

From the project root:

```bash
python3 scripts/verify_project.py
Rscript build.R
```

Preview the generated static site with a local HTTP server; do not open `index.html` with `file://`:

```bash
Rscript -e 'httpuv::runStaticServer("site", port = 8000)'
```

Then browse to `http://localhost:8000`.

## Publish

### GitHub Pages

1. Create a repository and copy this project into it.
2. Push to the `main` branch.
3. In repository settings, configure **Pages → Source** as **GitHub Actions**.
4. The included workflow builds `site/` and deploys it.

### Other static hosts

Run `Rscript build.R`, then publish the **contents** of `site/` to any HTTPS static host.

## Privacy and deployment guardrails

- `.gitignore` excludes Excel workbooks and common data exports.
- `scripts/verify_project.py` fails if a workbook is found or if a local workbook fallback is reintroduced.
- `build.R` repeats the workbook check before and after export.
- Do not add a workbook to `app/`, `www/`, `site/`, or the repository history.
- Because processing occurs locally in the browser, each user's device supplies the CPU and memory required for workbook parsing and modeling.

## Project layout

```text
app/app.R                         Shinylive-compatible application
build.R                          Static-site export with workbook guards
DESCRIPTION                      R dependencies
scripts/verify_project.py        Source and privacy checks
.github/workflows/deploy-pages.yml
                                 Automated GitHub Pages deployment
```
