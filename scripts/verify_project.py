#!/usr/bin/env python3
"""Fail fast if private workbook data or a local-workbook fallback enters the project."""

from pathlib import Path
import re
import sys

ROOT = Path(__file__).resolve().parents[1]
APP = ROOT / "app" / "app.R"
WORKBOOK_EXTENSIONS = {".xlsx", ".xls", ".xlsm", ".xlsb"}

problems: list[str] = []

for path in ROOT.rglob("*"):
    if path.is_file() and path.suffix.lower() in WORKBOOK_EXTENSIONS:
        problems.append(f"Workbook must not be bundled: {path.relative_to(ROOT)}")

if not APP.exists():
    problems.append("Missing app/app.R")
else:
    source = APP.read_text(encoding="utf-8")
    required_fragments = [
        'fileInput("model_file"',
        "input$model_file$datapath",
        "input$model_file$name",
        "read_xlsx(file_path",
        "read_xls(file_path",
        "Choose an Excel workbook before loading.",
    ]
    for fragment in required_fragments:
        if fragment not in source:
            problems.append(f"Missing upload-only implementation fragment: {fragment}")

    forbidden_patterns = {
        r'default_file_path\s*<-': "Local workbook fallback is present",
        r'OnNetDC_Modeling.*\.xlsx': "Legacy workbook filename is present",
        r'(?i)xgboost::|library\s*\(\s*xgboost\s*\)': "Native XGBoost dependency is not browser-safe",
    }
    for pattern, message in forbidden_patterns.items():
        if re.search(pattern, source):
            problems.append(message)

if problems:
    print("Project verification failed:")
    for problem in problems:
        print(f"- {problem}")
    sys.exit(1)

print("Project verification passed: upload-only workbook flow; no Excel workbook bundled.")
