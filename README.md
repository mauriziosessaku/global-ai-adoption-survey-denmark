# Global Artificial Intelligence Adoption Survey — Perceptions of Public Sector Employees in Denmark

This repository contains the data, analysis code, and manuscript for a cross-sectional survey study of artificial intelligence (AI) use and attitudes among public-sector employees in Denmark. It accompanies the article:

> **Global Artificial Intelligence Adoption Survey: Perceptions of Public Sector Employees in Denmark.**
> Reem Saad, Khalid Saifuddin, Kenneth Græbild Thomsen, Maurizio Sessa.

The study is the Danish arm of the international *Global Artificial Intelligence Adoption Survey: Perceptions of Public Sector Employees*, developed by the University of Ljubljana, Faculty of Public Administration, in collaboration with partners in the AI SocLab Consortium.

## Study at a glance

- **Design:** Cross-sectional survey of Danish public-sector entities, conducted between 1 October 2025 and 8 February 2026.
- **Sampling frame:** 904 public-sector entities (1 central government, 5 regions, 98 municipalities, 800 public companies).
- **Respondents:** 57 respondents from 24 unique institutions (institutional coverage 2.7%); 54 were AI users.
- **Instrument:** Sociodemographic characteristics, AI exposure, and five-point Likert items across acceptance, performance, learning, ethics, organizational support, readiness, adaptability, workplace experience, and outcomes.
- **Analysis:** Composite construct scores, internal consistency (Cronbach's α), Spearman rank correlations between constructs, and representativeness benchmarking against Statistics Denmark registers (OBESK2 employment by subsector; OFF29 expenditure by COFOG function).
- **Software:** R 4.5.2.

## Repository structure

```
global-ai-adoption-survey-denmark/
├── README.md                     This file
├── LICENSE                       License for the code (MIT)
├── CITATION.cff                  How to cite this repository
├── .gitignore
├── data/
│   ├── Denmark_Dataset.xlsx       Raw survey responses (input to the pipeline)
│   └── Data_all_sections.RData    Pre-processed R workspace (intermediate object)
├── analysis/
│   └── global_ai_adoption_survey_denmark.R   Full analysis pipeline
├── manuscript/
│   └── Global_AI_Adoption_Survey_Denmark_Manuscript_20260716_RS_v6_0_KT.docx
├── docs/
│   └── codebook.md                Variable-to-item mapping (Qxx → question text)
└── output/
    ├── figures/                   Generated figures (PNG) — created when the script runs
    └── tables/                    Generated Table 1 (XLSX) — created when the script runs
```

## Data

`data/Denmark_Dataset.xlsx` holds the individual-level survey responses: 58 rows (respondents) × 167 columns. Column names follow the survey item codes (`Q1`–`Q33`, with `Qx_y_text` free-text follow-ups) plus a leading `USAGE` classifier (`Use`, `I don't use`, `I don't know`). The mapping from item code to question wording is in [`docs/codebook.md`](docs/codebook.md) and in the manuscript's Appendix B.

`data/Data_all_sections.RData` is the pre-processed workspace (the `Data_all_sections` object) produced by the data-preparation stage of the pipeline. It is included for convenience so that the downstream analysis and figures can be reproduced without re-running the full cleaning step.

> ⚠️ **Privacy note (please read before publishing this repository publicly).**
> The raw dataset contains fields that can be indirectly identifying: institution names (`Q2`), several free-text fields (`Q3_4_text`, `Q4_11_text`, `Q5_3_text`, `Q6_3_text`, `Q14k_text`), and open-ended responses (`Q33`, 35 free-text entries in Danish and English). The manuscript reports responses anonymously and suppresses cells with fewer than five respondents. Before making the raw file publicly available, consider releasing a **de-identified** version (e.g., organization names removed or coded, free-text fields dropped or reviewed, small cells suppressed) to match the anonymity commitment stated in the manuscript's ethics section. See *Data availability* below.

## Reproducing the analysis

### Requirements

- R ≥ 4.5 (tested with R 4.5.2).
- The following R packages (installed automatically on first run if missing):
  `readxl`, `table1`, `skimr`, `dplyr`, `gtsummary`, `ggplot2`, `stringr`, `tidyr`, `psych`, `openxlsx`, `tibble`, `forcats`, `ggh4x`, `patchwork`.

### Steps

1. Clone the repository and set the repository root as your working directory:
   ```r
   setwd("/path/to/global-ai-adoption-survey-denmark")
   ```
   (In RStudio, simply open the repository folder.)
2. Run the pipeline:
   ```r
   source("analysis/global_ai_adoption_survey_denmark.R")
   ```

The script reads from `data/`, creates the `output/` subfolders if needed, writes all figures to `output/figures/` (PNG, up to 1200 dpi) and Table 1 to `output/tables/`, and saves the intermediate workspace to `output/Data_all_sections.RData`.

> **Note on paths.** The original analysis was written with absolute, machine-specific output paths. For this release all input/output paths were made repository-relative; the analysis logic itself is unchanged. Interactive `View()` calls are commented out so the script runs non-interactively.

## Outputs

Running the pipeline reproduces the manuscript figures and Table 1, including:

- **Table 1** — sociodemographic and occupational characteristics.
- **Figure 1** — representativeness of the sample vs. the Danish public sector (level of government, sex, functional domain).
- **Figure 2** — AI tool types, frequency of use, and self-rated experience among users.
- **Figure 3** — composite construct scores and internal consistency (Cronbach's α).
- **Figure 4** — Spearman correlations between constructs.
- **Figure 5** and **Supplementary Figures S1–S19** — item-level diverging stacked bar charts.

## Key findings

- AI use was already embedded: 54/57 (94.7%) respondents were AI users; chatbots and AI assistants were used by 89.5%.
- Highest composite scores were for expected future use (4.5/5) and perceived performance impact (4.1/5).
- Lowest scores were for ethical considerations (2.8/5) and citizen involvement and accountability (2.8/5).
- Expected future use correlated most strongly with perceived performance impact (ρ = 0.71).
- Relative to the Danish public sector, the sample over-represented regional and national government and the health domain; the sex distribution matched the female-dominated workforce.

## Population reference data

Representativeness benchmarking uses publicly available Statistics Denmark tables:

- **OBESK2** — public full-time employees by subsector (Q4 2025).
- **OFF29** — general government expenditure by COFOG function (2025).

The expected counts derived from these tables are computed inside the analysis script.

## Data availability

The analysis code and (subject to the privacy note above) the data are provided in this repository. If a de-identified public dataset is preferred, replace `data/Denmark_Dataset.xlsx` with the de-identified version and update this section accordingly.

## Citation

If you use this repository, please cite the accompanying article and this repository. See [`CITATION.cff`](CITATION.cff).

## License

- **Code** (`analysis/`) is released under the MIT License — see [`LICENSE`](LICENSE).
- **Data** and the **manuscript** are shared for the purpose of reproducing and reviewing the study. Please contact the corresponding author for other uses.

## Contact

**Maurizio Sessa** — Department of Drug Design and Pharmacology, University of Copenhagen
Email: maurizio.sessa@sund.ku.dk
