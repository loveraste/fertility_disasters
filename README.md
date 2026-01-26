# README for “Global fertility responses to climate-related hazards depend on population disruption, lethality, and hazard type”

### Leonardo Bonilla Mejía, Alejandro López-Feldman, Ana María Tribín Uribe, Stefany López Vera

## Overview

This repository contains the full replication package for the paper “Global fertility responses to climate-related hazards depend on population disruption, lethality, and hazard type”. The package reproduces all data construction steps, descriptive statistics, event-study figures, difference-in-differences estimations, and robustness checks reported in the main paper and Online Appendix. All results are generated using R.

All scripts are located in the `scripts/` directory and are designed to be executed sequentially. A master script (`scripts/00_run_all.R`) executes the full replication pipeline. A full replication run takes approximately one hour, depending on hardware specifications.

## Repository Structure

``` text
├── scripts/
│   ├── 00_run_all.R            # Master script to run full replication
│   ├── 01_clean_un_data.R      # Clean UN WPP fertility and population data
│   ├── 02_clean_emdat_data.R    # Process EM-DAT disaster data
│   ├── 03_merge_un_emdat.R      # Merge fertility and disaster datasets
│   ├── 04_descriptives.R        # Descriptive statistics and figures
│   ├── 05_estimations.R         # Difference-in-differences estimations
│   ├── 06_event_study_plots.R   # Event-study figures
│   └── 07_robustness_check.R    # Robustness analyses
├── data/
│   ├── raw/                    # Raw data
│   ├── processed/              # Intermediate and final datasets (.rds)
├── outputs/
│   ├── figures/                # Figures (main and appendix)
│   ├── tables/                 # LaTeX tables
│   └── estimates/              # Saved estimation objects
└── README.md                   # Project overview and instructions
```

## Data Availability and Provenance Statements

Processed datasets required to reproduce all results are provided in this repository. All raw data originate from publicly accessible third-party providers and are subject to their respective licensing terms. Raw data can be obtained from the original sources following the instructions below, allowing full reconstruction of the analytical panel from scratch.

### Summary of Availability

-   [x] Processed data are publicly available.
-   [x] Raw data can be obtained from original public sources.
-   [ ] Some data cannot be made publicly available.
-   [ ] No data can be made publicly available

### Data Sources

| Data.Name | Data.Files | Location | Provided | Citation |
|----|----|----|----|----|
| “United Nations World Population Prospects 2024” | WPP2024_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT.xlsx | `data/raw/` | TRUE | United Nations (2024) |
| “EM-DAT Natural Disasters Database. CRED” | emdat_natural_1950_2023.xlsx | `data/raw/` | FALSE | Guha-Sapir et al. (2024) |
| “World Bank Income Classification” | CLASS.xlsx | `data/raw/` | TRUE | World Bank (2024) |

### Details on each Data Source

#### United Nations World Population Prospects 2024

Fertility rates and population data are obtained from the United Nations World Population Prospects 2024 (WPP 2024).

Access:

1.  Website: <https://population.un.org/wpp/>

2.  Click on “Download data files”.

3.  Under “Demographic Indicators”, download: "Compact (most used: estimates and medium projections) (XLSX)".

Required file name: WPP2024_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT.xlsx

File placement: `data/raw/un/WPP2024_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT.xlsx`

#### EM-DAT Natural Disasters Database

Data on natural disasters are obtained from the Emergency Events Database (EM-DAT), maintained by the Centre for Research on the Epidemiology of Disasters (CRED).

Access:

1.  Register at <https://www.emdat.be/>

2.  Log in to the data extraction interface.

3.  Required filters:

    -   Classification: Natural

    -   Geographic coverage: All continents

    -   Time period: 1950–2023

    -   Activate “Include historical events (pre-2000)”

File placement: `data/raw/emdat_natural_1950_2023.xlsx`

#### World Bank Income Classification

Country income groups are obtained from the World Bank’s official income classification.

Access: <https://datatopics.worldbank.org/world-development-indicators/the-world-by-income-and-region.html>

Download: “Download the current classification by income in XLS format.”

File placement: `data/raw/CLASS.xlsx`

### Data Processing and Construction

Raw data are transformed into the final analytical panel using the scripts located in the `scripts/` directory:

-   `01_clean_un_data.R`: Cleans and harmonizes UN fertility and population data.
-   `02_clean_emdat_data.R`: Processes EM-DAT data and constructs disaster exposure measures.
-   `03_merge_un_emdat.R`: Merges fertility and disaster data into the final country-year panel.

All variable definitions, transformations, and standardizations used in the analysis are fully documented in the corresponding scripts.

## Computational requirements

### Software Requirements

-   **R**: version **4.5.1**\
    (Tested with R version 4.5.1, released 2025-06-13)

-   **Operating System**:\
    Tested on **Windows 11 x64**. The code is expected to be operating-system independent.

-   **R package management**:\
    All required R packages are loaded within scripts using `pacman::p_load()`. No manual package installation is required.

Key packages used in the analysis include:

-   DIDmultiplegtDYN (v2.1.2)
-   tidyverse (v2.0.0)
-   dplyr (v1.1.4)
-   tidyr (v1.3.1)
-   ggplot2 (v4.0.1)
-   readxl (v1.4.5)
-   janitor (v2.2.1)
-   kableExtra (v1.4.0)
-   here (v1.0.1)

All package dependencies are automatically handled by the scripts.

### Controlled Randomness

The analysis described in this replication package does not rely on pseudo-random number generation. All results are fully deterministic and reproducible.

### Memory, Runtime, Storage Requirements

#### Summary time to reproduce

Approximate time needed to reproduce the analyses:

-   [ ] \<10 minutes
-   [ ] 10-60 minutes
-   [x] 1-2 hours
-   [ ] 2-8 hours
-   [ ] 8-24 hours
-   [ ] 1-3 days
-   [ ] 3-14 days
-   [ ] \> 14 days

#### Summary of required storage space

Approximate storage space needed:

-   [ ] \< 25 MBytes

-   [x] 25 MB - 250 MB

-   [ ] 250 MB - 2 GB

-   [ ] 2 GB - 25 GB

-   [ ] 25 GB - 250 GB

-   [ ] \> 250 GB

-   [ ] Not feasible to run on a desktop machine, as described below.

#### Computational Details

The code was last run on a personal laptop with the following specifications:

-   Operating System: Windows 11 Home (64-bit), version 24H2
-   Processor: 13th Gen Intel® Core™ i7-13620H (2.40 GHz)
-   RAM: 16 GB
-   Graphics: Integrated Intel® UHD Graphics
-   Storage: Approximately 200 GB of free disk space available at runtime

## Instructions to Replicators

1.  Download all the necessary raw data following the instructions in the *Details on each Data Source* section

2.  Open the R project or set the working directory to the root of the repository.

3.  Run the full replication pipeline: `source("scripts/00_run_all.R")`

## List of tables and programs

The provided code reproduces:

-   [x] All numbers provided in text in the paper
-   [x] All tables and figures in the paper
-   [ ] Selected tables and figures in the paper, as explained and justified below.

| Figure/Table \# | Program                          | Line Number |
|-----------------|----------------------------------|-------------|
| Figure 1        | `scripts/04_descriptives.R`      | 96-111      |
| Figure 2        | `scripts/04_descriptives.R`      | 159-212     |
| Figure 3        | `scripts/06_event_study_plots.R` | 221-243     |
| Figure 4        | `scripts/06_event_study_plots.R` | 280-310     |
| Figure 5        | `scripts/06_event_study_plots.R` | 331-349     |
| Figure 6        | `scripts/06_event_study_plots.R` | 370-389     |
| Table A.1       | `scripts/04_descriptives.R`      | 274-288     |
| Figure A.1      | `scripts/07_robustness_check.R`  | 262-292     |
| Figure A.2      | `scripts/07_robustness_check.R`  | 351-369     |
| Figure A.3      | `scripts/07_robustness_check.R`  | 314-332     |
| Figure A.4      | `scripts/07_robustness_check.R`  | 386-404     |
| Figure A.5      | `scripts/07_robustness_check.R`  | 465-484     |
| Figure A.6      | `scripts/07_robustness_check.R`  | 426-445     |
| Figure A.7      | `scripts/07_robustness_check.R`  | 503-522     |
| Figure A.8      | `scripts/07_robustness_check.R`  | 557-575     |
| Figure A.9      | `scripts/07_robustness_check.R`  | 650-664     |
| Figure A.10     | `scripts/07_robustness_check.R`  | 702-719     |

## References

United Nations, Department of Economic and Social Affairs, Population Division. (2024). *World Population Prospects 2024, Online Edition*. [Dataset].

Guha-Sapir, Debarati; Below, Regina; Hoyois, Philippe. (2024). *EM-DAT: The Emergency Events Database*. Université Catholique de Louvain, Brussels, Belgium. [Dataset].

World Bank. (2024). *World Bank Country and Lending Groups*. [Dataset]. Washington, DC: World Bank.

------------------------------------------------------------------------
