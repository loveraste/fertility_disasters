# Replication Package

## *Too Hot to Handle? Climate-Related Disasters and Global Fertility Rates*

------------------------------------------------------------------------

## Overview

This repository contains the full replication package for the working paper:

**Too Hot to Handle? Climate-Related Disasters and Global Fertility Rates**

The code reproduces **all figures and tables in the main paper and in the Online Appendix**, using publicly available demographic and disaster data combined into a global country-year panel spanning 1950–2023.

All results are reproduced **bit by bit**, conditional on access to the original raw data sources.

------------------------------------------------------------------------

## Authors and Affiliations

-   **Leonardo Bonilla Mejía**\
    Banco de la República, Colombia

-   **Alejandro Lopez-Feldman**\
    University of Gothenburg, Sweden\
    Universidad Iberoamericana, Mexico

-   **Ana Maria Tribin Uribe**\
    The World Bank, Washington D.C.

-   **Stefany Lopez Vera**\
    Universidad EAFIT, Colombia

------------------------------------------------------------------------

## Status of the Paper

-   **Working Paper**

------------------------------------------------------------------------

## Purpose of the Replication Package

This repository reproduces:

-   All **data construction steps**
-   All **descriptive statistics**
-   All **event-study figures**
-   All **difference-in-differences estimations**
-   All **robustness checks**
-   All **tables and figures** reported in:
    -   The main paper
    -   The Online Appendix

The goal is full computational reproducibility of the empirical results.

------------------------------------------------------------------------

## Computational Requirements

-   **R version**: Tested on **R 4.5.1**\
    (The code makes use of modern R syntax; older versions may not be compatible.)

-   **Operating system**:\
    Tested on **Windows**; expected to be OS-independent.

-   **Package management**:\
    Packages are loaded within scripts using `pacman::p_load()`.

-   **Runtime**:\
    A full replication run takes approximately **30 minutes**, depending on machine specifications.

------------------------------------------------------------------------

## Repository Structure

data/\
├── raw/ Raw data downloaded from third-party providers\
├── processed/ Intermediate and final processed datasets (.rds)\
├── README_data.md Detailed instructions to obtain raw data

outputs/\
├── figures/ All figures (main + appendix)\
├── tables/ LaTeX tables\
├── estimates/ Saved estimation objects and tidy outputs

00_run_all.R \
01_clean_un_data.R\
02_clean_emdat_data.R\
03_merge_un_emdat.R\
04_descriptives.R\
05_estimations.R\
06_event_study_plots.R\
07_robustness_check.R

All outputs are **automatically overwritten** when the replication pipeline is re-run.

------------------------------------------------------------------------

## Data Availability

Raw data are obtained from **third-party providers** and are **not redistributed** in this repository.

Detailed instructions to download and place all required raw data files are provided in: `data/README_data.md`

This file documents: - Data sources - Download links - Required file names - Expected folder locations

All processed datasets included in the pipeline are fully reproducible from the raw data.

------------------------------------------------------------------------

## How to Run the Replication

1.  Download all required raw data following the instructions in\
    `data/README_data.md`.

2.  Open the R project (or set the working directory to the repository root).

3.  Run the full replication pipeline with: `source("00_run_all.R")`

## Outputs

After successful execution, the following outputs are generated:

-   **Processed datasets** (`.rds`) in `data/processed/`

-    **Figures** (`.png`) in `outputs/figures/`

-    **Tables** (`.tex`) in `outputs/tables/`

-    **Estimation objects and tidy results** in `outputs/estimates/`

File naming conventions follow the structure of the paper and appendix.
