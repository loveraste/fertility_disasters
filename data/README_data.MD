# Data Sources and Construction

This project uses country-year data on fertility and exposure to natural
disasters. Raw data originate from third-party providers and are not
redistributed in this repository due to licensing restrictions. This
document provides precise instructions to obtain the original datasets
and reconstruct the analytical panel.

------------------------------------------------------------------------

## 1. United Nations Population Division Data

Fertility rates and population data are obtained from the United Nations
World Population Prospects 2024 (WPP 2024).

### Access

1.  Website: <https://population.un.org/wpp/>

2.  Click on **“Download data files”**.

3.  In the section **“Demographic Indicators”**, download the file
    labeled: **“Compact (most used: estimates and medium projections)
    (XLSX)”** This option downloads a single file containing the most
    commonly used demographic indicators, including total fertility
    rates and population measures.

### Required File

The downloaded file should be:
WPP2024_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT.xlsx

### File Placement

Save the downloaded file as:
data/raw/un/WPP2024_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT.xlsx

## 2. EM-DAT: Natural Disasters Database

Data on natural disasters are obtained from the Emergency Events
Database (EM-DAT), maintained by the Centre for Research on the
Epidemiology of Disasters (CRED).

### Access

1.  Register for access at: <https://www.emdat.be/>
2.  Log in to the EM-DAT data extraction interface.

### Required Selection Criteria

When downloading the data, users must apply **exactly** the following
filters:

-   **Classification**:
    -   Select: `Natural`
-   **Geographic Coverage**:
    -   Select **all continents**:
        -   Africa
        -   Americas
        -   Asia
        -   Europe
        -   Oceania
-   **Time Period**:
    -   From: `1950`
    -   To: `2023`
    -   Activate the option **“Include historical events (pre-2000)”**
-   Download data.

### File Placement

Save the downloaded file as: data/raw/emdat_natural_1950_2023.xlsx

## 3. World Bank Income Classification

Country income groups are obtained from the World Bank’s official income
classification, as reported in the World Development Indicators (WDI).
The file is merged to the country-level panel using ISO3 codes and is
used to construct income-group heterogeneity analyses.

**Access**\
Website:
<https://datatopics.worldbank.org/world-development-indicators/the-world-by-income-and-region.html>

On the page *“The world by income and region”*, click on\
**“Download the current classification by income in XLS format.”**

**Required file**\
Save the downloaded file as: data/raw/CLASS.xlsx

## 4. Data Processing

All raw data are transformed using the scripts provided in the code/
directory:

`01_clean_un_data.R` Cleans and harmonizes UN fertility and population
data.

`02_clean_emdat_data.R`Processes EM-DAT disaster data and constructs
country-year exposure measures.

`03_merge_un_emdat.R`Merges UN and EM-DAT data into the final analytical
panel.

All variable transformations, standardizations, and normalizations used
in the paper are fully documented in the corresponding scripts.
