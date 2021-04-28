# On the Validity of Arrest as a Proxy for Offense: Race and the Likelihood of Arrest for Violent Crimes


## Overview
This repo contains code to reproduce the analysis in the paper "On the Validity of Arrest as a Proxy for Offense: Race and the Likelihood of Arrest for Violent Crimes". In this work, we investigate racial disparities in arrests on incident-level data of violent offenses that occurred in the period 2007-2016 in 16 US states and were reported by law enforcement agencies through the National Incident Based Reporting System ([NIBRS](https://www.fbi.gov/services/cjis/ucr/nibrs)).


## Data processing

Before running the following scripts, run the following lines of code in your R console.

```
install.packages('renv')
renv::restore()
```

### Files

To prepare the data for the analysis, run:

* **acquire_data.R**: Download NIBRS, LEAIC, and LEOKA data from ICPSR.
* **clean_leaic.R**: Aggregate LEAIC data files keeping only information relevant to our analysis.
* **clean_leoka.R**: Aggregate LEOKA data files keeping only information relevant to our analysis.
* **clean_nibrs.R**: Extract information from individual NIBRS files.
* **create_jointdata.R**: Merge NIBRS and LEOKA data across multiple years. In this step, all incidents containing at least one violent offense are still included in the data (i.e., none of the exclusions mentioned in the paper applied other than the offense type).
* **create_offensedata.R**: Create separate datasets, one for each type of offense. Every observation (row) in the data corresponds to one incident. Only incidents with one Black/White victim, one Black/White offender, and that occurred in the 16 states considered are kept.
* **create_valuesimpdata.R**: Impute missing data. 

### Quickly process the data

To run everything, copy and paste the following code in the terminal.

```
Rscript process-data/acquire_data.R;
Rscript process-data/clean_leaic.R;
Rscript process-data/clean_leoka.R;
Rscript process-data/clean_nibrs.R;
Rscript process-data/create_jointdata.R;
Rscript process-data/create_offensedata.R;
Rscript process-data/create_valuesimpdata.R
```

## Analysis

### Data exclusions
As a reminder, the data that we consider in the main analysis presented in the paper correspond to incidents

1. with least one offense corresponding to murder/non-negligent manslaughter (UCR offenses code = 09A), forcible rape (11A), robbery (120), aggravated assault (13A), or simple assault (13B). 
2. that occurred between 2007 and 2016 and have been reported through the NIBRS in one of the following 16 states: Arkansas (AR), Colorado (CO), Delaware (DE), Idaho (ID), 
Iowa (IA), Kentucky (KY), Michigan (MI), Montana (MT), 
New Hampshire (NH), North Dakota (ND), South Carolina (SC), 
South Dakota (SD), Tennessee (TN), Vermont (VT), Virginia (VA), 
and West Virginia (WV).
3. that have only one offender and one victim.
4. for which both offender and victim were identified as being of race either Black or White. Hispanics are included. 

### Files 

To obtain the results presented in the paper, use the following files:

* **eda_alloffenses.Rmd**: Exploratory data analysis (EDA) of the data considered (i.e., restrictions of data apply).
* **eda_fulldata.Rmd**: EDA of _all_ incidents, not only those reported in the analysis.
* **eda_offense.Rmd**: EDA of the data considered. Each offenses is considered separately. Here we look at the variation in arrest rates across jurisdictions. 
* **regression_fit.R**: Regression and model diagnostics. The estimates of the coefficients are saved in the `regression-results`.
* **regression_resultsanalysis.Rmd**: Analyze the results of the regression models.

### Quickly reproduce the analysis

You can get the entire analysis very quickly by running the following commands from the terminal.

```
Rscript -e "rmarkdown::render('analysis/eda_fulldata.Rmd', output_dir = 'analysis/html-output')";
Rscript -e "rmarkdown::render('analysis/eda_offense.Rmd', output_dir = 'analysis/html-output', params = list(offense = '11A'), output_file = 'eda_offense_11A.html')";
Rscript -e "rmarkdown::render('analysis/eda_offense.Rmd', output_dir = 'analysis/html-output', params = list(offense = '120'), output_file = 'eda_offense_120.html')";
Rscript -e "rmarkdown::render('analysis/eda_offense.Rmd', output_dir = 'analysis/html-output', params = list(offense = '13A'), output_file = 'eda_offense_13A.html')";
Rscript -e "rmarkdown::render('analysis/eda_offense.Rmd', output_dir = 'analysis/html-output', params = list(offense = '13B'), output_file = 'eda_offense_13B.html')";
Rscript -e "rmarkdown::render('analysis/eda_alloffenses.Rmd', output_dir = 'analysis/html-output')";
Rscript regression-fit.R '09A' 10; Rscript regression-fit.R '11A' 10; Rscript regression-fit.R '120' 10; Rscript regression-fit.R '13A' 10; Rscript regression-fit.R '13B' 10
Rscript -e "rmarkdown::render('analysis/regression_resultsanalysis.Rmd', output_dir = 'analysis/html-output')"
```

