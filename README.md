# Assessment flexibility in the measurement of SES

## Overview
This project aims at assess the flexibility in measureing SES in cognitive neuroscience. See our [PPT for NeuroMatch 2.0](https://osf.io/gcxs6/) for more details about this project.

## Reproducing SES scores using open dataset
We will use data from [China Family Panel Study, CFPS](https://opendata.pku.edu.cn/dataverse/CFPS?language=en) and [Panel Study of Income Dynamics (PSID) ](https://psidonline.isr.umich.edu/) to reproduce the SES scores following the ways of calculating SES that we extracted from the literature. 

### Data preparation
* CFPS: we downloaded the 2020 data from the CFPS and then extracted data using `data_extraction_CFPS.R`. 
  Input:
  Output: 
* PSID: we selected relevant variables from PSID and extracted data using `data_extraction_PSID.R`. 
  Input:
  Output:

### SES scores reproduction
* We used `SES_scores_calculation.R` to calculate the SES scores (currently with 11 ways). Output: 

### Assess the flexibility and variability
* `Correlation_analyses.R`

#### Correlation between SES scores

#### Omega

### Assessing the variability in effect size resulted from variability in SES measurement