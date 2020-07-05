# Assessment flexibility in the measurement of SES

## Overview
This project aims at assess the flexibility in measureing SES in cognitive neuroscience. See our [PPT for NeuroMatch 2.0](https://osf.io/gcxs6/) for more details about this project.

## Reproducing SES scores using open dataset
We will use data from [China Family Panel Study, CFPS](https://opendata.pku.edu.cn/dataverse/CFPS?language=en) and [Panel Study of Income Dynamics (PSID) ](https://psidonline.isr.umich.edu/) to reproduce the SES scores following the ways of calculating SES that we extracted from the literature. 

### Data preparation
* CFPS: we downloaded the 2020 data from the CFPS and then extracted data using `data_extraction_CFPS.R`. 
  Input: CFPS2010’s children dataset (2010Children.csv); adults dataset (2010adult.csv); familydataset (2010family.csv); community dataset (2010community.csv)
  Output: a combined Rdata ('CFPS2010.RData') containing selected variables from 2010Children.csv (df.children); 2010adult.csv (df.individual); 2010family.csv (df.family); and 2010community.csv (df.community)
* PSID: we selected relevant variables from PSID using the provided varible selection system in the website of PSID; checking the selected varibles and also age and gender distribution of participants using `data_extraction_PSID.R`. 

### SES scores reproduction
* We used `SES_scores_calculation.R` to calculate the SES scores (currently with 11 ways). Output: two RData files containing all the SES measurements calculated of each participants (SES_CFPS.RData; SES_PSID.RData)

### Assess the flexibility and variability
* `Correlation_analyses.R`

#### Correlation between SES scores
### Assessing the variability in effect size resulted from variability in SES measurement
* All the SES scores are positively correlated but the strength vary from R = 0.17 to R = 0.99 (CFPS); R = 0.43 to R = 0.95

#### Omega
* CFPS: The McDonald’s omega for all eleven SES scores were 0.706 (hieratical) and 0.922 (total)

* PSID: The McDonald's omega all eight SES scores were (hieratical) and (total). 

