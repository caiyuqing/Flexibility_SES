# Assessment flexibility in the measurement of SES

## Overview
This project aims at assessing the flexibility in measureing SES in cognitive neuroscience. See our [PPT for NeuroMatch 2.0](https://osf.io/gcxs6/) for more details about this project.

## Reproducing SES scores using open dataset
We will use data from [China Family Panel Study, CFPS](https://opendata.pku.edu.cn/dataverse/CFPS?language=en) and [Panel Study of Income Dynamics (PSID) ](https://psidonline.isr.umich.edu/) to reproduce the SES scores following the ways of calculating SES that we extracted from the literature. 

### Data preparation
* CFPS: we downloaded the 2010 data from the CFPS and then extracted data using `data_extraction_CFPS.R`. 
  * Input: CFPS2010’s children dataset (`2010Children.csv`); adults dataset (`2010adult.csv`); familydataset (`2010family.csv`); community dataset (`2010community.csv`)
  
  * Output: a combined Rdata (`CFPS2010.RData`) containing selected variables from `2010Children.csv` (`df.children`); `2010adult.csv` (`df.individual`); `2010family.csv` (`df.family`); and `2010community.csv` (`df.community`)

* PSID: we selected relevant variables from PSID using the provided varible selection system in the website of PSID; checking the selected varibles and also age and gender distribution of participants using `data_extraction_PSID.R`. 

### SES scores reproduction
* We used `SES_scores_calculation.R` to calculate the SES scores (currently with 11 ways). 

* Output: two RData files containing all the SES measurements calculated of each participants (`SES_CFPS.RData`; `SES_PSID.RData`)

### Assess the flexibility and variability
We used `Correlation_analyses.R` to quantify the variability caused by the flexibility. To visualize the flexibility, we used `Alluvial_Plot.r`, which may need further revision.

![Fig. flexibility of SES measures](./Alluvial.png)

##### Consistency between SES scores
* All the SES scores are positively correlated but the strength vary from 0.17 to 0.99 (CFPS); from 0.43 to 0.95 (PSID).

* We used Intra-Class Correlation (Two-way random effect model for single measurement agreement) to quantify the consistency between different SES scores. In this way, we view each way of calculating SES as the rating of a "rater", and ICC can estimate how consistent are these "raters". 

  * CFPS: 0.588, 95%CI [0.572, 0.604]

  * PSID: 0.623, 95%CI [0.608, 0.638]. 

##### Variability in effect size caused by SES measurement flexibility

We used a few surrogate target variables to estimate the variability brought by flexibility in measuring SES.

* CFPS: The correlation coefficients between different SES scores and depression scores varied from -0.01 [-0.0415, 0.0196] to 0.03312 [0.0049, 0.0613]; between SES scores and cognitive ability varied from 0.09 [0.0603, 0.1209] to 0.27 [0.2375, 0.3023] .

* PSID: The correlation between different SES scores and depression score varied from 0.11 [0.0548, 0.1437] to 0.18 [0.1395, 0.2102]; between SES scores and life satification varied from 0.04 [-0.0122, 0.0767] to 0.19 [0.1594, 0.2295].
