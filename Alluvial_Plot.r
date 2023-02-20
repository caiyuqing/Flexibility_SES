# Visualize the flexibility of SES using Easyalluvial

### clean the memory to avoid unnecessary errors:
rm(list = ls())

if (!require(easyalluvial)) {install.packages("easyalluvial",repos = "http://cran.us.r-project.org"); 
        require(easyalluvial)}
if (!require(parcats)) {install.packages("parcats",repos = "http://cran.us.r-project.org"); 
        require(parcats)}
if (!require(tidyverse)) {install.packages("tidyverse",repos = "http://cran.us.r-project.org"); 
        require(tidyverse)}

# install.packages("networkD3")
# library(networkD3)
# library(htmlwidgets)
# library(htmltools)

df <- read.csv('ses_data.csv') %>%
        dplyr::mutate(Uni_data = paste('data',Uni_data,sep='_'),
                      SES_Ver = paste('Index',SES_Ver,sep='_'))

df_p <- df %>% dplyr::select(-1) %>%
        dplyr::select(Nation, everything()) %>%
        dplyr::mutate_if(is.character,as.factor) %>%
        dplyr::rename(`Data source` =  Nation,
                      `Age range` = Dev_stage,
                      Dataset = Uni_data,
                      `SES measures` = SES_Ver,
                      `SES indicators`= SES_Types,
                      `Data types` = Data_type)

# grey flow but colored columns
p1 <- df_p %>%
        easyalluvial::alluvial_wide(.,
                      col_vector_flow = 'grey',
                      col_vector_value = palette_filter( greys = F),
                      stratum_label_size = 1, 
                      stratum_width = 1/3,
                      colorful_fill_variable_stratum = F) #%>%
        # add_marginal_histograms(df_p)

parcats::parcats(p1, marginal_histograms = FALSE, imp = TRUE, data_input = df_p, bundlecolors = FALSE)

# using the first column to color the flow
p2 <- df_p %>%
        easyalluvial::alluvial_wide(.,
                      fill_by = "first_variable",
                      # col_vector_flow = 'grey',
                      # col_vector_value = palette_filter( greys = F),
                      stratum_label_size = 1, 
                      stratum_width = 1/3,
                      colorful_fill_variable_stratum = T) 
parcats::parcats(p2, marginal_histograms = FALSE, imp = TRUE, data_input = df_p, arrangement = 'freeform')
