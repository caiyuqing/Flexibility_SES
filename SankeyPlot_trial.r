# Visualize the data in sankey/alluvial plot
# Load package

### clean the memory to avoid unnecessary errors:
rm(list = ls())

if (!require(tidyverse)) {install.packages("tidyverse",repos = "http://cran.us.r-project.org"); require(tidyverse)}
if (!require(easyalluvial)) {install.packages("easyalluvial",repos = "http://cran.us.r-project.org"); require(easyalluvial)}
if (!require(parcats)) {install.packages("parcats",repos = "http://cran.us.r-project.org"); require(parcats)}

if (!require(randomcoloR)) {install.packages("randomcoloR",repos = "http://cran.us.r-project.org"); 
        require(randomcoloR)}

# if (!require(networkD3)) {install.packages("networkD3",repos = "http://cran.us.r-project.org"); 
#        require(networkD3)}
#library(htmltools)
#library(randomcoloR)

df <- read.csv('ses_data.csv') %>%
        dplyr::mutate(Uni_data = paste('data',Uni_data,sep='_'),
                      SES_Ver = paste('Index',SES_Ver,sep='_'))
df_p <- df %>% dplyr::select(-1) %>%
        dplyr::select(Nation, everything()) %>%
        dplyr::relocate(SES_Ver, .after = last_col()) %>% 
        dplyr::relocate(Uni_data,.before = SES_Ver) %>%
        dplyr::mutate_if(is.character,as.factor)

n_color <- df_p %>%
        dplyr::distinct(., SES_Ver) %>%
        nrow()

palette <- randomcoloR::distinctColorPalette(n_color)
        
p <- df_p %>%
        easyalluvial::alluvial_wide(.,
                      fill_by = 'first_variable',
#                      col_vector_flow = 'grey50',
                      # palette_qualitative() %>% palette_filter(greys = F), #
#                      col_vector_value = palette, # RColorBrewer::brewer.pal(name = 'Dark2'),
                      stratum_label_size = 3, 
                      stratum_width = 1/4
#                      colorful_fill_variable_stratum = F
                      )
parcats::parcats(p, marginal_histograms = FALSE, imp = TRUE, data_input = df_p)

# 2nd way: plot all the flow as gray.
p2 <- df %>% dplyr::select(-1) %>% dplyr::mutate_if(is.character,as.factor) %>%
        easyalluvial::alluvial_wide(.,
                                    #fill_by = 'first_variable',
                                                          col_vector_flow = 'grey50',
                                    # palette_qualitative() %>% palette_filter(greys = F), #
                                                          col_vector_value = palette, # RColorBrewer::brewer.pal(name = 'Dark2'),
                                    stratum_label_size = 3, 
                                    stratum_width = 1/4
                                    #                      colorful_fill_variable_stratum = F
        )

parcats::parcats(p2, marginal_histograms = FALSE, imp = TRUE, 
                 data_input = df %>% dplyr::select(-1) %>% dplyr::mutate_if(is.character,as.factor))

