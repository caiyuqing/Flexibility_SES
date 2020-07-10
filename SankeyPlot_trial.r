# Visualize the data in sankey
# Load package

### clean the memory to avoid unnecessary errors:
rm(list = ls())

install.packages("networkD3")
library(networkD3)
library(dplyr)
library(htmlwidgets)
library(htmltools)
if (!require(easyalluvial)) {install.packages("easyalluvial",repos = "http://cran.us.r-project.org"); require(easyalluvial)}

df <- read.csv('ses_data.csv') %>%
        dplyr::mutate(Uni_data = paste('data',Uni_data,sep='_'),
                      SES_Ver = paste('Index',SES_Ver,sep='_'))
df_p <- df %>% dplyr::select(-1) %>%
        dplyr::select(Nation, everything()) 
df_p %>%
        alluvial_wide(.,
                      col_vector_flow = 'grey',
                      col_vector_value = palette_filter( greys = F),
                      stratum_label_size = 1, 
                      stratum_width = 1/3,
                      colorful_fill_variable_stratum = F) %>%
        add_marginal_histograms(df_p)


links <- df %>%
        dplyr::select(-Order_by_author) %>%
        dplyr::mutate(row = row_number()) %>%
        tidyr::gather('column', 'source', -row) %>%
        dplyr::mutate(column = match(column, names(df))) %>%
        dplyr::group_by(row) %>%
        dplyr::arrange(column) %>%
        dplyr::mutate(target = lead(source)) %>%
        dplyr::ungroup() %>%
        dplyr::filter(!is.na(target))

links <- links %>%
        dplyr::mutate(source = paste0(source, '_', column),
                      target = paste0(target, '_', column + 1)) %>%
        select(source, target)

nodes <- data.frame(name = unique(c(links$source, links$target)))

links$source <- match(links$source, nodes$name) -1
links$target <- match(links$target, nodes$name) -1
links <- links %>% 
        dplyr::group_by(source, target) %>%
        dplyr::summarise(value = n())

nodes$name <- sub('_[0-9]+$', '', nodes$name)


skplot <- sankeyNetwork(Links = links, Nodes = nodes, Source = 'source',
              Target = 'target', Value = 'value', NodeID = 'name',
              height = 1200, width = 1200,
              fontSize = 12, nodeWidth = 30, nodePadding = 2) %>%
        htmlwidgets::prependContent(htmltools::tags$h1("SES_Flexibility")) %>% 
        saveNetwork(file = 'SES_Flexibility.html')
skplot

rbokeh::widget2png(skplot, "sankey.png")

## try ploty https://plot.ly/r/sankey-diagram/
library(plotly)

#nodes$color <- as.factor(nodes$name)


p <- plotly::plot_ly(type = "sankey",
                     domain = list(
                             x = c(0, 1),
                             y = c(0, 1)
                     ),
                     orientation = "h",
                     valueformat = ".0f",
                     valuesuffix = "TWH",
                     node = list(
                             label = nodes$name,
                             #color = nodes$color,
                             pad = 30,
                             thickness = 15,
                             line = list(
                                     color = "black",
                                     width = 0.5
                             )
                     ),
                     link = links
                     ) %>%
        plotly::layout(
                title = "Sankey Diagram for flexibility of SES",
                font = list(
                        size = 10
                ),
                xaxis = list(showgrid = F, zeroline = F),
                yaxis = list(showgrid = F, zeroline = F)
        )

p

#### Plot Sankey plot for incomes

df_income <- read.csv('income_data.csv', stringsAsFactors=FALSE) %>%
        dplyr::rename(Order_by_author = 1) %>%
        dplyr::mutate(Item_form = ifelse(is.na(Item_form), "NA", Item_form)) %>%
        dplyr::filter(!is.na(Order_by_author))


links_inc <- df_income %>%
        dplyr::select(-Order_by_author) %>%
        dplyr::mutate(row = row_number()) %>%
        tidyr::gather('column', 'source', -row) %>%
        dplyr::mutate(column = match(column, names(df_income))) %>%
        dplyr::group_by(row) %>%
        dplyr::arrange(column) %>%
        dplyr::mutate(target = lead(source)) %>%
        dplyr::ungroup() %>%
        dplyr::filter(!is.na(target))

links_inc <- links_inc %>%
        dplyr::mutate(source = paste0(source, '_', column),
                      target = paste0(target, '_', column + 1)) %>%
        select(source, target)

nodes_inc <- data.frame(name = unique(c(links_inc$source, links_inc$target)))

links_inc$source <- match(links_inc$source, nodes_inc$name) -1
links_inc$target <- match(links_inc$target, nodes_inc$name) -1
links_inc <- links_inc %>% 
        dplyr::group_by(source, target) %>%
        dplyr::summarise(value = n())

nodes_inc$name <- sub('_[0-9]+$', '', nodes_inc$name)

## try ploty https://plot.ly/r/sankey-diagram/
library(plotly)

#nodes$color <- as.factor(nodes$name)


p_inc <- plotly::plot_ly(type = "sankey",
                     domain = list(
                             x = c(0, 1),
                             y = c(0, 1)
                     ),
                     orientation = "h",
                     valueformat = ".0f",
                     valuesuffix = "TWH",
                     node = list(
                             label = nodes_inc$name,
                             #color = nodes$color,
                             pad = 30,
                             thickness = 15,
                             line = list(
                                     color = "black",
                                     width = 0.5
                             )
                     ),
                     link = links_inc
) %>%
        plotly::layout(
                title = "Sankey Diagram for flexibility of income",
                font = list(
                        size = 10
                ),
                xaxis = list(showgrid = F, zeroline = F),
                yaxis = list(showgrid = F, zeroline = F)
        )

p_inc

## tried to using ggplot 2 extension, failed
library(ggplot2)
library(ggalluvial)
ggplot2::ggplot(df, aes(y = SES_Ver, axis1 = Dev_stage, axis2 = Nation)) +
        geom_alluvium(aes(fill = Dev_stage, color = Dev_stage),
                      width = 1/12, alpha = alpha, knot.pos = 0.4) +
        geom_stratum(width = 1/6, color = "grey") +
        geom_label(stat = "stratum", label.strata = TRUE) +
        scale_x_continuous(breaks = 1:3, labels = c("Age", "Nation", "Dataset")) +
        #scale_fill_manual(values = )
        ggtitle("flexibility of SES") +
        theme_minimal() +
        theme(axis.text.x = element_text(size = 12, face = "bold"))
