# Visualize the data in sankey
# Load package
install.packages("networkD3")
library(networkD3)
library(dplyr)

df <- read.csv('ses_data.csv')

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

library(networkD3)
library(htmlwidgets)

skplot <- sankeyNetwork(Links = links, Nodes = nodes, Source = 'source',
              Target = 'target', Value = 'value', NodeID = 'name',
              height = 1200, width = 1200,
              fontSize = 12, nodeWidth = 30)

rbokeh::widget2png(skplot, "sankey.png")
