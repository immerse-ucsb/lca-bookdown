
# This code is adapted from the source code for the `plotLTA` function found in from the `MplusAutomation` package:
# Link to MplusAuotomation source code:
# https://github.com/michaelhallquist/MplusAutomation/blob/995d1ecfae3656524153456ce647f86fe8c1cf1e/R/mixtures.R


if (!requireNamespace("ggrepel", quietly = TRUE)) {
  install.packages("ggrepel")
}
library(ggrepel)

plot_patterns <- function(model_name,facet_labels,lca_labels,class_labels, title, subtitle){
  
  node_stroke = 2
  edges <- model_name$class_counts$transitionProbs
  
  all_classes <- unique(c(edges$from, edges$to))
  latent_variables <- unique(gsub("\\..+$", "", all_classes))
  edges$x <- as.numeric(factor(gsub("\\..+$", "", as.character(edges$from)), levels = latent_variables))
  edges$xend <-as.numeric(factor(gsub("\\..+$", "", as.character(edges$to)), levels = latent_variables))
  edges$y <- as.numeric(gsub("^.+\\.", "", as.character(edges$from)))
  edges$yend <- as.numeric(gsub("^.+\\.", "", as.character(edges$to)))
  
  edges <- edges %>% 
    mutate(trans_perc = round(probability*100,0)) %>% 
    mutate(y_mid = y + ((yend - y)/2)) 
  
  nodes <- rbind(edges[, c(1, 4, 6)], setNames(edges[, c(2, 5, 7)], names(edges)[c(1, 4, 6)]))
  nodes <- nodes[!duplicated(nodes),]
  names(nodes)[1] <- "nodeID"
  
  n_prop <- model_name$class_counts$modelEstimated %>% 
    mutate(percent = round(proportion*100,0))
  
  n_prop$proportion <- node_stroke * n_prop$proportion * inverse.rle(list(
    lengths = rle(n_prop$variable)$lengths,
    values = rle(n_prop$variable)$lengths))
  
  n_prop$nodeID <- paste(n_prop$variable, n_prop$class, sep = ".")
  nodes <- merge(nodes, n_prop, by = "nodeID") 
  nodesize <- max(max(sapply(nodes$nodeID, nchar)) * 4.5, 6)

  nodes2 <- nodes %>% rename(x1 = x, y1 = y) %>% 
    mutate(class = factor(class))
  
  p <- ggplot(NULL) +
    geom_segment(
      data = edges,
      aes(
        x = x,
        y = y,
        xend = xend,
        yend = yend,
        size = probability,
        alpha = probability
      ),
      color = "black"
    ) +
    scale_alpha_continuous(range = c(.05, .5)) +
    facet_wrap( ~ y, labeller = labeller(
      y = function(x)
        str_wrap(facet_labels, width = 40)
    )) +
    scale_x_continuous(expand = c(.1, .1),
                       breaks = c(1, 2),
                       labels = lca_labels) +
    scale_y_reverse(
      expand = c(.15, .15),
      breaks = seq_along(unique(class_labels)),
      labels = class_labels,
      position = "right"
    ) +
    geom_text_repel(
      data = edges,
      aes(
        x = xend,
        y = y_mid,
        label = glue("{trans_perc}%"),
        fontface = "bold"
      ),
      segment.colour = NA,
      nudge_x = -.5
    ) +
    geom_point(
      data = nodes2,
      shape = 21,
      size = nodesize,
      color = "black",
      aes_string(
        x = "x1",
        y = "y1",
        stroke = "proportion",
        fill = "class"
      ),
      show.legend = FALSE
    ) +
    #scale_fill_grey(start = 0.5, end = 0.7) +
    geom_text(data = nodes2, fontface = "bold", aes(
      x = x1,
      y = y1,
      label = paste0(percent, "%")
    )) +
    theme_classic() + 
    labs(
      y = "",
      x = "",
      subtitle = subtitle,
      title = title
    ) +
    theme(
      legend.position = "none",
      panel.grid.minor.x = element_blank(),
      text = element_text(family = "serif", size = 20, color = "black"),
      axis.text.x = element_text(vjust = 1, color = "black"),
      plot.subtitle = element_text(face = "italic", size = 20),
      plot.title = element_text(size = 20),
      axis.line.y=element_blank(),
      axis.text.y = element_text(color = "black"),
    ) 
  
  p 
  return(p) 
}


  
  
  