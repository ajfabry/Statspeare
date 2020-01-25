## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, include = FALSE----------------------------------------------
library(Statspeare)
load(system.file("extdata", "P.rda", package = "Statspeare"))
load(system.file("extdata", "W.rda", package = "Statspeare"))

## ------------------------------------------------------------------------
zscore(c(1,2,3,4))

## ------------------------------------------------------------------------
sort(zscore(P["love",]), decreasing = T)

## ------------------------------------------------------------------------
correlation(c(1,2,3,4), c(1,2,3,5))

## ------------------------------------------------------------------------
correlation(W["loue",], W["hate",])

## ------------------------------------------------------------------------
top_relations(W, "loue", "most similar", 15)

## ------------------------------------------------------------------------
centrality("Ham", "degree")

## ---- fig.height = 6, fig.width = 6, fig.align = "center"----------------
show_graph("Rom")

