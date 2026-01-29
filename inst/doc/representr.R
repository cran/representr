## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE
)

library(ggplot2)
set.seed(1234)

## ----ex-records---------------------------------------------------------------
# load library
library(representr)

# load data
data("rl_reg1") # data for record linkage and regression
data("identity.rl_reg1") # true identity of each record

## ----echo = FALSE-------------------------------------------------------------
knitr::kable(head(rl_reg1))

## -----------------------------------------------------------------------------
data("linkage.rl")

## ----echo=FALSE---------------------------------------------------------------
# take the last iteration of linkage for lambda value
lambda <- linkage.rl[nrow(linkage.rl),]

# split data by linkage results
clusters <- split(rl_reg1, lambda)

## ----echo=FALSE---------------------------------------------------------------
knitr::kable(clusters[[names(which(table(lambda) == 4)[1])]])

## ----random-------------------------------------------------------------------
# ids for representative records (random)
random_id <- represent(rl_reg1, lambda, "proto_random", parallel = FALSE)
rep_random <- rl_reg1[random_id,] # representative records (random)

## ----echo = FALSE-------------------------------------------------------------
knitr::kable(rep_random[as.numeric(names(which(table(lambda) == 4)))[1:5],])

## ----minimax------------------------------------------------------------------
# additional parameters for minimax prototyping
# need column types, the order levels for any ordinal variables, and column weights
col_type <- c("string", "string", "numeric", "numeric", "numeric", "categorical", "ordinal", "numeric", "numeric")
orders <- list(education = c("Less than a high school diploma", "High school graduates, no college", "Some college or associate degree", "Bachelor's degree only", "Advanced degree"))
weights <- c(.25, .25, .05, .05, .1, .15, .05, .05, .05)

# ids for representative records (minimax)
minimax_id <- represent(rl_reg1, linkage.rl[nrow(linkage.rl),], "proto_minimax",
                        distance = dist_col_type, col_type = col_type, 
                        weights = weights, orders = orders, scale = TRUE, parallel = FALSE)
rep_minimax <- rl_reg1[minimax_id,] # representative records (minimax)

## ----echo = FALSE-------------------------------------------------------------
knitr::kable(rep_minimax[as.numeric(names(which(table(lambda) == 4)))[1:5],])

## ----composite----------------------------------------------------------------
# representative records (composite)
rep_composite <- represent(rl_reg1, linkage.rl[nrow(linkage.rl),], "composite", col_type = col_type, parallel = FALSE)

## ----echo = FALSE-------------------------------------------------------------
knitr::kable(rep_composite[as.numeric(names(which(table(lambda) == 4)))[1:5],])

## ----pp-----------------------------------------------------------------------
# Posterior prototyping weights
pp_weights <- pp_weights(rl_reg1, linkage.rl[seq(80000, 100000, by = 100), ], 
                         "proto_minimax", distance = dist_col_type, 
                         col_type = col_type, weights = weights, orders = orders, 
                         scale = TRUE, parallel = FALSE)

## ----thresh-plot, fig.width = 5, echo = FALSE, fig.align="center", fig.cap = "The distribution of PP weights as generated from posterior draws of $\\boldsymbol \\Lambda$ colored by if they are true or duplicated records in the original data set. The dotted vertical line shows the threshold value of 0.5. The true records have consistently higher PP weights and the proportion of duplicated records with high weights is relatively low."----
data.frame(pp_weights = pp_weights,
           true = seq_len(nrow(rl_reg1)) %in% unique(identity.rl_reg1),
           included = pp_weights >= .5) -> threshold_df

ggplot(threshold_df) +
  geom_histogram(aes(pp_weights, fill = true), position = "dodge") +
  geom_vline(aes(xintercept = .5), lty = 2, alpha = .5) +
  xlab("Posterior Prototyping weights") +
  ylab("") +
  scale_fill_discrete("True record")  +
  theme(legend.position = "bottom")

## ----pp_thresh----------------------------------------------------------------
# representative records (PP threshold)
rep_pp_thresh <- rl_reg1[pp_weights > .5, ]

## ----echo = FALSE-------------------------------------------------------------
knitr::kable(head(rep_pp_thresh))

## ----empkl_div, message = FALSE, warning = FALSE------------------------------
true_dat <- rl_reg1[unique(identity.rl_reg1),] # true records
emp_kl_div(true_dat, rep_random, c("sex"), c("income", "bp"))
emp_kl_div(true_dat, rep_minimax, c("sex"), c("income", "bp"))
emp_kl_div(true_dat, rep_composite, c("sex"), c("income", "bp"))
emp_kl_div(true_dat, rep_pp_thresh, c("sex"), c("income", "bp"))

