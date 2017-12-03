# Main functions for SaniPath analysis and plotting functions
sapply(grep('.R$', list.files('model', full.names = T), value=T), source)
load("rsrc/mpn_tbl.rda")
load("rsrc/intake.rda")
source('config.R')


