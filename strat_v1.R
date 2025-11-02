# Load Data and clean workspace
source("getETFData.R")
source("getMacroData.R")
rm(list=setdiff(ls(), c("assets_long", "assets_wide", "macro_long", "macro_wide")))



