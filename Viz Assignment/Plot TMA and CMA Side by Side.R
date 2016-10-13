## Plot images to compare outliers and non outliers ##

library(gridExtra)

grid.arrange(cma_only, cma_only_no_outliers, tma_only, tma_only_no_outliers, ncol=2)
