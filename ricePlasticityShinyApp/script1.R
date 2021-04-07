# Install the spatialHeatmap package.
devtools::install_github('jianhaizhang/spatialHeatmap')

library(spatialHeatmap)
# Path of aSVG files.
svg1 <- 'oryza.sativa_cross.sections.root.v7_shm.svg'
svg5 <- 'oryza.sativa_cross.long.sections.root.v5_shm.svg'
# Import the data
mat <- read_fr('TPM_TRAP_shm.txt')
# Optionally filter the data.
# mat <- filter_data(data=mat, pOA=c(3/ncol(mat), 5), top.CV=0.01, dir='.'); dim(mat)
# Spatial heatmap of cross section on gene OS03G0667300.
shm1 <- spatial_hm(svg.path=svg1, data=mat, ID=c('OS03G0667300'), width=1, height=0.9, legend.r=2.7, legend.nrow=4, line.size=0.1, cores=1)

# Spatial heatmap of cross/long section on gene OS03G0667300.
shm5 <- spatial_hm(svg.path=svg5, data=mat, ID=c('OS03G0667300'), width=1, height=0.8, legend.r=3, legend.nrow=6, line.size=0.1, cores=1) 


