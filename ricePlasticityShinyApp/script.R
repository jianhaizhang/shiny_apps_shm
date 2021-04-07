devtools::install_github('jianhaizhang/spatialHeatmap', build_vignettes = FALSE)

library(spatialHeatmap)
svg1 <- 'oryza.sativa_cross.sections.root.v7_shm.svg'
svg2 <- 'oryza.sativa_cross.long.sections.root.v3_shm.svg'
svg3 <- 'oryza.sativa_cross.long.sections.root.v4_shm.svg'
svg5 <- 'oryza.sativa_cross.long.sections.root.v5_shm.svg'
mat <- read_fr('TPM_TRAP_shm.txt')
mat1 <- filter_data(data=mat, pOA=c(3/ncol(mat), 5), top.CV=0.01, dir='.'); dim(mat1)
system.time(
shm5 <- spatial_hm(svg.path=svg5, data=mat, ID=c('OS01G0136050'), legend.r=1, legend.nrow=6, line.size=0.1, cores=1) 
)


library(ggplot2)
ids <- factor(c("1.1", "2.1", "1.2", "2.2", "1.3", "2.3"))

values <- data.frame(id = ids, value = c(3, 3.1, 3.1, 3.2, 3.15, 3.5))

positions <- data.frame(
  id = rep(ids, each = 4),
  x = c(2, 1, 1.1, 2.2, 1, 0, 0.3, 1.1, 2.2, 1.1, 1.2, 2.5, 1.1, 0.3,
  0.5, 1.2, 2.5, 1.2, 1.3, 2.7, 1.2, 0.5, 0.6, 1.3),
  y = c(-0.5, 0, 1, 0.5, 0, 0.5, 1.5, 1, 0.5, 1, 2.1, 1.7, 1, 1.5,
  2.2, 2.1, 1.7, 2.1, 3.2, 2.8, 2.1, 2.2, 3.3, 3.2)
)

# Currently we need to manually merge the two together
datapoly <- merge(values, positions, by = c("id"))
mar.lb <- c(0.01, 0.01)
gg.all <- lapply(1:10, function(x) return(ggplot(datapoly, aes(x = x, y = y)) + geom_polygon(aes(fill = value, group = id)) + theme(plot.margin=margin(mar.lb[2], mar.lb[1], mar.lb[2], mar.lb[1], "npc"),legend.position = 'none') + ggtitle(paste0('Plot ', x))))
gg.all <- lapply(1:10, function(x) return(ggplot(datapoly, aes(x = x, y = y)) + geom_polygon(aes(fill = value, group = id)) + theme(plot.margin=margin(mar.lb[2], mar.lb[1], mar.lb[2], mar.lb[1], "npc"), legend.position = 'none', aspect.ratio = 1) + ggtitle(paste0('Plot ', x))))

names(gg.all) <- paste0('gg', 1:10)
dev.new(width=10, height=5, unit="in")

grob.list <- lapply(gg.all, ggplotGrob)

g.tr <- lapply(grob.list[seq_len(length(grob.list))], grobTree)

lay <- matrix(1:10, byrow = TRUE, ncol = 2)
lay1 <- cbind(lay, rep(NA, 5))

g.arr <- arrangeGrob(grobs=g.tr, layout_matrix=lay1, widths=unit(rep(1/2, 2), "npc"), heights=unit(rep(1/5, 5), "npc"))
lgd.lis <- gg.all[1:2]
lgd.lis <- lapply(lgd.lis, function(x) return(x+theme(aspect.ratio=1/3)))
grob.lgd.lis <- lapply(lgd.lis, ggplotGrob)
lgd.tr <- lapply(grob.lgd.lis, grobTree)
legend.r <- 5; na.lgd <- names(lgd.lis)
lgd.w <- 0.99; lgd.h <- 0.99/length(na.lgd)/legend.r
if (lgd.h*length(na.lgd)>1) { lgd.h <- 0.99/length(na.lgd); lgd.w <- lgd.h*legend.r } 

lgd.arr <- arrangeGrob(grobs=lgd.tr, layout_matrix=matrix(seq_along(na.lgd), ncol=1), widths=unit(0.99, "npc"), heights=unit(rep(0.3, length(na.lgd)), "npc"))

grid.arrange(lgd.arr, ncol=1, widths=unit(c(1), 'npc'))

bar.width <- 0.08; ncol <- 2; legend.width <- 1
w.lgd <- (1-bar.width)/(ncol+1)*legend.width # Legend is reduced.
shm.w <- 1-bar.width-w.lgd
# A plot pops up when 'grid.arrange' runs.
shm <- grid.arrange(g.arr, lgd.arr, ncol=2, widths=unit(c(shm.w, w.lgd), 'npc'))

grid.arrange(g.arr, ncol=1, widths=unit(c(0.8), 'npc'))
grid.arrange(g.arr, g.arr, ncol=2, widths=unit(c(0.5, 0.5), 'npc'))


dat <- datapoly
dat$x <- dat$x/10; dat$y <- dat$y/10 
ggplot(dat, aes(x = x, y = y)) + geom_polygon(aes(fill = value, group = id))





library(ggplot2); library(grid); library(gridExtra)
load('grob.lis.all'); load('all')
gg.all <- grob.lis.all[[1]][[3]]

mar.lb <- sub.margin <- (1-c(129.33128, 32.871689)/129.33128*0.99)/2
mar.lb <- mar.lb*100; mar.lb
gg.all <- lapply(gg.all, function(x) x + theme(aspect.ratio=1/2, legend.position = "none"))
gg.all[1:5] <- lapply(gg.all[1:5], function(x) x + theme(aspect.ratio=1/2, legend.position = "none"))
gg.all[6:10] <- lapply(gg.all[6:10], function(x) x + theme(aspect.ratio=1/4, legend.position = "none"))
gg.all <- lapply(gg.all, function(x) x + theme(aspect.ratio=1159.45/4582.11, legend.position = "none"))
gg.all <- lapply(gg.all, function(x) x + theme(aspect.ratio=32.871689/129.33128, legend.position = "none"))
gg.all <- lapply(gg.all, function(x) x + theme(aspect.ratio=1, legend.position = "none"))
gg.all <- lapply(gg.all, function(x) x + theme(legend.position = "none", plot.margin=margin(mar.lb[2], mar.lb[1], mar.lb[2], mar.lb[1], "npc")))
gg.all <- lapply(gg.all, function(x) x + theme(legend.position = "none", plot.margin=margin(mar.lb[2], mar.lb[1], mar.lb[2], mar.lb[1], "pt")))

grob.all <- lapply(gg.all, ggplotGrob)
grob.list <- grob.all

grid.arrange(g.arr, g.arr, ncol=2, widths=unit(c(0.5, 0.5), 'npc'))
grid.arrange(g.arr, ncol=1, widths=unit(c(1), 'npc'))


system.time(
shm1 <- spatial_hm(svg.path=svg1, data=mat, ID=c('OS01G0136050'), legend.r=2.7, legend.nrow=4, preserve.scale=FALSE) 
)

# The list with name slots of "name", "data", and "svg".
lis1 <- list(name='root.cross', data='customData.txt', svg=svg1)
lis2 <- list(name='root.cross.long', data='customData.txt', svg=svg5)

# Retrieve the default parameters.
lis.par <- custom_shiny(lis.par.tmp=TRUE)
# The default dataset to show upon the app is launched.
lis.par$default.dataset <- 'root.cross'
lis.par$shm.img[c('height', 'width', 'preserve.scale'), 'default'] <- c(700, 590, 'No')
lis.par$legend['key.row', 'default'] <- 4

# Create custom Shiny app by feeding this function these datasets and parameters.
custom_shiny(lis1, lis2, lis.par=lis.par, example=FALSE, app.dir='.')
# Lauch the app.
shiny::runApp('shinyApp') 

devtools::install_github('jianhaizhang/spatialHeatmap', force=T, build_vignettes=F)
options(repos = BiocManager::repositories())
library(rsconnect); deployApp(appDir='shinyApp', account='tgirke', appName='ricePlasticity')

library(rsconnect); deployApp(appDir='shinyApp', account='jianhaizhang', appName='ricePlasticity')



data.path1 <- system.file('extdata/shinyApp/example/expr_arab.txt', package='spatialHeatmap')
svg.path1 <- system.file('extdata/shinyApp/example/arabidopsis.thaliana_shoot_shm.svg', package='spatialHeatmap')
# The list with name slots of "name", "data", and "svg".
lis.dat1 <- list(name='shoot', data=data.path1, svg=svg.path1)
lis.par$default.dataset <- 'shoot'

custom_shiny(lis.dat1, lis.par=lis.par, example=TRUE, app.dir='.')


df.random <- data.frame(matrix(sample(x=1:100, size=50, replace=TRUE), nrow=10))
colnames(df.random) <- c('shoot_totalA__condition1', 'shoot_totalA__condition2', 'shoot_totalB__condition1', 'shoot_totalB__condition2', 'notMapped') # Assign column names
rownames(df.random) <- paste0('gene', 1:10) 
svg.sh1 <- system.file("extdata/shinyApp/example", "arabidopsis.thaliana_organ_shm1.svg", package="spatialHeatmap")
svg.sh2 <- system.file("extdata/shinyApp/example", "arabidopsis.thaliana_organ_shm2.svg", package="spatialHeatmap")
spatial_hm(svg.path=c(svg.sh1, svg.sh2), data=df.random, ID=c('gene1'), legend.r=0.2)


w.h1 <- svg.df.lis$arabidopsis.thaliana_organ_shm1.svg$w.h
w.h2 <- svg.df.lis$arabidopsis.thaliana_organ_shm2.svg$w.h

relative.scale <- 2; sub.title.size <- 20
if (is.numeric(relative.scale) & relative.scale > 0) for (i in names(svg.df.lis)) {

mar.tb <- ((1 - w.h1['height'] / w.h.max) * (0.99 / nrow(lay.mat))) / 2 * relative.scale

gg.all1 <- gg.all2 <- gg.all
for (i in seq_along(gg.all1)) {
g <- gg.all1[[i]]
gg.all1[[i]] <- g+theme(plot.margin=margin(mar.tb, 0.005, mar.tb, 0.005, "npc"), plot.title=element_text(hjust=0.5, size=sub.title.size))
}

gg.lis <- c(gg.all1, gg.all2)
grob.lis <- lapply(gg.lis, ggplotGrob)
tr.lis <- lapply(grob.lis, grobTree)

arr <- arrangeGrob(grobs=tr.lis, layout_matrix=matrix(seq_along(gg.lis), ncol=2), widths=unit(c(0.5, 0.5), "npc"), heights=unit(rep(0.99/4, 4), "npc"))

grid.arrange(arr, ncol=1, widths=unit(c(1), 'npc'))

}

w.lgd <- (1-bar.width)/(ncol+1)
shm.w <- 1-bar.width-w.lgd
legend.r =0.1

lgd.arr <- arrangeGrob(grobs=lgd.tr, layout_matrix=matrix(seq_along(na.lgd), ncol=1), widths=unit(0.99, "npc"), heights=unit(rep(w.lgd + (0.99 - w.lgd) * legend.r, length(na.lgd)), "npc"))




lgd.arr <- arrangeGrob(grobs=lgd.tr, layout_matrix=matrix(seq_along(na.lgd), ncol=1), widths=unit(w.lgd, "npc"), heights=unit(rep(w.lgd + (0.99 - w.lgd) * legend.r, length(na.lgd)), "npc"))

grob.lgd.lis <- lapply(lgd.lis, ggplotGrob)
lgd.tr <- lapply(grob.lgd.lis, grobTree)

w.lgd <- (1-bar.width)/(ncol+1)
shm.w <- 1-bar.width-w.lgd
legend.r =0.1

lgd.arr <- arrangeGrob(grobs=lgd.tr, layout_matrix=matrix(seq_along(na.lgd), ncol=1), widths=unit(0.99, "npc"), heights=unit(rep(w.lgd + (0.99 - w.lgd) * legend.r, length(na.lgd)), "npc"))


grid.arrange(lgd.arr, g.arr, lgd.arr, ncol=3, widths=unit(c(bar.width-0.005, shm.w, w.lgd), 'npc'))

library(grid); library(ggplot2)
p1 <- qplot(mpg, wt, data = mtcars, colour = cyl)
p2 <- qplot(mpg, data = mtcars) + ggtitle("title")
p3 <- qplot(mpg, data = mtcars, geom = "dotplot")
p4 <- p1 + facet_wrap( ~ carb, nrow = 1) + theme(legend.position = "none") + ggtitle("facetted plot")

pushViewport(viewport(layout = grid.layout(2, 2)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(p1, vp = vplayout(1, 1:2))
print(p2, vp = vplayout(2, 1))
print(p3, vp = vplayout(2, 2))


load('all')
cna <- colnames(gene)
ft.dat <- unique(gsub('^(.*)__.*', '\\1', cna))
lis <- rep(list(NULL), 3); names(lis) <- ft.dat
lis[[2]] <- c('shoot_totalB', 'root_total') 


gg_shm <- function(gene, con.na=TRUE, geneV, coord, ID, cols, tis.path, lis.rematch = NULL, ft.trans=NULL, sub.title.size, ft.legend='identical', legend.col, legend.ncol=NULL, legend.nrow=NULL, legend.position='bottom', legend.direction=NULL, legend.key.size=0.02, legend.text.size=12, legend.plot.title=NULL, legend.plot.title.size=11, line.size=0.2, line.color='grey70', aspect.ratio = 1, ...) {

    save(gene, con.na, geneV, coord, ID, cols, tis.path, lis.rematch, ft.trans, sub.title.size, ft.legend, legend.col, legend.ncol, legend.nrow, legend.position, legend.direction, legend.key.size, legend.text.size, legend.plot.title, legend.plot.title.size, line.size, line.color, aspect.ratio, file='all')
  
  # Main function to create SHMs (by conditions) and legend plot.
  g_list <- function(con, lgd=FALSE, ...) {
    if (is.null(con)) cat('Legend plot ... \n') else cat(con, ' ')
    value <- feature <- x <- y <- tissue <- NULL; tis.df <- as.vector(unique(coord[, 'tissue']))
    # tis.path and tis.df have the same length by default, but not entries, since tis.df is appended '__\\d+' at the end.
    # Assign default colours to each path.
    g.col <- rep(NA, length(tis.path)); names(g.col) <- tis.df
    if (lgd==FALSE) {
      # Keep text colors in the main SHM.
      g.col <- legend.col[grep('_LGD$', names(legend.col), ignore.case=TRUE)][sub('__\\d+$', '', names(g.col))]
      names(g.col) <- tis.df # Resolves legend.col['tissue'] is NA by default. 
      # Un-related aSVG mapped to data.
      if (rematch.dif.svg) {
        # The data column index of data features that are assinged new aSVG features under a specific condition.
        tis.tar.idx <- cname %in% paste0(tis.tar, '__', con)
        # In data columns, feature__conditions and colors have the same index. 
        tis.tar.col <- color.dat[tis.tar.idx]; names(tis.tar.col) <- tis.tar
        # Copy colors of target features in data to corresponding aSVG features according to lis.rematch. 
        col.ft.rematch <- NULL; for (i in names(lis.rematch[tis.tar])) {
          ft.svg <- lis.rematch[tis.tar][[i]]
          col0 <- rep(tis.tar.col[i], length(ft.svg)); names(col0) <- ft.svg
          col.ft.rematch <- c(col.ft.rematch, col0)
        }
        for (i in names(col.ft.rematch)) g.col[tis.path %in% i] <- col.ft.rematch[i]
       } else {
        con.idx <- grep(paste0("^", con, "$"), cons)
        # Target tissues and colors in data columns.
        tis.col1 <- tis.col[con.idx]; color.dat1 <- color.dat[con.idx]
        for (i in unique(tis.path)) {
        # Map target colors to target tissues.
        tis.idx <- which(tis.col1 %in% i); if (length(tis.idx)==1) {
          # Account for single-shape tissue without '__\\d+$' and multi-shape tissue with '__\\d+$'.
          pat <- paste0(paste0('^', i, '$'), '|', paste0('^', i, '__\\d+$'))
          g.col[grep(pat, tis.df)] <- color.dat1[tis.idx] # names(g.col) is tis.df 
        }
      }
      # Rematch features between the same pair of data-aSVG.
      if (is.list(lis.rematch)) {
        for (i in seq_along(lis.rematch)) {
          lis0 <- lis.rematch[i]; if (!is.character(lis0[[1]])) next
          # Index of features will be rematched.
          idx.tis.rematch <- tis.path %in% lis0[[1]]
          # Index of color for rematching.
          idx.tis.rematch.color <- tis.path %in% names(lis0)
          # if (sum(idx.tis.rematch.color) == 0) stop(paste0('Feature "', names(lis0), '" is not detected in aSVG!'))
          if (sum(idx.tis.rematch) == 0 | sum(idx.tis.rematch.color) == 0) next
          g.col[idx.tis.rematch] <- unique(g.col[idx.tis.rematch.color])
         }
       }
     }
    } 
    # The colors might be internally re-ordered alphabetically during mapping, so give them names to fix the match with tissues. E.g. c('yellow', 'blue') can be re-ordered to c('blue', 'yellow'), which makes tissue mapping wrong. Correct: colours are not re-ordered. The 'tissue' in 'data=coord' are internally re-ordered according to a factor. Therfore, 'tissue' should be a factor with the right order. Otherwise, disordered mapping can happen. Alternatively, name the colors with corresponding tissue names.
    # aes() is passed to either ggplot() or specific layer. Aesthetics supplied to ggplot() are used as defaults for every layer. 
    # Show selected or all samples in legend.
    if (length(ft.legend)==1) if (ft.legend=='identical') {
      if (rematch.dif.svg) {
        ft.legend <- intersect(unlist(lis.rematch), unique(tis.path)) 
      } else ft.legend <- intersect(sam.uni, unique(tis.path)) 
    } else if (ft.legend=='all') ft.legend <- unique(tis.path)
    
    if (lgd==FALSE) { # Legend plot.
      # Make selected tissues transparent by setting their colours NA.
      if (!is.null(ft.trans)) g.col[sub('__\\d+$', '', tis.df) %in% ft.trans] <- NA # This step should not be merged with 'lgd=T'.
      ft.legend <- setdiff(ft.legend, ft.trans) 
      leg.idx <- !duplicated(tis.path) & (tis.path %in% ft.legend)
      # Bottom legends are set for each SHM and then removed in 'ggplotGrob', but a copy with legend is saved separately for later used in video.
      scl.fil <- scale_fill_manual(values=g.col, breaks=tis.df[leg.idx], labels=tis.path[leg.idx], guide=guide_legend(title=NULL, ncol=legend.ncol, nrow=legend.nrow))
    } else { 
      # Assign legend key colours if identical samples between SVG and matrix have colors of "none".
      legend.col1 <- legend.col[ft.legend] # Only includes matching samples. 
      if (any(legend.col1=='none')) {
         n <- sum(legend.col1=='none'); col.all <- grDevices::colors()[grep('honeydew|aliceblue|white|gr(a|e)y', grDevices::colors(), invert=TRUE)]
         col.none <- col.all[seq(from=1, to=length(col.all), by=floor(length(col.all)/n))]
         legend.col1[legend.col1=='none'] <- col.none[seq_len(n)]
       }
       # Map legend colours to tissues.
       # Exclude transparent tissues.
       ft.legend <- setdiff(ft.legend, ft.trans) 
       leg.idx <- !duplicated(tis.path) & (tis.path %in% ft.legend)
       legend.col1 <- legend.col1[ft.legend]
       # Keep all colors in the original SVG.
       g.col <- legend.col[sub('__\\d+$', '', names(g.col))]
       names(g.col) <- tis.df # Resolves "legend.col['tissue'] is NA" by default.
       g.col[g.col=='none'] <- NA 
       # Make selected tissues transparent by setting their colours NA.
       if (!is.null(ft.trans)) g.col[sub('__\\d+$', '', tis.df) %in% ft.trans] <- NA
       # Copy colors across same numbered tissues. 
       g.col <- lapply(seq_along(g.col), function(x) {
         # In lapply each run must return sth.
         if (!is.na(g.col[x])) return(g.col[x])
         g.col0 <- legend.col1[sub('__\\d+$', '', names(g.col[x]))]
         if (!is.na(g.col0)) g.col[x] <- g.col0
         return(g.col[x])
         } 
       ); g.col <- unlist(g.col)
       # No matter the tissues in coordinate data frame are vector or factor, the coloring are decided by the named color vector (order of colors does not matter as long as names are right) in scale_fill_manual.
       scl.fil <- scale_fill_manual(values=g.col, breaks=tis.df[leg.idx], labels=tis.path[leg.idx], guide=guide_legend(title=NULL, ncol=legend.ncol, nrow=legend.nrow)) 
    }
    lgd.par <- theme(legend.position=legend.position, legend.direction=legend.direction, legend.background = element_rect(fill=alpha(NA, 0)), legend.key.size=unit(legend.key.size, "npc"), legend.text=element_text(size=legend.text.size), legend.margin=margin(l=0.1, r=0.1, unit='npc'))
    ## Add 'feature' and 'value' to coordinate data frame, since the resulting ggplot object is used in 'ggplotly'. Otherwise, the coordinate data frame is applied to 'ggplot' directly by skipping the following code.
    coord$gene <- k; coord$condition <- con; coord$value <- NA
    coord$feature <- sub('__\\d+$', '', coord$tissue)
    # Assign values to each tissue.
    col.na <- paste0(coord$feature, '__', coord$condition)
    idx1 <- col.na %in% colnames(gene); df0 <- coord[idx1, ]
    # df0$value <- unlist(gene[df0$gene[1], col.na[idx1]]) # Slow in large data.
    col.na1 <- unique(col.na[idx1])
    tab0 <- table(col.na[idx1])[col.na1]
    lis.v <- lapply(col.na1, function(i) { rep(gene[df0$gene[1], i], tab0[i][[1]]) } )
    df0$value <- unlist(lis.v) 
    coord[idx1, ] <- df0; coord$line.size <- coord$line.size+line.size
    # If "data" is not in ggplot(), g$data slot is empty. x, y, and group should be in the same aes().
    g <- ggplot(data=coord, aes(x=x, y=y, value=value, group=tissue, text=paste0('feature: ', feature, '\n', 'value: ', value)), ...)+geom_polygon(aes(fill=tissue), color=line.color, size=coord$line.size, linetype='solid')+scl.fil+theme(axis.text=element_blank(), axis.ticks=element_blank(), panel.grid=element_blank(), panel.background=element_rect(fill="white", colour="grey80"), plot.title=element_text(hjust=0.5, size=sub.title.size), legend.box.margin=margin(-20, 0, 2, 0, unit='pt'), plot.margin=margin(0.005, 0.005, 0.005, 0.005, "npc"), aspect.ratio = 1/aspect.ratio)+labs(x="", y="")+scale_y_continuous(expand=c(0.01, 0.01))+scale_x_continuous(expand=c(0.01, 0.01))+lgd.par
    # The aspect ratio should not be calculated by margins that are inferred from original width/height (mar.lb <- (1-w.h/w.h.max*0.99)/2). It works on single plot, but will squeeze the subplots in arrangeGrob/grid.arrange. Rather the "aspect ratio" in theme should be used, which will not squeeze subplots.
    # After theme(aspect.ratio) is set, change in only top or left margin will not distort the image. Rather width/height will scale propotionally, since the aspect ratio is fixed.
    # if (is.null(mar.lb)) g <- g+theme(plot.margin=margin(0.005, 0.005, 0.005, 0.005, "npc")) else g <- g+theme(plot.margin=margin(mar.lb[2], mar.lb[1], mar.lb[2], mar.lb[1], "npc"))
    if (con.na==FALSE) g.tit <- ggtitle(k) else g.tit <- ggtitle(paste0(k, "_", con)); g <- g+g.tit
    if (lgd==TRUE) {
      g <- g+theme(plot.margin=margin(0.005, 0.005, 0.2, 0, "npc"), plot.title=element_text(hjust=0.5, size=legend.plot.title.size))+ggtitle(legend.plot.title)
    }; return(g)

  }

  # Map colours to samples according to expression level.
  # Column names without '__' are not excluded so as to keep one-to-one match with color.dat.
  cname <- colnames(gene)
  cons <- gsub("(.*)(__)(.*)", "\\3", cname)
  sam.uni <- unique(gsub("(.*)(__)(.*)", "\\1", cname)); ft.trans <- make.names(ft.trans)

  g.lis.all <- NULL; for (k in ID) {
    # Match color key values to selected genes.
    color.dat <- NULL; for (i in gene[k, ]) { 
      ab <- abs(i-geneV); col.ind <- which(ab==min(ab))[1]; color.dat <- c(color.dat, cols[col.ind])
    }

    # idx <- grep("__", cname); c.na <- cname[idx]
    # Column names without '__' are also included so as to keep one-to-one match with color.dat.
    tis.col <- gsub("(.*)(__)(.*)", "\\1", cname)
    # Valid/target tissues.
    tis.col.uni <- unique(tis.col); tis.path.uni <- unique(tis.path)
    tis.col.path.idx <- tis.col.uni %in% tis.path.uni
    tis.tar <- tis.col.uni[tis.col.path.idx]
    # Un-related aSVG mapped to data.
    rematch.dif.svg <- is.list(lis.rematch) & length(tis.tar)==0
    if (rematch.dif.svg) {
      # Take the features in data that are assinged new aSVG features.
      tis.tar <- unlist(lapply(names(lis.rematch), function(i) {
        vec0 <- lis.rematch[[i]]
        if (!is.null(vec0)) if (all(vec0 %in% tis.path.uni)) return(i)
      }))
    }; if (length(tis.tar)==0) return(NULL)
    cname1 <- cname[grepl(paste0('^(', paste0(tis.tar,collapse='|'), ')__'), cname)]
    # Only conditions paired with valid tissues (have matching samples in data) are used. 
    con.vld <- gsub("(.*)(__)(.*)", "\\3", cname1); con.vld.uni <- unique(con.vld)
    na0 <- paste0(k, "_", con.vld.uni); cat('ggplot: ', k, ', ', sep = ''); g.lis <- lapply(con.vld.uni, g_list, ...); cat('\n')
    names(g.lis) <- na0; g.lis.all <- c(g.lis.all, g.lis)
  }; g.lgd <- g_list(con=NULL, lgd=TRUE, ...)
  return(list(g.lgd = g.lgd, g.lis.all = g.lis.all))
}


