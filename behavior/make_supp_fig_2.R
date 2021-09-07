# supplementary figure 2 from the paper
library(tidyverse)
library(grid)
library(gridExtra)
library(gtable)
library(gridBase)

source('ggplot_setup.R')
load('data/stan_postprocess_2v_t.rdata')
effects <- effects %>% filter(group =='mri')
#dat <- dat %>% filter(group |'mri')


############### Panel A: Punishment and case strength effect correlations ##################################
load('data/stan_postprocess_2v_t.rdata')

effects <- effects %>% filter(group =='mri' | group == 'mturk')

panel_A <- ggplot(data=(effects %>% filter(grepl('Omega', variable), evidence=='baseline'))) +
  geom_hline(yintercept=0, colour='grey') +
  geom_pointrange(aes(x=evidence, y=X50., ymin=X2.5., ymax=X97.5., color=group), 
                  position=position_dodge(width = 0.5)) + 
  xlab('Type') + ylab('\nCase Strength /\nPunishmnet Correlation') +
  group_color_scale +
  evidence_plus_baseline_x_axis +
  labs(title="A", size=rel(3)) +
  th + 
  theme(
    legend.position=c(0.8, 0.8)
  ) + theme(text=element_text(family="Helvetica"),
            plot.title = element_text(family="Helvetica",face="bold"))

############### Panels B & C: PCA  ##################################
#run PCA on weights for case strength and punishment
#fmri sample
#read data
fmri_data <- read.csv(file="data/scenario_effects_fmri_sample.csv",
                      header=TRUE, sep = ",",
                      na.strings=c("","NA"))
#pca
fmri_data.pca <- prcomp(fmri_data[,c(2:3)],
                        center = TRUE,
                        scale. = TRUE)
#summary
summary(fmri_data.pca)
#loadings
fmri_data.pca$rotation
#scores
fmri_data.pca$x
#save results
write.csv(fmri_data.pca$x,
          file = "pca_loadings-fmri.csv")

#plotting
fmri_data$case_strength_mean_scaled<- scale(fmri_data$case_strength_mean)
fmri_data$punishment_mean_scaled<- scale(fmri_data$punishment_mean)

mean_cs = mean(fmri_data$case_strength_mean)
sd_cs = sd(fmri_data$case_strength_mean)

mean_pun = mean(fmri_data$punishment_mean)
sd_pun = sd(fmri_data$punishment_mean)

# Self-defined formatting to go back to raw values
z_to_orig_x <- function(x) {
  m <- mean_cs
  sd <- sd_cs
  o <- (x*sd)+m
  o <- round(o,0)
  lab <- sprintf('%d',o) # Format the strings as HH:MM:SS
}

z_to_orig_y <- function(x) {
  m <- mean_pun
  sd <- sd_pun
  o <- (x*sd)+m
  o <- round(o,0)
  lab <- sprintf('%d',o) # Format the strings as HH:MM:SS
}

#plot
g <- ggplot(data = fmri_data, mapping = aes(x = case_strength_mean_scaled, y = punishment_mean_scaled))
g <- g + geom_point(size=5,color='#fdaa4c')  # alpha b/c of overplotting
#g <- g + geom_smooth(method = "lm")  # just for comparsion
#g <- g + coord_fixed()  # otherwise, the angles of vectors are off
corre <- cor(x = fmri_data$case_strength_mean_scaled, y = fmri_data$punishment_mean_scaled, method = "pearson")  # calculate correlation, must be spearman b/c of measurement
matrix <- matrix(c(1, corre, corre, 1), nrow = 2)  # make this into a matrix
eigen <- eigen(matrix)  # calculate eigenvectors and values
eigen$vectors.scaled <- eigen$vectors %*% diag(sqrt(eigen$values))  
# scale eigenvectors to length = square-root
# as per http://stats.stackexchange.com/questions/9898/how-to-plot-an-ellipse-from-eigenvalues-and-eigenvectors-in-r
# g <- g + stat_ellipse(type = "norm")
# g <- g + stat_ellipse(type = "t")
# add ellipse, though I am not sure which is the adequate type
# as per https://github.com/hadley/ggplot2/blob/master/R/stat-ellipse.R
g <- g + geom_abline(intercept = 0, slope = eigen$vectors.scaled[1,1], colour = "#fdaa4c")  # add slope for pc1
g <- g + geom_abline(intercept = 0, slope = eigen$vectors.scaled[1,2], colour = "black")  # add slope for pc2
g <- g + scale_x_continuous(labels = z_to_orig_x) + scale_y_continuous(labels = z_to_orig_y) +
  xlab('Case Strength') + ylab('Punishment') + labs(title="B", size=rel(3))
panel_B <- g + th +
  theme(text=element_text(family="Helvetica"),
        plot.title = element_text(family="Helvetica",face="bold"))
####################

#run PCA on weights for case strength and punishment
#mturk sample
#read data
mturk_data <- read.csv(file="data/scenario_effects_mturk_sample.csv",
                       header=TRUE, sep = ",",
                       na.strings=c("","NA"))
#pca
mturk_data.pca <- prcomp(mturk_data[,c(2:3)],
                         center = TRUE,
                         scale. = TRUE)
#summary
summary(mturk_data.pca)
#loadings
mturk_data.pca$rotation
#scores
mturk_data.pca$x
#save results
write.csv(mturk_data.pca$x,
          file = "pca_loadings-mturk.csv")

#plotting
mturk_data$case_strength_mean_scaled<- scale(mturk_data$case_strength_mean)
mturk_data$punishment_mean_scaled<- scale(mturk_data$punishment_mean)

mean_cs = mean(mturk_data$case_strength_mean)
sd_cs = sd(mturk_data$case_strength_mean)

mean_pun = mean(mturk_data$punishment_mean)
sd_pun = sd(mturk_data$punishment_mean)

# Self-defined formatting to go back to raw values
z_to_orig_x <- function(x) {
  m <- mean_cs
  sd <- sd_cs
  o <- (x*sd)+m
  o <- round(o,0)
  lab <- sprintf('%d',o) # Format the strings as HH:MM:SS
}

z_to_orig_y <- function(x) {
  m <- mean_pun
  sd <- sd_pun
  o <- (x*sd)+m
  o <- round(o,0)
  lab <- sprintf('%d',o) # Format the strings as HH:MM:SS
}

#plot
g <- ggplot(data = mturk_data, mapping = aes(x = case_strength_mean_scaled, y = punishment_mean_scaled))
g <- g + geom_point(size=5,color='#0656A3')  # alpha b/c of overplotting
#g <- g + geom_smooth(method = "lm")  # just for comparsion
#g <- g + coord_fixed()  # otherwise, the angles of vectors are off
corre <- cor(x = mturk_data$case_strength_mean_scaled, y = mturk_data$punishment_mean_scaled, method = "pearson")  # calculate correlation, must be spearman b/c of measurement
matrix <- matrix(c(1, corre, corre, 1), nrow = 2)  # make this into a matrix
eigen <- eigen(matrix)  # calculate eigenvectors and values
eigen$vectors.scaled <- eigen$vectors %*% diag(sqrt(eigen$values))  
# scale eigenvectors to length = square-root
# as per http://stats.stackexchange.com/questions/9898/how-to-plot-an-ellipse-from-eigenvalues-and-eigenvectors-in-r
# g <- g + stat_ellipse(type = "norm")
# g <- g + stat_ellipse(type = "t")
# add ellipse, though I am not sure which is the adequate type
# as per https://github.com/hadley/ggplot2/blob/master/R/stat-ellipse.R
g <- g + geom_abline(intercept = 0, slope = eigen$vectors.scaled[1,1], colour = "#0656A3")  # add slope for pc1
g <- g + geom_abline(intercept = 0, slope = eigen$vectors.scaled[1,2], colour = "black")  # add slope for pc2
g <- g + scale_x_continuous(labels = z_to_orig_x) + scale_y_continuous(labels = z_to_orig_y) +
  xlab('Case Strength') + ylab('Punishment') + labs(title="C", size=rel(3))
panel_C <- g + th +
  theme(text=element_text(family="Helvetica"),
        plot.title = element_text(family="Helvetica",face="bold"))

############### Combine into a single figure ##################################
# make a list of panels
plt_list <- list(panel_A, panel_B, panel_C)

# convert to grobs
grob_list <- lapply(plt_list, ggplotGrob)

# make sure axes align
max_heights <- do.call(unit.pmax, lapply(grob_list, function(x) {x$heights}))
grob_list <- lapply(grob_list, function(x) {x$heights <- max_heights; x})

# arrange with differing widths
lay <- rbind(c(1, 2, 3))
plt_all <- do.call(arrangeGrob, c(grob_list, ncol=3, layout_matrix=list(lay),
                                  widths=list(c(2.0, 3.0, 3.0))))

# save to disk
ggsave('figs/supp_fig_2.pdf', plot=plt_all, width=13, height=6, units='in', useDingbats=FALSE)
