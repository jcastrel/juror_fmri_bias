## Plot scenario-wise fMRI parameter estimates

#packages
library(dplyr)
library(lme4)
library(jtools)
library(ggplot2)
library(ggpubr)
library(lmerTest)
library(magick)
library(gridBase)

# set seed (for reproducible effects)
set.seed(1000)

# theme
th <- theme_minimal() +
  theme(plot.background = element_rect(fill = 'white', color = 'white'),
        plot.title = element_text(face = 'bold', colour = 'black', size = 18),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'white',color = 'white'),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(colour = "black", size = 16),
        axis.title.y = element_text(colour = "black", size = 16),
        axis.text=element_text(size = 16),
        axis.text.x = element_text(colour = "black", size = 16),
        axis.text.y = element_text(colour = "black", size = 16))

# open fmri parameter estimates data
data <- read.csv(file="./subject_scenario_mean_zstat.csv",
                 header=TRUE,
                 sep = ",",
                 na.strings=c("","NA"))

data <- within(data, {
  victim_type <- factor(victim_type,
                        levels = 0:2,
                        labels = c('Victimless',
                                   'Loss of Property',
                                   'Injury or Loss of Life'))
  subjectID <- factor(subjectID)
})

#################################################################
# fMRI Social Map Overlays - Fig 4A
#################################################################
# social maps for panel
social_maps <- image_read_pdf('../decoding_analysis/social_maps/Fig_4a.pdf') #8 x 5 fig
social_maps_trimmed <- image_trim(social_maps)

panel_A <- ggplot() +
  annotation_custom(grid::rasterGrob(social_maps_trimmed,interpolate=TRUE)) +
  theme(text=element_text(family="Helvetica"),
        plot.title = element_blank()) + 
  theme(panel.border = element_rect(colour = "white", fill=NA, size=0)) +
  theme(panel.background = element_rect(colour = "white", fill=NA, size=0)) +
  theme(axis.line=element_blank())+
  theme(plot.margin=unit(c(25.5, 25.5, 0, 25.5),"points"))


#################################################################
# ANOVA comparing victim type effects on PC1 (Behavior) - Fig 4B
#################################################################

# grab behavior data
behavior <- read.csv(file="../../behavior/data/scenario_classification.csv",
                     header=TRUE,sep = ",",na.strings=c("","NA"))

behavior <- within(behavior, {
  victim_type <- factor(JRL,levels = 0:2,
                        labels = c('Victimless','Loss of Property',
                                   'Injury or Loss of Life'))
  category <- factor(category)
})

#grab pca data
pca <- read.csv(file="../../behavior/pca_loadings-fmri.csv",
                header=TRUE,sep = ",",na.strings=c("","NA"))
pca <- within(pca, {
  scenario <- factor(X)
})

#merge dataframes
behavior_data <- merge(behavior, pca, by='scenario')

#PC1
victim_type_behavior_PC1 <- lm(PC1 ~ factor(victim_type), data = behavior_data) #minus one to avoid coding victimless as intercept
summ(victim_type_behavior_PC1, scale = TRUE, confint = TRUE, digits = 4)

anova(victim_type_behavior_PC1)

# FIGURE 4 PANEL B #
#effect plot
behavior_victim_type_plot_PC1 <- effect_plot(victim_type_behavior_PC1,
                                             pred=victim_type,
                                             cat.interval.geom = "linerange",
                                             confint = TRUE) + th +
  geom_hline(yintercept=0, colour='grey') +
  theme(text=element_text(family="Helvetica")) + th +
  labs(y = "Crime-type Bias") +
  theme(plot.margin=unit(c(5.5, 5.5, 25.5, 25.5),"points")) + ylim(-2.5,2.5) +
  theme(axis.text.x = element_text(angle = 45, hjust=0.95)) +
  theme(axis.title.x = element_blank())


#################################################################
# ANOVA comparing victim type effects on fMRI - Fig 4C
#################################################################

#Victim Type - fMRI
fmri_victim_type <- lmerTest::lmer(zstat ~ factor(victim_type) + (1 | subjectID), data = data)
#summary
summ(fmri_victim_type, scale = TRUE, confint = TRUE, digits = 4)

anova(fmri_victim_type)

# FIGURE 4 PANEL C #

#effect plot
fmri_victim_type_plot <- effect_plot(fmri_victim_type,
                                     pred=victim_type,
                                     cat.interval.geom = "linerange",
                                     confint = TRUE) + th +
  geom_hline(yintercept=0, colour='grey') +
  theme(text=element_text(family="Helvetica")) + th +
  labs(y = "Crime-type bias fMRI activation", x = "Crime Type") +
  theme(plot.margin=unit(c(5.5, 25.5, 5.5, 0),"points")) + ylim(-0.45,0.45) +
  theme(axis.text.x = element_text(angle = 45, hjust=0.95)) +
  theme(axis.title.x = element_blank())



################################################################
#merge plots

combo_plot <- ggarrange(panel_A,
                        ggarrange(behavior_victim_type_plot_PC1,fmri_victim_type_plot,
                                  labels=c('B','C'), font.label = list(size = 20), hjust = 1.0,
                                  align = 'hv', widths = c(1,1), heights = c(1,1), ncol = 2),
                        nrow = 2,labels = "A", font.label = list(size = 20), heights=c(1,1),
                        widths=c(1,1), align = 'hv')

# combo_plot <- ggarrange(panel_A,
#                         ggarrange(behavior_victim_type_plot_PC1,
#                                   fmri_victim_type_plot,
#                                   labels=c('B','C'),hjust = 0.6,vjust = c(1.5,-0.5),
#                                   font.label = list(size = 20),
#                                   align = 'hv',heights = c(1,1), nrow = 2),
#                         ncol = 2,labels = "A",
#                         font.label = list(size = 20), widths=c(3,1),
#                         vjust = 1.5, hjust = -0.25, align = 'v')

ggsave('plots/main_fig_4.pdf', plot=combo_plot,
       width=10, height=10, units='in', useDingbats=FALSE)
