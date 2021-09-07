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

#set seed (for reproducible effects)
set.seed(1000)

#theme
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

#open fmri parameter estimates data
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


#########################################################
# ANOVA comparing statute category effects on PC1 (Behavior)
#########################################################

# grab behavior data
behavior <- read.csv(file="../../behavior/data/scenario_classification.csv",
                     header=TRUE,
                     sep = ",",
                     na.strings=c("","NA"))

behavior <- within(behavior, {
  victim_type <- factor(JRL,
                        levels = 0:2,
                        labels = c('Victimless',
                                   'Loss of Property',
                                   'Injury or Loss of Life'))
  category <- factor(category)
})

#grab pca data
pca <- read.csv(file="../../behavior/pca_loadings-fmri.csv",
                header=TRUE,
                sep = ",",
                na.strings=c("","NA"))
pca <- within(pca, {
  scenario <- factor(X)
})

#merge dataframes
behavior_data <- merge(behavior, pca, by='scenario')


category_type_behavior_PC1 <- lm(PC1 ~ factor(category), data = behavior_data)
summ(category_type_behavior_PC1, scale = TRUE, confint = TRUE, digits = 4)

anova(category_type_behavior_PC1)

#effect plot version
behavior_category_type_plot_PC1 <- effect_plot(category_type_behavior_PC1,
                                               pred=category,
                                               cat.interval.geom = "linerange",
                                               confint = TRUE) + th +
  geom_hline(yintercept=0, colour='grey') +
  theme(text=element_text(family="Helvetica")) +
  labs(y = "Mean crime-type bias", x = "Crime Type") +
  theme(plot.margin=unit(c(25.5, 25.5, 25.5, 25.5),"points")) +  ylim(-2.5,2.5) +
  scale_x_discrete(limits = c("Misdemeanor", "Felony"))


#########################################################
# ANOVA comparing statute category effects on fMRI
#########################################################

#Category/seriousness - fMRI
fmri_seriousness <- lmerTest::lmer(zstat ~ factor(category) + (1 | subjectID), data = data)
#summary
summ(fmri_seriousness, scale = TRUE, confint = TRUE, digits = 4)
anova(fmri_seriousness)

#effect plot
fmri_category_type_plot <- effect_plot(fmri_seriousness,
                                       pred=category,
                                       cat.interval.geom = "linerange",
                                       confint = TRUE) + th +
  geom_hline(yintercept=0, colour='grey') +
  theme(text=element_text(family="Helvetica")) + th +
  labs(y = "Crime-type bias fMRI activation", x = "Crime Type") +
  theme(plot.margin=unit(c(25.5, 25.5, 25.5, 5.5),"points")) + ylim(-0.35,0.35) +
  scale_x_discrete(limits = c("Misdemeanor", "Felony"))



################################################################
#merge plots
panels <- ggarrange(behavior_category_type_plot_PC1,fmri_category_type_plot, ncol = 1, nrow=2,
                    labels=c('A','B'),font.label = list(size = 20), align='v',widths = c(1,1))

ggsave('plots/supp_fig_9.pdf', plot=panels,
       width=8, height=8, units='in', useDingbats=FALSE)

#################################################################


