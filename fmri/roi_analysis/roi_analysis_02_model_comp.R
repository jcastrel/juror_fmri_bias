#packages
library(dplyr)
library(lme4)
library(jtools)
library(ggplot2)
library(lmerTest)
library(magick)
library(gridBase)
library(sjPlot)
library(ggpubr)

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
# ANOVA comparing effects on fMRI
#########################################################

# Model 0
# Intercept - fMRI
m0 <- lmer(zstat ~ 1 + (1 | subjectID), data = data, REML = FALSE)
# summary
summ(m0, scale = TRUE, digits = 4)
#anova(m0)

# Model 1
# Victim Type - fMRI
m1 <- lmer(zstat ~ factor(victim_type) + (1 | subjectID), data = data, REML = FALSE)
# summary
summ(m1, scale = TRUE, confint = TRUE, digits = 4)
anova(m1)

# Model 2
# Category/seriousness - fMRI
m2 <- lmer(zstat ~ factor(category) + (1 | subjectID), data = data, REML = FALSE)
# summary
summ(m2, scale = TRUE, confint = TRUE, digits = 4)
anova(m2)

# Model 3
# Category/seriousness - fMRI
m3 <- lmer(zstat ~ factor(victim_type) + factor(category) + (1 | subjectID),
                     data = data, REML = FALSE)
# summary
summ(m3, scale = TRUE, confint = TRUE, digits = 4)
anova(m3)

# Model 4
# Category/seriousness - fMRI
m4 <- lmer(zstat ~ factor(victim_type)*factor(category) + (1 | subjectID),
                     data = data, REML = FALSE)
# summary
summ(m4, scale = TRUE, confint = TRUE, digits = 4)
anova(m4)


# MODEL COMPARISON/SELECTION #changed order of 1 & 2 to facilitate d.f. interpretation in output
fmri_crime_type_model_comparison <- anova(m0,m2,m1,m3,m4,test="Chisq")

#lmtest version
library(lmtest)
lmtest::lrtest(m0,m2,m1,m3,m4)


export_summs(m1,m2,m3,m4)
BIC(m0,m2,m1,m3,m4)

fmri_ct_mc_df <- as.data.frame(fmri_crime_type_model_comparison)
fmri_ct_mc_df$Model[fmri_ct_mc_df$npar == 3 ] <- "Intercept"
fmri_ct_mc_df$Model[fmri_ct_mc_df$npar == 4 ] <- "Statute"
fmri_ct_mc_df$Model[fmri_ct_mc_df$npar == 5 ] <- "Victim Type"
fmri_ct_mc_df$Model[fmri_ct_mc_df$npar == 6 ] <- "Victim + Statute"
fmri_ct_mc_df$Model[fmri_ct_mc_df$npar == 8 ] <- "Victim x Statute"

fmri_ct_mc_df <- fmri_ct_mc_df %>%
  mutate(BIC_delta = BIC - BIC[1])

panel_fmri <- ggplot(data = fmri_ct_mc_df, aes(x=Model,y=BIC_delta)) +
  geom_col() + th + ylab(expression(Delta~"BIC")) +
  geom_hline(yintercept=0, colour='grey') +
  scale_x_discrete(limits = c("Statute","Victim Type",
                              "Victim + Statute","Victim x Statute")) +
  theme(axis.text.x = element_text(angle = 45, hjust=0.95)) +
  theme(plot.margin=unit(c(25.5, 25.5, 25.5, 25.5),"points")) + ylim(-40,5)

#table
tab_model(m0,m1,m2,m3,m4, show.aic = TRUE, show.ci = FALSE,
          collapse.se = TRUE, p.style = "stars")



#########################################################
# ANOVA comparing victim type effects on PC1 (Behavior)
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

#m0
m0_behavior <- lm(PC1 ~ 1, data = behavior_data)
summ(m0_behavior, scale = TRUE, confint = TRUE, digits = 4)
anova(m0_behavior)

#m1
m1_behavior <- lm(PC1 ~ factor(victim_type), data = behavior_data)
summ(m1_behavior, scale = TRUE, confint = TRUE, digits = 4)
anova(m1_behavior)

#m2
m2_behavior <- lm(PC1 ~ factor(category), data = behavior_data)
summ(m2_behavior, scale = TRUE, confint = TRUE, digits = 4)
anova(m2_behavior)

#m3
m3_behavior <- lm(PC1 ~ factor(victim_type) + factor(category), data = behavior_data)
summ(m3_behavior, scale = TRUE, confint = TRUE, digits = 4)
anova(m3_behavior)

#m4
m4_behavior <- lm(PC1 ~ factor(victim_type)*factor(category), data = behavior_data)
summ(m4_behavior, scale = TRUE, confint = TRUE, digits = 4)
anova(m4_behavior)

# MODEL COMPARISON/SELECTION #changed order of 1 & 2 to facilitate d.f. interpretation in output
behavior_crime_type_model_comparison <- anova(m0_behavior,m2_behavior,
                                              m1_behavior,m3_behavior,
                                              m4_behavior,test="Chisq")

#lmtest version
library(lmtest)
lmtest::lrtest(m0_behavior,m2_behavior,m1_behavior,m3_behavior,m4_behavior)


export_summs(m0_behavior,m1_behavior,m2_behavior,m3_behavior,m4_behavior)
BIC(m0_behavior,m2_behavior,m1_behavior,m3_behavior,m4_behavior)

behav_ct_mc_df <- as.data.frame(BIC(m0_behavior,m1_behavior,m2_behavior,m3_behavior,m4_behavior))

behav_ct_mc_df$Model[behav_ct_mc_df$df == 2 ] <- "Intercept"
behav_ct_mc_df$Model[behav_ct_mc_df$df == 3 ] <- "Statute"
behav_ct_mc_df$Model[behav_ct_mc_df$df == 4 ] <- "Victim Type"
behav_ct_mc_df$Model[behav_ct_mc_df$df == 5 ] <- "Victim + Statute"
behav_ct_mc_df$Model[behav_ct_mc_df$df == 7 ] <- "Victim x Statute"

behav_ct_mc_df <- behav_ct_mc_df %>%
  mutate(BIC_delta = BIC - BIC[1])

panel_behavior<- ggplot(data = behav_ct_mc_df, aes(x=Model,y=BIC_delta)) +
  geom_col() + th + ylab(expression(Delta~"BIC")) +
  geom_hline(yintercept=0, colour='grey') +
  scale_x_discrete(limits = c("Statute","Victim Type",
                              "Victim + Statute","Victim x Statute")) +
  theme(axis.text.x = element_text(angle = 45, hjust=0.95)) +
  theme(plot.margin=unit(c(25.5, 25.5, 25.5, 25.5),"points")) + ylim(-40,5)

#table
tab_model(m0,m1,m2,m3,m4, show.aic = TRUE, show.ci = FALSE,
          collapse.se = TRUE, p.style = "stars")

###################

combo_plot <- ggarrange(panel_behavior,panel_fmri,labels=c('A','B'),
                        font.label = list(size = 20),ncol = 2,
                        align = 'hv',heights = c(1,1))

ggsave('plots/supp_fig_X.pdf', plot=combo_plot,
       width=10, height=5, units='in', useDingbats=FALSE)





