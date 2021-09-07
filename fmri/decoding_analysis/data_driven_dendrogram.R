
#packages
library(ggplot2)
library(reshape)
library(dplyr)

#open data
data <- read.csv(file="decoding_output/decoding_results_topics_thresholded.csv",
                 header=TRUE, sep = ",", stringsAsFactors=FALSE)

data <- read.csv(file="decoding_output/decoding_results_topics_unthresholded.csv",
                 header=TRUE, sep = ",", stringsAsFactors=FALSE)


#social-moral-affective rows only (topic # plus 1)
data <- data %>%
  slice(c(101, 146, 155, 113, 136, 127, 140, 181))

#change index
rownames(data) <- data$topic_name


#only one column
data <- data %>%
  select(scenario)



# Compute distances and hierarchical clustering
dd <- dist(scale(data), method = "euclidean")

hc <- hclust(dd, method = "ward.D2")
hc <- hclust(dd, method = "single")
hc <- hclust(dd, method = "complete")
hc <- hclust(dd, method = "average")


#plot
plot(hc, hang = 0.1, 
     main = "Cluster dendrogram", sub = NULL,
     xlab = NULL, ylab = "Height")

