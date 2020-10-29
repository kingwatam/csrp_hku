rm(list=ls())
graphics.off()
# par(mar=c(0,0,0,0)) # set plot margins to 0
if (substring(getwd(),2,2) == ":") {
  setpath <- "/MEGAsync/Work/RA HKU/CSRP"
} else {
  setpath <- ""
}
setwd(sprintf("~%s", setpath))
source("helper_functions.R")

setwd(sprintf("~%s/school_clusters", setpath))

library(jsonlite)
library(openxlsx)
library(magrittr) # pipe 

sch <- openxlsx::read.xlsx("school_clustering_updated.xlsx" )
# output <- openxlsx::read.xlsx("output.xlsx" )
cc <- openxlsx::read.xlsx("cc_2007_2016.xlsx")

# output <- subset(output, select = c('cn_name', 'resources', 't_quality',
#                                  'ts_ratio', 'edu_quality', 'stud_support', 'acad_results',
#                                  'extra_results', 'sch_morale',
#                                  'study_atm', 'stu_conduct', 'teach_style', 'teach_morale', 'sch_manage',
#                                  'sch_ach'))

normalize <- function(x){
  return((x- min(x)) /(max(x)-min(x)))
}
standardize <- function(x){
  return((x - mean(x)) / sd(x))
}

# k-means clustering ----
# ouput <- output[complete.cases(output),]
# scores <- as.data.frame(lapply(sch[2:15], normalize))
# scores <- as.data.frame(lapply(scores, standardize))
# scores <- scale(sch[2:15])

test <- prcomp(sch[2:15], scale = TRUE) # standardization and normalization seem to produce the same clusters, whereas raw values produce slightly different clusters
predict_test <- as.data.frame(predict(test, newdata = sch[2:15])[, 1:14]) # using only the first 2 components

# elbow plot
factoextra::fviz_nbclust(predict_test[1:3], kmeans, method = "wss")
# visualise clusters
km <- kmeans(predict_test[1:3], centers = 8, nstart = 25)
km$cluster <- car::recode(km$cluster, "
1 = 5;
2 = 3;
3 = 8;
4 = 1;
5 = 2;
6 = 6;
7 = 7;
8 = 4
")
sch$km_cluster <- km$cluster
factoextra::fviz_cluster(km, c_test[1:3], choose.vars = c("PC1", "PC2"), stand = FALSE, geom = "point")

# km7 <- kmeans(predict_test[1:3], centers = 7, nstart = 25)
# km7$cluster <- car::recode(km7$cluster, "
# 1 = 6;
# 2 = 3;
# 3 = 5;
# 4 = 4;
# 5 = 7;
# 6 = 1;
# 7 = 2
# ")
# sch$km_cluster7 <- km7$cluster

# km6 <- kmeans(predict_test[1:3], centers = 6, nstart = 25)
# km6$cluster <- car::recode(km6$cluster, "
# 1 = 6;
# 2 = 3;
# 3 = 4;
# 4 = 2;
# 5 = 5;
# 6 = 1
# ")
# sch$km_cluster6 <- km6$cluster

mean_max_km <- data.frame('km_cluster' = (1:max(sch$km_cluster)))
for (var_name in names(sch)[2:15]){
  for (j in 1:max(sch$km_cluster)){
    mean_max_ratio <- mean(sch[which(sch$km_cluster==j), var_name])/max(sch[var_name])
    eval_(sprintf('mean_max_km$%s[which(mean_max_km$km_cluster==j)] <- mean_max_ratio', var_name))
  }
  # print(mean(sch$resources[which(sch$cluster==i)]))
}

colnames(mean_max_km)[2:15] <- c('resources & facilities', 'teaching quality', 'student-teacher ratio',
                              'education quality', 'student support', 'academic performance',	'extracurricular performance',
                              'school ethos', 'learning atmosphere', 'student conduct', 'teaching style', 'teacher ethics', 'management style', 'school reputation')

# rownames(mean_max_km) <- paste("Cluster" , mean_max_km$cluster , sep="-")
mean_max_km <- rbind(rep(1,ncol(mean_max_km)) , rep(0,ncol(mean_max_km)) , mean_max_km)
fmsb::radarchart(mean_max_km[2:15], axistype=1 , 
                 #custom the grid
                 cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,1,0.25), cglwd=0.8,
                 #custom labels
                 vlcex=1
                 )
legend(x=1.5, y=1, legend = mean_max_km$km_cluster[3:(2+max(sch$km_cluster))], col = c(1:max(sch$km_cluster)), lty = c(1:max(sch$km_cluster)), lwd = 2)

# Gaussian mixture model-based clustering ----
library(mclust)
# gmm <- Mclust(as.data.frame(lapply(sch[2:15], scale)), G = 5)

# scores <- as.data.frame(lapply(sch[2:15], normalize))
# scores <- scale(sch[2:15])
# scores <- as.data.frame(lapply(scores, standardize))

test <- prcomp(sch[2:15], scale = TRUE) # standardization and normalization seem to produce the same clusters, whereas raw values produce slightly different clusters
predict_test <- as.data.frame(predict(test, newdata = sch[2:15])[, 1:14]) 

gmm <- Mclust(predict_test[1:3] # using only the first 3 components
              , prior=priorControl()
              , modelNames = 'EII'
              # , modelNames = c('EEI', 'EII')
              # , modelNames = c('EII', 'VII', 'EEI', 'VEI', 'EVI', 'VVI', 'EEE', 'VEE', 'EVE', 'EEV', 'EVV')
              # , modelNames = c('EII', 'VII', 'EEI', 'VEI', 'EVI', 'VVI', 'EEE', 'VEE', 'EVE', 'EEV', 'EVV', 'VEV')
              # , control=emControl(eps=0, tol=c(1e-3 , 1e-3))
              , G = 1:9
              ) 

# library(clustMD) # too slow & not really working
# gmm_md <- matrix(NA, nrow = 9, ncol = 6)
# for (method in c("EII", "VII", "EEI", "VEI", "EVI", "VVI")){
#   for (g in c(1:9)){
#     gmm_md[g, which(c("EII", "VII", "EEI", "VEI", "EVI", "VVI") == method)] <- clustMD(sch[2:15], G = g, CnsIndx = 0, OrdIndx = 14, Nnorms = 100, MaxIter = 500,
#                       model = method, scale = FALSE, startCL = 'mclust')$BIChat
#   }
# }

# library(Rmixmod)
# gmm_rmix <- mixmodCluster(sch[2:15], nbCluster = 1:9,  criterion= "BIC", model = mixmodGaussianModel())

plot(gmm, what = 'BIC',
     # legendArgs =  NULL
     legendArgs = list(x = "bottomright", ncol = 7, nrow = 0)
     ) 

# # EII
# gmm$classification <- car::recode(gmm$classification, "
# 1 = 2;
# 2 = 4;
# 3 = 6;
# 4 = 3;
# 5 = 5;
# 6 = 7;
# 7 = 1
# ")

# VEV
gmm$classification <- car::recode(gmm$classification, "
1 = 2;
2 = 5;
3 = 3;
4 = 1;
5 = 4
")

factoextra::fviz_cluster(gmm, predict_test[1:3], choose.vars = c("PC1", "PC3"), stand = FALSE, geom = "point")
sch$gmm_cluster <- gmm$classification

# # VVV
# sch$gmm_cluster_VVV <- car::recode(sch$gmm_cluster_VVV, "
# 1 = 3;
# 2 = 2;
# 3 = 1;
# 4 = 4
# ")

# # VEV
# sch$gmm_cluster_VEV <- car::recode(sch$gmm_cluster_VEV, "
# 1 = 2;
# 2 = 4;
# 3 = 3;
# 4 = 5;
# 5 = 1
# ")

mean_max_gmm <- data.frame('gmm_cluster' = (1:max(sch$gmm_cluster)))
for (var_name in names(sch)[2:15]){
  for (j in 1:max(sch$gmm_cluster)){
    mean_max_ratio <- mean(sch[which(sch$gmm_cluster==j), var_name])/max(sch[var_name])
    eval_(sprintf('mean_max_gmm$%s[which(mean_max_gmm$gmm_cluster==j)] <- mean_max_ratio', var_name))
  }
  # print(mean(sch$resources[which(sch$cluster==i)]))
}

colnames(mean_max_gmm)[2:15] <- c('resources & facilities', 'teaching quality', 'student-teacher ratio',
                                 'education quality', 'student support', 'academic performance',	'extracurricular performance',
                                 'school ethos', 'learning atmosphere', 'student conduct', 'teaching style', 'teacher ethics', 'management style', 'school reputation')

mean_max_gmm <- rbind(rep(1,ncol(mean_max_gmm)) , rep(0,ncol(mean_max_gmm)) , mean_max_gmm) # add minimum and maximum values for radarchart
fmsb::radarchart(mean_max_gmm[2:15], axistype=1, 
                 #custom the grid
                 cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,1,0.25), cglwd=0.8,
                 #custom labels
                 vlcex=1,
                 pcol = c('black', 'red', 'darkgreen', 'blue', 'purple', 'orange', 'brown')
)
legend(x=1.5, y=1, legend = mean_max_gmm$gmm_cluster[3:(2+max(sch$gmm_cluster))]
       , col = c('black', 'red', 'darkgreen', 'blue', 'purple', 'orange', 'brown')
       , lty = c(1:max(sch$gmm_cluster)), lwd = 2, ncol = 1)


# merge with cc data ----
cc <- merge(cc, unique(sch)[, c("cn_name", "banding","type", "num_stud", 
                                "km_cluster", "gmm_cluster")], by=c("cn_name"), all.x = TRUE)

cc$rank <- car::recode(cc$type, "
0 = 1;
1 = 5;
2 = 3;
3 = 2;
4 = 4;
5 = 6
")

write_excel("cc_2007_2016_updated.xlsx", cc)

sch$rank <- car::recode(sch$type, "
0 = 1;
1 = 5;
2 = 3;
3 = 2;
4 = 4;
5 = 6
")

num_stud_cluster <- aggregate(num_stud ~ rank , sch, FUN=base::sum)

# calculte differential between our estimate and observed number of students
# all local schools (447 vs 472)
# 29/31*20574+356/359*233630+2/2*1221+59/60*45601+1/20*6994 = 297337
# 345061/297337 = 1.1605
# local schools excluding caput & private (444 vs 450)
# 29/31*20574+356/359*233630+59/60*45601= 295765
# (345061-(815+668)-931)/295765 = 1.1585

# HDDC (selected K seems to large at 8/9 instead of 4-6)----
library(HDclassif)
set.seed(61696) # a 5-digit random number generated from random.org with a range (1:99999), set as seed
hddc <- hddc(sch[2:15], K = 1:9, 
             # model = 'All',
             d_select = 'BIC',  itermax = 10000)

sch$hddc_cluster <- hddc$class



mean_max_hddc <- data.frame('hddc_cluster' = (1:max(sch$hddc_cluster)))
for (var_name in names(sch)[2:15]){
  for (j in 1:max(sch$hddc_cluster)){
    mean_max_ratio <- mean(sch[which(sch$hddc_cluster==j), var_name])/max(sch[var_name])
    eval_(sprintf('mean_max_hddc$%s[which(mean_max_hddc$hddc_cluster==j)] <- mean_max_ratio', var_name))
  }
  # print(mean(sch$resources[which(sch$cluster==i)]))
}

colnames(mean_max_hddc)[2:15] <- c('resources & facilities', 'teaching quality', 'student-teacher ratio',
                                  'education quality', 'student support', 'academic performance',	'extracurricular performance',
                                  'school ethos', 'learning atmosphere', 'student conduct', 'teaching style', 'teacher ethics', 'management style', 'school reputation')

mean_max_hddc <- rbind(rep(1,ncol(mean_max_hddc)) , rep(0,ncol(mean_max_hddc)) , mean_max_hddc) # add minimum and maximum values for radarchart
fmsb::radarchart(mean_max_hddc[2:15], axistype=1 , 
                 #custom the grid
                 cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,1,0.25), cglwd=0.8,
                 #custom labels
                 vlcex=1)

legend(x=1.6, y=1, legend = mean_max_hddc$hddc_cluster[3:(2+max(sch$hddc_cluster))], col = c(1:max(sch$hddc_cluster)), lty = c(1:max(sch$hddc_cluster)), lwd = 2)


# Dirichlet process mixture model ----
library(dirichletprocess)
library(ggplot2)
# dp <- DirichletProcessMvnormal(lapply(predict_test, scale))
# ggplot(predict_test, aes(x=PC1, y=PC2)) + geom_point()

scores <- as.data.frame(lapply(sch[2:15], normalize))
test <- prcomp(scores) # standardization and normalization seem to produce the same clusters, whereas raw values produce slightly different clusters
predict_test <- as.data.frame(predict(test, newdata = scores)[, 1:14]) # using only the first 2 components

predict_test_scaled <- scale(predict_test[1:3])
dpmm <- DirichletProcessMvnormal(as.matrix(predict_test_scaled))
dp <- Fit(dpmm, 50, progressBar=TRUE)
plot(predict_test$PC1, predict_test$PC2, col = dp$clusterLabels, pch = 19)
plot(dp)
pairs(predict_test_scaled, col=dp$clusterLabels)

sch$dpmm_cluster <- dp$clusterLabels
mean_max_dpmm <- data.frame('dpmm_cluster' = (1:max(sch$dpmm_cluster)))
for (var_name in names(sch)[2:15]){
  for (j in 1:max(sch$dpmm_cluster)){
    mean_max_ratio <- mean(sch[which(sch$dpmm_cluster==j), var_name])/max(sch[var_name])
    eval_(sprintf('mean_max_dpmm$%s[which(mean_max_dpmm$dpmm_cluster==j)] <- mean_max_ratio', var_name))
  }
  # print(mean(sch$resources[which(sch$cluster==i)]))
}

colnames(mean_max_dpmm)[2:15] <- c('resources & facilities', 'teaching quality', 'student-teacher ratio',
                                  'education quality', 'student support', 'academic performance',	'extracurricular performance',
                                  'school ethos', 'learning atmosphere', 'student conduct', 'teaching style', 'teacher ethics', 'management style', 'school reputation')

mean_max_dpmm <- rbind(rep(1,ncol(mean_max_dpmm)) , rep(0,ncol(mean_max_dpmm)) , mean_max_dpmm) # add minimum and maximum values for radarchart
fmsb::radarchart(mean_max_dpmm[2:15], axistype=1 , 
                 #custom the grid
                 cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,1,0.25), cglwd=0.8,
                 #custom labels
                 vlcex=1
)
legend(x=1.6, y=1, legend = mean_max_dpmm$dpmm_cluster[3:(2+max(sch$dpmm_cluster))], col = c(1:max(sch$dpmm_cluster)), lty = c(1:max(sch$dpmm_cluster)), lwd = 2)

# seems to be a deadend without PCA
sch_scaled <- scale(sch[2:15])
dpmm <- DirichletProcessMvnormal2(as.matrix(sch_scaled))
dp2 <- Fit(dpmm, 20, progressBar=TRUE)
plot(dp2)
pairs(sch_scaled, col=dp2$clusterLabels)

