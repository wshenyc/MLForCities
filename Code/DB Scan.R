library(tidyverse)
library(dbscan)
library(fpc)
library(factoextra)

prob_pred_all = read_csv("prob_edit_geo.csv")

prob_pred_dt <- prob_pred_all %>% 
  filter(!is.na(Latitude) & dt_flag == 1)

locs = dplyr::select(test,Latitude,Longitude)

locs.scaled = scale(locs, center = T, scale = T)

db = dbscan::dbscan(locs.scaled,eps=0.15,minPts = 12)
db

# DBSCAN clustering for 23025 objects.
# Parameters: eps = 0.15, minPts = 12
# Using euclidean distances and borderpoints = TRUE
# The clustering contains 8 cluster(s) and 125 noise points.
# 
# 0     1     2     3     4     5     6     7     8 
# 125 22343   184   140    39   117    19    22    36 
# 
# Available fields: cluster, eps, minPts, dist, borderPoints

factoextra::fviz_cluster(db,locs.scaled,stand = F,ellipse = T,geom = "point")

dbscan::kNNdistplot(locs.scaled,k=12)
abline(h=0.15,lty = 2,col=rainbow(1),main="eps optimal value")

trainIndex = sample(nrow(locs.scaled),size = round(0.8*nrow(locs)))
X_train = locs.scaled[trainIndex,]
y_train = locs.scaled[-trainIndex,]

#model application
dbsc = fpc::dbscan(X_train,eps = 0.15,MinPts  = 12)

# Cluster prediction

preds = fpc::predict.dbscan(object = dbsc,
                            data = X_train,newdata = y_train,
)

table(preds)
