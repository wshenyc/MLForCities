library(tidyverse)
library(dbscan)
library(fpc)
library(factoextra)

prob_pred_all = read_csv("/Users/winnieshen/Desktop/RStudio Projects/prob_edit_geo.csv")
#prob_pred_all = read_csv("/Users/winnieshen/Desktop/RStudio Projects/hmc_2022_slim.csv")

prob_pred_clean <- prob_pred_all %>% 
  filter(!is.na(Latitude))

locs = dplyr::select(prob_pred_clean,Longitude,Latitude)

locs.scaled = scale(locs, center = T, scale = T)

db = dbscan::dbscan(locs.scaled,eps=0.15,minPts = 200)
db


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
