library(gbm)
library(tidyverse)
library(caret)
library(parallel)

setwd("C:/Users/matt.dornback/R_Working_Directory/DeepSeaCorals/Deep-Sea Climatology/Boosted_Reg_Tree")
d <- read_rds('C:/Users/matt.dornback/R_Working_Directory/DeepSeaCorals/Deep-Sea Climatology/EMU/Coral_EMU_20190709-3_Pacific.rds')



d.selection <- d %>% select(DepthInMeters, EMU_temp_, EMU_salinity, EMU_appO2ut, 
                            EMU_dissO2, EMU_nitrate, EMU_percO2sat, EMU_phosphate, 
                            EMU_silicate, EMU_ChlorA_12yrAvg, Class, Order,Family, Genus,FishCouncilRegion, EMU_Cluster37, EMU_GeomorphologyBase, CatalogNumber,
                            SamplingEquipment, ObservationYear, Ocean) %>% 
  filter(Ocean == "North Pacific",
         SamplingEquipment %in% c("AUV","drop camera", "ROV", "SCUBA", "submersible", "towed camera", "trawl"),
         ObservationYear > 1977,
         Class %in% c("Anthozoa"), 
         Order %in% c("Alcyonacea"#, "Antipatharia", "Pennatulacea", "Scleractinia"
         ), 
         Family %in% c("Acanthogorgiidae","Alcyoniidae","Chrysogorgiidae","Coralliidae","Isididae",
                       "Paragorgiidae","Plexauridae","Primnoidae"),
         EMU_Cluster37 %in% c(3,13,33,37)
  ) %>% 
  na.omit() %>% 
  rename(Temp='EMU_temp_',Salinity='EMU_salinity',appO2u='EMU_appO2ut',dissO2='EMU_dissO2',Nitrate='EMU_nitrate',O2sat='EMU_percO2sat',
         Phosph='EMU_phosphate',Silicate='EMU_silicate',Chlor='EMU_ChlorA_12yrAvg',Depth="DepthInMeters")
# , Family %in% c("Acanthogorgiidae","Alcyoniidae","Isididae","Paragorgiidae","Plexauridae","Primnoidae")
#c(0,11,14)
#randomize the rows
d.selection <- d.selection[sample(nrow(d.selection)),]

#reproducibility
set.seed(123)

#create train and test partitions
#d.train <- createDataPartition(y=d.selection$Family, p=.7, list=FALSE)
#train <- d.selection[d.train,]
#test <- d.selection[-d.train,]

(GBM.model = gbm(Family ~ Temp + Salinity + appO2u + dissO2 + Nitrate + O2sat + Phosph + Silicate + Chlor + Depth, 
                 distribution = "multinomial",data = d.selection, n.trees = 500, shrinkage = 0.01, cv.folds = 5, 
                 interaction.depth = 1,n.minobsinnode = 5))

# get MSE and compute RMSE
sqrt(min(GBM.model$cv.error))

# plot loss function as a result of n trees added to the ensemble
gbm.perf(GBM.model, method = "cv")

# summarize variables with most influence
summary(GBM.model,cBars = 10, las = 1)
print(GBM.model)
pretty(GBM.model, i.tree=1)

best.iter <- gbm.perf(GBM.model, method = "OOB")
print(best.iter)

# Check performance using the 50% heldout test set
best.iter <- gbm.perf(GBM.model, method = "test")
print(best.iter)

# Check performance using 5-fold cross-validation
ntree_opt_cv <- gbm.perf(GBM.model, method = "cv")
print(best.iter)

Predictions <- predict(object = GBM.model, newdata = test, n.trees = ntree_opt_cv, type ='response')

PredictionBinaries <- as.factor(ifelse(Predictions>0.1,1,0))
test$Family <- as.factor(test$Family)
confusionMatrix(PredictionBinaries,test$Family)

caret::RMSE(Predictions, test$EMU_ChlorA_12yrAvg)

GBM.model %>%
  partial(pred.var = "DepthInMeters", n.trees = 500, grid.resolution = 100) %>%
  autoplot(rug = TRUE, train = train) +
  scale_y_continuous(labels = scales::dollar)

train(Family ~ DepthInMeters + EMU_dissO2 + EMU_nitrate + EMU_percO2sat + EMU_phosphate + 
        EMU_silicate + EMU_ChlorA_12yrAvg, distribution = "multinomial", data = d.selection, method = "gbm", verbose = F)

#parSapply(cl,data.list,function(i) {
#  train(Order ~ DepthInMeters + EMU_temp_ + EMU_salinity + EMU_appO2ut + EMU_dissO2 + EMU_nitrate + EMU_percO2sat + EMU_phosphate + 
#        EMU_silicate + EMU_ChlorA_12yrAvg, data = i, method = "gbm", verbose = F)})

system.time(train(Family ~ EMU_salinity + EMU_appO2ut + EMU_percO2sat + DepthInMeters + EMU_temp_ +EMU_dissO2 + EMU_nitrate + EMU_phosphate + 
        EMU_silicate + EMU_ChlorA_12yrAvg, distribution = "multinomial",data = d.selection, method = "gbm", verbose = F,
      tuneGrid = expand.grid(n.trees = c(50, 100, 150), interaction.depth = c(1:3), shrinkage = c(0.001,0.01,0.1), n.minobsinnode = 14000)))

quit(save = "yes")

summary(update(GBM.model, shrinkage = 0.01))

plot(GBM.model, "EMU_ChlorA_12yrAvg")


##############

d.random <- d.selection[sample(nrow(d.selection)),]

hyper_grid <- expand.grid(
  shrinkage = c(0.1, .01, .001),
  interaction.depth = c(1,2,3),
  n.minobsinnode = c(5,10,15),
  bag.fraction = c(.8), 
  optimal_trees = 0,               # a place to dump results
  min_RMSE = 0                     # a place to dump results
)

for(i in 1:nrow(hyper_grid)) {
  
  # reproducibility
  #set.seed(123)
  
  # train model
  gbm.tune <- gbm(
    formula = Family ~ EMU_salinity + EMU_appO2ut + EMU_percO2sat + DepthInMeters + EMU_temp_ +EMU_dissO2 + EMU_nitrate + EMU_phosphate + 
      EMU_silicate + EMU_ChlorA_12yrAvg,
    distribution = "multinomial",
    data = d.random,
    n.trees = 500,
    interaction.depth = hyper_grid$interaction.depth[i],
    shrinkage = hyper_grid$shrinkage[i],
    n.minobsinnode = hyper_grid$n.minobsinnode[i],
    bag.fraction = hyper_grid$bag.fraction[i],
    train.fraction = .75,
    n.cores = NULL, # will use all cores by default
    verbose = FALSE
  )
  
  # add min training error and trees to grid
  hyper_grid$optimal_trees[i] <- which.min(gbm.tune$valid.error)
  hyper_grid$min_RMSE[i] <- sqrt(min(gbm.tune$valid.error))
}

hyper_grid %>% 
  dplyr::arrange(min_RMSE) %>%
  head(10)
