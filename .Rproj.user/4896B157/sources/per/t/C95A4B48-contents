require(tidyverse)
require(caret)
require(shinyalert)
require(shinyWidgets)
require(shinyjs)
require(ranger)
require(tools)

library(DT)

library(shiny)
library(magrittr)
library(xgboost)
library(raster)
library(nabor)
library(shinycssloaders)
library(DT)
library(plotly)
library(ggforce)

options(shiny.maxRequestSize=10000*1024^2)
West_musgraves_xgb_Model <- xgb.load(modelfile = 'Models/West_Musgraves_Near_Miss_Model.xgb')
West_musgraves_RDS_Model<-readRDS("Models/West_Musgraves_Near_Miss.RDS")
Carra_Model<-readRDS("Models/Carra_Near_Miss.RDS")
Carra_Model_No_V<-readRDS("Models/Carra_Near_Miss_No_Vanadium.RDS")
Starra_Model_xgb_Model<-xgb.load(modelfile='Models/Starra_Near_Miss_Model.xgb')
Starra_RDS_Model<-readRDS('Models/Starra_Near_Miss.RDS')
PH_RDS_Dis_Model<-readRDS('Models/prom_hill_distance_model.rds')
PH_RDS_Model_distance_regression<-readRDS('Models/Prom_Hill_Distance_regression.RDS')
Samplefile<-read.csv('www/Sample.csv')

Model<-function(datasource)
{
  datasource <- datasource[,colSums(is.na(datasource))<nrow(datasource)]
  
  datasource[,6:ncol(datasource)]<-sapply(datasource[,6:ncol(datasource)],as.numeric)
  datasource[,6:ncol(datasource)]<-sapply(datasource[,6:ncol(datasource)],as.numeric)
  

  if (all(c('AlPPM','CaPPM','KPPM','NaPPM',
            'SbPPM','TiPPM','AgPPM','AsPPM',
            'CoPPM', 'FePPM', 'MnPPM','MoPPM',
            'NiPPM', 'PbPPM', 'SPPM','UPPM','ZnPPM') %in% names(datasource)==TRUE) ) {
    data<-datasource
    data
    data <-data %>% rename(Al_pct_BESTEL=AlPPM,Ca_pct_BESTEL=CaPPM,K_pct_BESTEL=KPPM,Na_pct_BESTEL=NaPPM,
                           Sb_ppm_BESTEL=SbPPM,Ti_pct_BESTEL=TiPPM,Ag_ppm_BEST_D=AgPPM,As_ppm_BEST_D= AsPPM,
                           Co_ppm_BEST_D=CoPPM, Fe_ppm_BEST_D=FePPM, Mn_ppm_BEST_D=MnPPM,Mo_ppm_BEST_D=MoPPM,
                           Ni_ppm_BEST_D=NiPPM, Pb_ppm_BEST_D=PbPPM, S_pct_BEST_D=SPPM,U_ppm_BEST_D=UPPM,
                           Zn_pct_BEST_D=ZnPPM)
    data$Al_pct_BESTEL=data$Al_pct_BESTEL/10000
    data$Ca_pct_BESTEL=data$Ca_pct_BESTEL/10000
    data$K_pct_BESTEL=data$K_pct_BESTEL/10000
    data$Na_pct_BESTEL=data$Na_pct_BESTEL/10000
    data$Ti_pct_BESTEL=data$Ti_pct_BESTEL/10000
    data$S_pct_BEST_D=data$S_pct_BEST_D/10000
    data$Zn_pct_BEST_D=data$Zn_pct_BEST_D/10000
    data<-subset(data, select = c(PROJECT,SiteID,SampleID,DEPTH_FROM,DEPTH_TO,Al_pct_BESTEL,
                                  Ca_pct_BESTEL, K_pct_BESTEL, Na_pct_BESTEL, Sb_ppm_BESTEL,
                                  Ti_pct_BESTEL, Ag_ppm_BEST_D, As_ppm_BEST_D, Co_ppm_BEST_D,
                                  Fe_ppm_BEST_D, Mn_ppm_BEST_D, Mo_ppm_BEST_D, Ni_ppm_BEST_D,
                                  Pb_ppm_BEST_D, S_pct_BEST_D, U_ppm_BEST_D, Zn_pct_BEST_D))


    data
    data_Complete <-data %>% drop_na(Starra_RDS_Model$Variables)
    data_Complete
    data_ToProcess <- data %>% dplyr::select(Starra_RDS_Model$Variables) %>% as.matrix()
    data_ToProcess
    pred_res <- predict(object = Starra_Model_xgb_Model, newdata = data_ToProcess)
    datasource <- cbind(datasource,CloseValue_Starra =round(pred_res, digits=4))

  } else {
    ReqVariables =c('AlPPM','CaPPM','KPPM','NaPPM',
                    'SbPPM','TiPPM','AgPPM','AsPPM',
                    'CoPPM', 'FePPM', 'MnPPM','MoPPM',
                    'NiPPM', 'PbPPM', 'SPPM','UPPM','ZnPPM')
    ReqVariables
    datasource <- datasource[,colSums(is.na(datasource))<nrow(datasource)]
    datasource
    AvailableVariables=c(colnames(datasource))
    AvailableVariables
    
    d=(setdiff(ReqVariables,AvailableVariables))
    Missing_elements="Missing element/s"
    datasource <- cbind(datasource,CloseValue_Carra_Model = paste(Missing_elements,d ))
  
  }
  if (all(c('CuPPM','AuPPM',
           'AgPPM', 'UPPM', 'FePPM', 'AsPPM',
             'BaPPM', 'BiPPM','CaPPM','CePPM',
            'CoPPM', 'KPPM','LaPPM','MgPPM',
            'MnPPM','MoPPM', 'NaPPM','NiPPM',
            'PPPM','PbPPM', 'SbPPM', 'SPPM',
            'SrPPM','TiPPM', 'YPPM','ZnPPM') %in% names(datasource)==TRUE) ) {
    data<-datasource
    data
    data <-data %>% rename(SAMPLE_ID=SampleID, HOLE_ID=SiteID,  Cu_PPMpref=CuPPM, Au_PPMpref =AuPPM,
                           Ag_PPMpref =AgPPM, U_PPMpref=UPPM, Fe_PPMpref=FePPM, As_PPMpref=AsPPM,
                           Ba_PPMpref =BaPPM,Bi_PPMpref =BiPPM,Ca_PPMpref=CaPPM, Ce_PPMpref=CePPM,
                           Co_PPMpref=CoPPM, K_PPMpref=KPPM, La_PPMpref=LaPPM, Mg_PPMpref =MgPPM,
                           Mn_PPMpref=MnPPM, Mo_PPMpref=MoPPM, Na_PPMpref=NaPPM,Ni_PPMpref=NiPPM,
                           P_PPMpref=PPPM, Pb_PPMpref=PbPPM, Sb_PPMpref=SbPPM, S_PPMpref=SPPM,
                           Sr_PPMpref=SrPPM, Ti_PPMpref=TiPPM, Y_PPMpref=YPPM,Zn_PPMpref=ZnPPM)
    data<-subset(data, select = c(SAMPLE_ID,HOLE_ID, Cu_PPMpref, Au_PPMpref, Ag_PPMpref,
                                  U_PPMpref, Fe_PPMpref, As_PPMpref, Ba_PPMpref, Bi_PPMpref,
                                  Ca_PPMpref, Ce_PPMpref, Co_PPMpref, K_PPMpref, La_PPMpref,
                                  Mg_PPMpref, Mn_PPMpref, Mo_PPMpref, Na_PPMpref, Ni_PPMpref,
                                  P_PPMpref, Pb_PPMpref, Sb_PPMpref, S_PPMpref, Sr_PPMpref,
                                  Ti_PPMpref, Y_PPMpref, Zn_PPMpref))
    
    
    
    prom_hill_distance_model_fun <- function(prom_hill_distance_model, data_to_model){
      
      # possible NA values:
      na_vals <- c(-99, -990000, 4999.5, 49995000)
      
      # remove suffixes and NA values
      new_data <-
        data_to_model %>% 
        dplyr::select(prom_hill_distance_model$Variables) %>% 
        rename_all(function(x) str_remove(x, '_PPMpref')) %>% 
        mutate_all(function(x){
          x[x %in% na_vals] <- NA
          x
        })
      
      # feature engineering
      new_data %<>%
        mutate(
          SAMPLE_ID = as.character(SAMPLE_ID),
          HOLE_ID = as.character(HOLE_ID),
          Cu_on_S = Cu/S,
          Mg_on_Na = Mg/Na,
          Ti_on_Sr = Ti/Sr
        ) %>% 
        na.omit() %>% 
        mutate_all(    # replace Inf values with whatever the next
          function(x){ # highest value in each column
            ifelse(is.infinite(x),
                   max(x[which(x < Inf)]),
                   x)
          } ) %>%
        distinct(SAMPLE_ID, HOLE_ID, .keep_all = T) # beware duplicates!
      
      message(paste("There are", nrow(new_data), "data to model"))
      
      # separate samples that were in the training data from new samples:
      to_model_old <- # samples in new_data IN training data
        semi_join(
          x = new_data,
          y = prom_hill_distance_model$Models$training,
          by = c("SAMPLE_ID", "HOLE_ID"))

      to_model_new <- # samples in new_data NOT IN training data
        anti_join(
          x = new_data,
          y = prom_hill_distance_model$Models$training,
          by = c("SAMPLE_ID", "HOLE_ID"))

      message(paste("of which there are", nrow(to_model_new), "NEW data"))
      message(paste("and which there are", nrow(to_model_old), "data used in training"))
      # 
      
      # predict the old and new data
      pred_xgb_old <-
        to_model_old %>%
        left_join(
          prom_hill_distance_model$Models$training,
          by = c("SAMPLE_ID", "HOLE_ID"))

      pred_xgb_new <-
        to_model_new %>%
        mutate(PH_distance_pred =
                 predict(object = prom_hill_distance_model$Models$model,
                         newdata = to_model_new))

      #join those:
      out_data <-
        bind_rows(
          pred_xgb_old,
          pred_xgb_new
        ) %>%
        dplyr::select(SAMPLE_ID, HOLE_ID, PH_distance_pred)

      # Pseudo-probability (0-1):
      prob_distance <- 2500 # pseudo prob of being within this distance
      prob_z <- 4 # "SD" of regression val is PH_distance_pred/prob_z
      
      out_data %<>%
        mutate(
          mod_PH_distance_pred = pmax(0, PH_distance_pred),
          distance_sd = mod_PH_distance_pred/prob_z,
          distance_z = (mod_PH_distance_pred - prob_distance)/distance_sd,
          distance_z_clip = pmax(pmin(distance_z, 4), -4),
          !!paste0("pseudo_prob_", prob_distance) :=
            1-1*(distance_z_clip - min(distance_z_clip))/diff(range(distance_z_clip))
        ) %>% 
        dplyr::select(-distance_sd, -distance_z,
                      -distance_z_clip, -mod_PH_distance_pred)
      
    }
    
    ki<-prom_hill_distance_model_fun(PH_RDS_Dis_Model,data)
    ki<-subset(ki, select = c(pseudo_prob_2500))
    ki<-ki %>% rename(CloseValue_Promhill_Model=pseudo_prob_2500)
    datasource <- cbind(datasource,round(ki,digits=4))
    # datasource
    
  }else {
    ReqVariables =c('CuPPM','AuPPM',
                    'AgPPM', 'UPPM', 'FePPM', 'AsPPM',
                    'BaPPM', 'BiPPM','CaPPM','CePPM',
                    'CoPPM', 'KPPM','LaPPM','MgPPM',
                    'MnPPM','MoPPM', 'NaPPM','NiPPM',
                    'PPPM','PbPPM', 'SbPPM', 'SPPM',
                    'SrPPM','TiPPM', 'YPPM','ZnPPM')
    ReqVariables
    datasource <- datasource[,colSums(is.na(datasource))<nrow(datasource)]
    datasource
    AvailableVariables=c(colnames(datasource))
    AvailableVariables
    
    d=(setdiff(ReqVariables,AvailableVariables))
    Missing_elements="Missing element/s"
    datasource <- cbind(datasource,CloseValue_Promhill_Model = paste(Missing_elements,d ))
  }


}



# SAMPLE_ID HOLE_ID Cu_PPMpref Au_PPMpref 
# Ag_PPMpref U_PPMpref Fe_PPMpref As_PPMpref
# Ba_PPMpref Bi_PPMpref Ca_PPMpref Ce_PPMpref
# Co_PPMpref K_PPMpref La_PPMpref Mg_PPMpref 
# Mn_PPMpref Mo_PPMpref Na_PPMpref Ni_PPMpref
# P_PPMpref Pb_PPMpref Sb_PPMpref S_PPMpref Sr_PPMpref Ti_PPMpref Y_PPMpref Zn_PPMpref