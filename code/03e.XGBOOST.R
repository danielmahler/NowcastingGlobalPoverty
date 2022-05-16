####################
### INTRODUCTION ###
####################

# This script predicts the level of and annualized growth in headcount rates, mean consumption, median consumption, and the Gini coefficient using gradiant boosting.
# Running this script takes about 1 day.

############################
### LOAD PACKAGES & DATA ###
############################

# LOADING PACKAGES
#install.packages("haven")
#install.packages("xgboost")
#install.packages("dplyr")
#install.packages("dummies")
#install.packages("caret")
#install.packages("stringr")
library(caret)
library(xgboost)
library(dplyr)
library(haven)
library(dummies)
library(stringr)

# DIRECTORY
set.seed(1) 
rm(list=ls())
setwd("C:/Users/wb514665/OneDrive - WBG/Research/NowcastingGlobalPoverty")
#setwd("//wbmserccpi201/GDIM/Papers/TrendsPatterns/Do-files/ARCHIVE/Now")

# Loading data
data = read_dta("02.inputdata/FinalInputData.dta")

####################
### PREPARE DATA ###
####################

# Storing lagged mean, median, headcount, gini, year. Will be useful later.
data <- data %>%
  group_by(code, datatype) %>%
  mutate(lagyear  = lag(year, 1)) %>%
  mutate(laghead  = lag(Y_l_head, 1)) %>%
  mutate(lagmedi  = lag(Y_l_medi, 1)) %>%
  mutate(lagmean  = lag(Y_l_mean, 1)) %>%
  mutate(laggini  = lag(Y_l_gini, 1))

# Convert factor variables into dummies (needed for XGBOOST)
data = dummy.data.frame(
  as.data.frame(data),
  names = c(
    "CLASS_region",
    "CLASS_region_povcalnet",
    "CLASS_fcv",
    "CLASS_ida",
    "CLASS_incgroup",
    "CLASS_region_ssasub"
  ),
  sep = ""
)
data$datatype = as.integer(data$datatype)


#############
### FOLDS ###
#############

# Create outer-folds (inner folds created through xgboost)
data$outerfolds = sample(1:5, nrow(data), replace = TRUE)

##############################################
### LOOPING OVER OUTCOME VARIABLE & SAMPLE ###
##############################################

# We want to predict six different outcome variables
for (yvar in c(
  "Y_l_head",
  "Y_c_head",
  "Y_l_mean",
  "Y_g_mean",
  "Y_l_medi",
  "Y_g_medi",
  "Y_l_gini",
  "Y_g_gini"
)) {
  ################
  ### FEATURES ###
  ################

  # Features common to all outcome variables
  PCN   = c(colnames(data)[grepl("PCN_",  names(data))])
  CLASS = c(colnames(data)[grepl("CLASS_", names(data))])

  # Features specific to change/growth predictions
  if (grepl("_g_|_c_", yvar)) {
    NAA = c(colnames(data)[grepl("NA_", names(data))])
    WEO = c(colnames(data)[grepl("WEO_", names(data))])
    WDI = c(colnames(data)[grepl("WDI_", names(data))])
    RS  = c(colnames(data)[grepl("RS",   names(data))])


  }
  # Features specific to level predictions
  if (grepl("_l_", yvar)) {
    NAA = c(colnames(data)[grepl("NA_l_", names(data))])
    WEO = c(colnames(data)[grepl("WEO_l_", names(data))])
    WDI = c(colnames(data)[grepl("WDI_l_", names(data))])
    RS  = c(colnames(data)[grepl("RS_l_",  names(data))])

  }

  # Features is the joint set of all of these
  features = c("year", "datatype", PCN, NAA, CLASS, WEO, WDI, RS)

  # Storing the number of features, which will be useful later
  (numbfeatures = length(features))


  # For each, using both all data points and only comparable spells
  for (sample in c("al", "co")) {
    # Show which combination is being processed at the moment
    print(c(yvar, sample))

    ######################
    ### PREPARING DATA ###
    ######################

    # Create training sample
    if (sample == "al" & grepl("_g_|_c_", yvar)) {
      modeldata = data[data$sample_al_g == 1, ]
    }
    if (sample == "al" & grepl("_l_", yvar)) {
      modeldata = data[data$sample_al_l == 1, ]
    }
    if (sample == "co" & grepl("_g_|_c_", yvar)) {
      modeldata = data[data$sample_co_g == 1, ]
    }
    if (sample == "co" & grepl("_l_", yvar)) {
      modeldata = data[data$sample_co_l == 1, ]
    }

    # There can't be any missing values in the outcome variable
    modeldata = modeldata[complete.cases(modeldata[[yvar]]), ]
    # Specific code needed for xgboost: the y-variable needs to be separated
    modeldatalabel = as.matrix(modeldata[[yvar]])
    datalabel = as.matrix(data[[yvar]])


    ############################
    ### PREPARING FOR TUNING ###
    ############################

    # Tuning grid
    tune_grid <- expand.grid(
      nrounds            = seq(from = 200, to = 600, by = 400),
      max_depth          = c(2, 4),
      eta                = c(0.03, 0.06),
      gamma              = 0,
      colsample_bytree   = seq(0.5, 0.7, 0.2),
      min_child_weight   = 1,
      subsample          =  seq(0.6, 0.8, 0.2)
    )
    dim(tune_grid)

    # Cross validation settings
    tune_control <- caret::trainControl(
      method        = "cv",
      # cross-validation
      number        = 5,
      # with n folds
      verboseIter   = TRUE,
      # no training log
      allowParallel = TRUE # FALSE for reproducible results
    )

    # Creates a matrix that will contain the optimal parameter values by fold
    OPT = data.frame(matrix(NA, nrow = 5, ncol = 1 + dim(tune_grid)[2]))
    colnames(OPT) <- c("fold", colnames(tune_grid))
    OPT$fold <- seq(1, 5)

    # LOOPING OVER FOLDS
    for (folds in seq(1, 5)) {
      ##############
      ### TUNING ###
      ##############

      # Tuning
      xgb_tune <- caret::train(
        x         = data.matrix(modeldata[modeldata$outerfolds != folds, ][features]),
        y         = as.double(modeldatalabel[modeldata$outerfolds != folds]),
        trControl = tune_control,
        tuneGrid  = tune_grid,
        method    = "xgbTree",
        verbose   = FALSE,
        metric    = "RMSE"
      )

      # Stores optimal parameters
      OPT[folds, 1:dim(tune_grid)[2] + 1] = xgb_tune$bestTune

      # Renames the matrices with the results such that all result matrix will be kept
      assign(paste('OPT',    yvar, sample, sep = "_"), OPT)
      write_dta(
        OPT,
        paste(
          '03.intermediatedata/Tuning/grbo_',
          noquote(yvar),
          '_',
          noquote(sample),
          '.dta',
          sep = ""
        )
      )


      # End cross-validation loop
    }

    ###############################
    ### RUNNING OPTIMAL VERSION ###
    ###############################

    # RUNNING WITH OPTIMAL PARAMETERS
    # In principle, we should retune using the entire dataset. Here I used the average optimal tuning parameters instead


    xgb <-
      xgboost(
        data             = data.matrix(modeldata[features]),
        label            = modeldatalabel,
        # Y-variable
        eta              =       mean(OPT[, "eta"]),
        # Learning rate
        max_depth        = round(mean(OPT[, "max_depth"])),
        # Max depth of tree
        nround           =       mean(OPT[, "nrounds"]),
        # Max number of boosting iterations
        subsample        =       mean(OPT[, "subsample"]),
        colsample_bytree =       mean(OPT[, "colsample_bytree"]),
        # Subsample ratio of columns when constructing each tree
        objective        = "reg:squarederror"
      )

    ################################
    ### PLOTTING OPTIMAL VERSION ###
    ################################

    varimp_absolute = xgb.importance(model = xgb)[, 1:2]

    # Adjust the variable importance measure, such that the circumstance with the highest importance gets a value of 1
    varimp_relative = round(varimp_absolute[, 2] / max(varimp_absolute[, 2]), 3)
    rownames(varimp_relative) <- varimp_absolute$Feature

    # Only keeping 10 most important variables (otherwise difficult to visualize)
    varimp_plot  = as.data.frame(varimp_relative[1:10])
    names(varimp_plot) <- c("varimp")
    varimp_plot <- tibble::rownames_to_column(varimp_plot, "var")

    # Fixing names of variables
    varimp_plot$label = ""
    for (vr in 1:10) {
      # Some of the dummies created don't have variable labels. Separating according to that
      if (!is.null(attributes(data[[rownames(varimp_relative)[vr]]])$label)) {
        varimp_plot$label[vr] = attributes(data[[rownames(varimp_relative)[vr]]])$label
      }
      if (is.null(attributes(data[[rownames(varimp_relative)[vr]]])$label)) {
        varimp_plot$label[vr] = rownames(varimp_relative)[vr]
      }
    }

    # Plot the results
    ggplot(varimp_plot, aes(x = reorder(label, varimp), y = varimp)) + geom_bar(stat =
                                                                                  "identity", fill = "#1A8693") +
      coord_flip() +  ylab("Variable importance") +  xlab("") +
      scale_x_discrete(
        labels = function(x)
          str_wrap(x, width = 50)
      ) +
      theme(axis.text = element_text(size = 20),
            axis.title = element_text(size = 20))
    ggsave(
      paste(
        '05.figures/varimp/varimp_grbo_',
        noquote(yvar),
        '_',
        noquote(sample),
        '.jpg',
        sep = ""
      ),
      width = 18,
      units = "in"
    )

    ###################################
    ### (OUT-OF-SAMPLE) PREDICTIONS ###
    ###################################

    # Now the out-of-sample predictions.
    predname = paste(noquote(yvar), '_grbo_', noquote(sample), '_dire', sep =
                       "")
    data[[predname]] = as.double(NA)
    # For the nowcasting sample we use the final forest run
    data[data$sample_now == 1, ][predname] <-
      predict(xgb, data.matrix(data[data$sample_now == 1, features]))
    # For the non-nowcasting sample we use the optimal tuned version for each fold
    for (folds in seq(1, 5)) {
      xgb <-
        xgboost(
          data             = data.matrix(modeldata[modeldata$outerfolds != folds, ][features]),
          label            = as.double(modeldatalabel[modeldata$outerfolds !=
                                                        folds]),
          # Y-variable
          eta              = OPT[folds, "eta"],
          # Learning rate
          max_depth        = OPT[folds, "max_depth"],
          # Max depth of tree
          nround           = OPT[folds, "nrounds"],
          # Max number of boosting iterations
          subsample        = OPT[folds, "subsample"],
          colsample_bytree = OPT[folds, "colsample_bytree"],
          # Subsample ratio of columns when constructing each tree
          objective        = "reg:squarederror"
        )
      data[data$sample_now == 0 &
             data$outerfolds == folds, ][predname] <-
        predict(xgb, data.matrix(data[data$sample_now == 0 &
                                        data$outerfolds == folds, ][features]))
    }

    # Saving dataset in case code breaks in the middle
    write_dta(data, "03.intermediatedata/Predictions/grbo.dta")

    ####################################
    ### ENDING OUTCOME & SAMPLE LOOP ###
    ####################################

    # Ending sample loop
  }
  # Ending outcome variable loop
}

######################################
### REFORMATING/SAVING FINAL DATA ####
######################################

data <- data[order(data$code, data$datatype, data$year), ]
data <-
  data[c(
    "code",
    "year" ,
    "datatype",
    "sample_now",
    "sample_al_l",
    "sample_al_g",
    "sample_co_l",
    "sample_co_g",
    "weight_al_l",
    "weight_al_g",
    "weight_co_l",
    "weight_co_g",
    colnames(data)[grepl("Y_",  names(data))],
    colnames(data)[grepl("lag",  names(data))]
  )]

# In case some growth predictions are outside of logical intervals
data$Y_l_head_grbo_al_dire[data$Y_l_head_grbo_al_dire < 0]  = 0
data$Y_l_head_grbo_co_dire[data$Y_l_head_grbo_co_dire < 0]  = 0
data$Y_l_mean_grbo_al_dire[data$Y_l_mean_grbo_al_dire < 10] = 10
data$Y_l_mean_grbo_co_dire[data$Y_l_mean_grbo_co_dire < 10] = 10
data$Y_l_medi_grbo_al_dire[data$Y_l_medi_grbo_al_dire < 10] = 10
data$Y_l_medi_grbo_co_dire[data$Y_l_medi_grbo_co_dire < 10] = 10
data$Y_l_gini_grbo_al_dire[data$Y_l_gini_grbo_al_dire < 10] = 10
data$Y_l_gini_grbo_co_dire[data$Y_l_gini_grbo_co_dire < 10] = 10
data$Y_l_head_grbo_al_grow[data$Y_l_head_grbo_al_grow > 100] = 100
data$Y_l_head_grbo_co_grow[data$Y_l_head_grbo_co_grow > 100] = 100
data$Y_l_gini_grbo_al_grow[data$Y_l_gini_grbo_al_grow > 70]  = 70
data$Y_l_gini_grbo_co_grow[data$Y_l_gini_grbo_co_grow > 70]  = 70
data$Y_l_head_grbo_al_grow[data$Y_l_head_grbo_al_grow < 0]   = 0
data$Y_l_head_grbo_co_grow[data$Y_l_head_grbo_co_grow < 0]   = 0

# Works for first extrapolation year
data <- data %>%
  group_by(code, datatype) %>%
  mutate(Y_l_head_grbo_al_grow = laghead + Y_c_head_grbo_al_dire * (year -
                                                                      lagyear)) %>%
  mutate(Y_l_head_grbo_co_grow = laghead + Y_c_head_grbo_co_dire * (year -
                                                                      lagyear)) %>%
  mutate(Y_l_mean_grbo_al_grow = lagmean * (Y_g_mean_grbo_al_dire + 1) ^
           (year - lagyear)) %>%
  mutate(Y_l_mean_grbo_co_grow = lagmean * (Y_g_mean_grbo_co_dire + 1) ^
           (year - lagyear)) %>%
  mutate(Y_l_medi_grbo_al_grow = lagmedi * (Y_g_medi_grbo_al_dire + 1) ^
           (year - lagyear)) %>%
  mutate(Y_l_medi_grbo_co_grow = lagmedi * (Y_g_medi_grbo_co_dire + 1) ^
           (year - lagyear)) %>%
  mutate(Y_l_gini_grbo_al_grow = laggini * (Y_g_gini_grbo_al_dire + 1) ^
           (year - lagyear)) %>%
  mutate(Y_l_gini_grbo_co_grow = laggini * (Y_g_gini_grbo_co_dire + 1) ^
           (year - lagyear))

# Works for remaining extrapolation years
for (reps in seq(1, 21)) {
  data <- data %>%
    group_by(code, datatype) %>%
    mutate(
      Y_l_head_grbo_al_grow = ifelse(
        is.na(Y_l_head_grbo_al_grow),
        lag(Y_l_head_grbo_al_grow) + Y_c_head_grbo_al_dire * (year - lagyear),
        Y_l_head_grbo_al_grow
      )
    ) %>%
    mutate(
      Y_l_head_grbo_co_grow = ifelse(
        is.na(Y_l_head_grbo_co_grow),
        lag(Y_l_head_grbo_co_grow) + Y_c_head_grbo_co_dire * (year - lagyear),
        Y_l_head_grbo_co_grow
      )
    ) %>%
    mutate(Y_l_mean_grbo_al_grow = ifelse(
      is.na(Y_l_mean_grbo_al_grow),
      lag(Y_l_mean_grbo_al_grow) * (Y_g_mean_grbo_al_dire + 1) ^ (year - lagyear),
      Y_l_mean_grbo_al_grow
    )) %>%
    mutate(Y_l_mean_grbo_co_grow = ifelse(
      is.na(Y_l_mean_grbo_co_grow),
      lag(Y_l_mean_grbo_co_grow) * (Y_g_mean_grbo_co_dire + 1) ^ (year - lagyear),
      Y_l_mean_grbo_co_grow
    )) %>%
    mutate(Y_l_medi_grbo_al_grow = ifelse(
      is.na(Y_l_medi_grbo_al_grow),
      lag(Y_l_medi_grbo_al_grow) * (Y_g_medi_grbo_al_dire + 1) ^ (year - lagyear),
      Y_l_medi_grbo_al_grow
    )) %>%
    mutate(Y_l_medi_grbo_co_grow = ifelse(
      is.na(Y_l_medi_grbo_co_grow),
      lag(Y_l_medi_grbo_co_grow) * (Y_g_medi_grbo_co_dire + 1) ^ (year - lagyear),
      Y_l_medi_grbo_co_grow
    )) %>%
    mutate(Y_l_gini_grbo_al_grow = ifelse(
      is.na(Y_l_gini_grbo_al_grow),
      lag(Y_l_gini_grbo_al_grow) * (Y_g_gini_grbo_al_dire + 1) ^ (year - lagyear),
      Y_l_gini_grbo_al_grow
    )) %>%
    mutate(Y_l_gini_grbo_co_grow = ifelse(
      is.na(Y_l_gini_grbo_co_grow),
      lag(Y_l_gini_grbo_co_grow) * (Y_g_gini_grbo_co_dire + 1) ^ (year - lagyear),
      Y_l_gini_grbo_co_grow
    ))
}


# In case some growth predictions are outside of logical intervals
data$Y_l_head_grbo_al_grow[data$Y_l_head_grbo_al_grow > 100] = 100
data$Y_l_head_grbo_co_grow[data$Y_l_head_grbo_co_grow > 100] = 100
data$Y_l_gini_grbo_al_grow[data$Y_l_gini_grbo_al_grow > 70]  = 70
data$Y_l_gini_grbo_co_grow[data$Y_l_gini_grbo_co_grow > 70]  = 70
data$Y_l_head_grbo_al_grow[data$Y_l_head_grbo_al_grow < 0]   = 0
data$Y_l_head_grbo_co_grow[data$Y_l_head_grbo_co_grow < 0]   = 0

# Dropping the lag columns
data = data[, -grep("lag", colnames(data))]

# SAVES DATASET WITH THE FINAL PREDICTIONS
write_dta(data, "03.intermediatedata/Predictions/grbo.dta")
