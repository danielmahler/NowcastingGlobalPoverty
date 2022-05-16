####################
### INTRODUCTION ###
####################

# This script predicts the level of and annualized growth in headcount rates, mean consumption, median consumption and the Gini coefficient using CART random forests.
# Running this script takes about XX hours

############################
### LOAD PACKAGES & DATA ###
############################

# LOADING PACKAGES
#install.packages("sjlabelled")
#install.packages("haven")
#install.packages("randomForest")
#install.packages("dplyr")
#install.packages("stringr")
#install.packages("ggplot2")
library(sjlabelled)
library(dplyr)
library(haven)
library(randomForest)
library(stringr)
library(ggplot2)

# DIRECTORY
set.seed(1)
rm(list = ls())
setwd("C:/Users/wb514665/OneDrive - WBG/Research/NowcastingGlobalPoverty")
#setwd("//wbmserccpi201/GDIM/Papers/TrendsPatterns/Do-files/ARCHIVE/Now")

# Loading data
data = read_dta("02.inputdata/FinalInputData.dta")

####################
### PREPARE DATA ###
####################

# Converting some strings to factor variables
data$CLASS_region           = as_factor(data$CLASS_region)
data$CLASS_region_povcalnet = as_factor(data$CLASS_region_povcalnet)
data$CLASS_region_ssasub    = as_factor(data$CLASS_region_ssasub)
data$CLASS_ida              = as_factor(data$CLASS_ida)
data$CLASS_fcv              = as_factor(data$CLASS_fcv)
data$CLASS_incgroup         = as_factor(data$CLASS_incgroup)
data$datatype               = as_factor(data$datatype)

# Storing lagged mean, median, headcount, gini, year. Will be useful later.
data <- data %>%
  group_by(code, datatype) %>%
  mutate(lagyear  = lag(year, 1)) %>%
  mutate(laghead  = lag(Y_l_head, 1)) %>%
  mutate(lagmedi  = lag(Y_l_medi, 1)) %>%
  mutate(lagmean  = lag(Y_l_mean, 1)) %>%
  mutate(laggini  = lag(Y_l_gini, 1))

#############
### FOLDS ###
#############

# Create outer-folds (inner folds not needed since we rely on OOB predictions there)
data$outerfolds = sample(1:5, nrow(data), replace = TRUE)

##############################################
### LOOPING OVER OUTCOME VARIABLE & SAMPLE ###
##############################################

# We want to predict eight different outcome variables
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
  PCN   = gsub(", ", "+", toString(colnames(data)[grepl("PCN_",  names(data))]))
  CLASS = gsub(", ", "+", toString(colnames(data)[grepl("CLASS_", names(data))]))

  # Features specific to change/growth predictions
  if (grepl("_g_|_c_", yvar)) {
    NAA = gsub(", ", "+", toString(colnames(data)[grepl("NA_",  names(data))]))
    WEO = gsub(", ", "+", toString(colnames(data)[grepl("WEO_", names(data))]))
    WDI = gsub(", ", "+", toString(colnames(data)[grepl("WDI_", names(data))]))
    RS  = gsub(", ", "+", toString(colnames(data)[grepl("RS_",  names(data))]))

  }
  # Features specific to level predictions
  if (grepl("_l_", yvar)) {
    NAA = gsub(", ", "+", toString(colnames(data)[grepl("NA_l",  names(data))]))
    WEO = gsub(", ", "+", toString(colnames(data)[grepl("WEO_l", names(data))]))
    WDI = gsub(", ", "+", toString(colnames(data)[grepl("WDI_l", names(data))]))
    RS  = gsub(", ", "+", toString(colnames(data)[grepl("RS_l",  names(data))]))

  }

  # JOining all features
  features = paste("datatype", "year", PCN, NAA, CLASS, WEO, WDI, RS, sep =
                     "+")

  # Storing the number of features, which will be useful later
  (numbfeatures = sum(charToRaw(features) == charToRaw('+')) + 1)


  # For each, using both all data points and only comparable spells, and rich or poor rows
    #for (sample in c("al", "co","ri","po")) {
    for (sample in c("ri","po")) {
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
    if (sample == "ri" & grepl("_g_|_c_", yvar)) {
      modeldata = data[data$sample_ri_g == 1, ]
    }
    if (sample == "ri" & grepl("_l_", yvar)) {
      modeldata = data[data$sample_ri_l == 1, ]
    }
    if (sample == "po" & grepl("_g_|_c_", yvar)) {
      modeldata = data[data$sample_po_g == 1, ]
    }
    if (sample == "po" & grepl("_l_", yvar)) {
      modeldata = data[data$sample_po_l == 1, ]
    }

    ######################
    ### MISSING VALUES ###
    ######################

    # There can't be any missing values in the outcome variable
    modeldata = modeldata[complete.cases(modeldata[[yvar]]), ]
    # Imputing missing values by putting in the mode/median. For this to work we
    # first need to remove character columns
    modeldata = modeldata[, !sapply(modeldata, is.character)]
    modeldata = na.roughfix(modeldata)
    # There shouldn't be any columns with missing values after this. If there
    # is, print them and remove them
    columnstoremove = colnames(modeldata)[colSums(is.na(modeldata)) > 0]
    print(columnstoremove)
    if (length(columnstoremove) != 0) {
      modeldata = modeldata[, -which(names(modeldata) %in% columnstoremove)]
    }

    ##################################################
    ### PREPARING FOR LOOPING OVER VARIOUS FORESTS ###
    ##################################################

    # Creating a grid of forest specifications
    # Decides what the minimum number of observations for a split to be allowed
    # Seems like weights are not possible with randomForest
    (nodesizeseq = round(seq(nrow(modeldata)*0.02,nrow(modeldata)*0.06,nrow(modeldata)*0.02)))
    # Decides the fraction of features to be considered in each forest
    (mtryseq = round(
      seq(numbfeatures * 0.05, 0.45 * numbfeatures, numbfeatures * 0.2)
    ))

    # Creates a matrix that will store the mean absolute deviation for each forest
    RESULTS = data.frame(matrix(
      NA,
      nrow = 5 * length(mtryseq),
      ncol = length(nodesizeseq) + 2
    ))
    colnames(RESULTS) <- c("fold", "mtryseq", nodesizeseq)
    RESULTS$fold <- rep(seq(1, 5), each = length(mtryseq))
    RESULTS$mtryseq <- rep(c(mtryseq), 5)
    # Creates a matrix that will contain the optimal parameter values by fold
    OPT = data.frame(matrix(NA, nrow = 5, ncol = 3))
    colnames(OPT) <- c("fold", "mtryseq", "nodesizeseq")
    OPT$fold <- seq(1, 5)


    ####################################
    ### LOOPING OVER VARIOUS FORESTS ###
    ####################################

    # LOOPING OVER FOLDS
    for (folds in seq(1, 5)) {
      column = 3

      # LOOPING OVER NODE SIZE ITERATIONS
      for (j in nodesizeseq) {
        row    = 1 + length(mtryseq) * (folds - 1)

        # LOOPING OVER MTRY ITERATIONS
        for (i in mtryseq) {
          print(c(folds, j, i))

          forest = randomForest(
            as.formula(paste(yvar, '~', features)),
            data = modeldata[modeldata$outerfolds !=
                               folds, ],
            ntree = 200,
            importance = FALSE,
            nodesize = j,
            mtry = i
          )

          # Inner fold out-of-sample predictions
          modeldata$PRED[modeldata$outerfolds != folds] <-
            predict(forest) ## Uses out-of-bag predictions by default
          modeldata$AD[modeldata$outerfolds != folds]   <-
            abs(modeldata[modeldata$outerfolds != folds, ][[yvar]] - modeldata$PRED[modeldata$outerfolds !=
                                                                                      folds])
          # Renames the forest so it can be called later on
          assign(paste('forest', folds, j, i, sep = "_"), forest)
          # STORES MAD IN THE RELEVANT CELL
          RESULTS[row, column] <-
            mean(modeldata$AD[modeldata$outerfolds != folds])

          row = row + 1
        }
        column = column + 1

      }

      #####################################
      ### STORES THE OPTIMAL PARAMETERS ###
      #####################################

      # Finds the cell with the minimum mean absolute deviation and reports the
      # associated mtry and nodesize
      index = which(RESULTS[RESULTS$fold == folds,] == min(RESULTS[RESULTS$fold ==
                                                                     folds, 3:length(RESULTS)]), arr.ind = TRUE)
      OPT[folds, 2] = RESULTS[RESULTS$fold == folds, 2][index[1]]
      OPT[folds, 3] = as.numeric(colnames(RESULTS)[index[2]])


      # Renames the matrices with the results such that all result matrix will be kept
      assign(paste('RESULTS', yvar, sample, sep = "_"), RESULTS)
      assign(paste('OPT',    yvar, sample, sep = "_"), OPT)
      write_dta(
        OPT,
        paste(
          '03.intermediatedata/Tuning/carf_',
          noquote(yvar),
          '_',
          noquote(sample),
          '.dta',
          sep = ""
        )
      )


      # End cross-validation loop
    }

    ##############################
    ### RUNNING OPTIMAL FOREST ###
    ##############################

    # In principle, we should retune using the entire dataset. Here I used the average optimal tuning parameters instead
    mtry_opt     = mean(OPT[, 2])
    nodesize_opt = mean(OPT[, 3])

    # Run a large forest here to stabilize variable importance plots
    forest = randomForest(
      as.formula(paste(yvar, '~', features)),
      data = modeldata,
      ntree = 500,
      importance = TRUE,
      mtry = mtry_opt,
      nodesize = nodesize_opt
    )

    # Stores the optimal forest
    # assign(paste('forest',yvar,sample,sep="_"),forest)

    #################################
    ### VARIABLE IMPORTANCE PLOTS ###
    #################################

    # Calculates the importance of each circumstance in predicting the outcome
    # NB: Takes some time to run.
    for (measure in 1:2) {
      varimp_absolute = importance(forest)[, measure]

      # Adjust the variable importance measure, such that the circumstance with the highest importance gets a value of 1
      varimp_relative = round(varimp_absolute / max(varimp_absolute), 3)

      # Only keeping 10 most important variables (otherwise difficult to visualize)
      varimp_plot  = as.data.frame(varimp_relative[order(varimp_relative)[(numbfeatures -
                                                                             9):numbfeatures]])
      names(varimp_plot) <- c("varimp")
      varimp_plot <- tibble::rownames_to_column(varimp_plot, "var")

      # Fixing names of variables
      varimp_plot$label = ""
      for (vr in 1:10) {
        varimp_plot$label[vr] = attributes(data[[varimp_plot$var[vr]]])$label
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
          '05.figures/varimp/varimp_carf_',
          noquote(measure),
          '_',
          noquote(yvar),
          '_',
          noquote(sample),
          '.jpg',
          sep = ""
        ),
        width = 18,
        units = "in"
      )
    }


    #######################################################
    ### (OUT-OF-SAMPLE) PREDICTIONS FROM OPTIMAL FOREST ###
    #######################################################

    # Imputing missing values by putting in the mode/median. For this to work we
    # first need to remove character columns
    datacopy = data[, !sapply(data, is.character)]
    datacopy = na.roughfix(datacopy)
    # Now the out-of-sample predictions.
    predname = paste(noquote(yvar), '_carf_', noquote(sample), '_dire', sep =
                       "")
    data[[predname]] = as.double(NA)
    # For the nowcasting sample we use the final forest run
    data[data$sample_now == 1, ][predname] <-
      predict(forest, newdata = datacopy[datacopy$sample_now == 1, ])
    # For the non-nowcasting sample we use the optimal tuned forest for each fold
    for (folds in seq(1, 5)) {
      print(folds)
      foresttouse = get(paste('forest', folds, OPT[folds, 3], OPT[folds, 2], sep =
                                "_"))
      data[data$sample_now == 0 &
             data$outerfolds == folds, ][predname] <-
        predict(foresttouse, newdata = datacopy[data$sample_now == 0 &
                                                  data$outerfolds == folds, ])
    }
    # Saving dataset in case code breaks in the middle
    write_dta(data, "03.intermediatedata/Predictions/cartforest.dta")

    # Removing all forests stored
    rm(list = ls(pattern = "^forest"))

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

backup = data
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
    "sample_ri_l",
    "sample_ri_g",
    "sample_po_l",
    "sample_po_g",
    "weight_al_l",
    "weight_al_g",
    "weight_co_l",
    "weight_co_g",
    "weight_ri_l",
    "weight_ri_g",
    "weight_po_l",
    "weight_po_g",
    colnames(data)[grepl("Y_",  names(data))],
    colnames(data)[grepl("lag",  names(data))]
  )]

# Works for first extrapolation year
data <- data %>%
  group_by(code, datatype) %>%
  mutate(Y_l_head_carf_al_grow = laghead + Y_c_head_carf_al_dire * (year -
                                                                      lagyear)) %>%
  mutate(Y_l_head_carf_co_grow = laghead + Y_c_head_carf_co_dire * (year -
                                                                      lagyear)) %>%
  mutate(Y_l_head_carf_ri_grow = laghead + Y_c_head_carf_ri_dire * (year -
                                                                      lagyear)) %>%
  mutate(Y_l_head_carf_po_grow = laghead + Y_c_head_carf_po_dire * (year -
                                                                      lagyear)) %>%
  mutate(Y_l_mean_carf_al_grow = lagmean * (Y_g_mean_carf_al_dire + 1) ^
           (year - lagyear)) %>%
  mutate(Y_l_mean_carf_co_grow = lagmean * (Y_g_mean_carf_co_dire + 1) ^
           (year - lagyear)) %>%
  mutate(Y_l_mean_carf_ri_grow = lagmean * (Y_g_mean_carf_ri_dire + 1) ^
           (year - lagyear)) %>%
  mutate(Y_l_mean_carf_po_grow = lagmean * (Y_g_mean_carf_po_dire + 1) ^
           (year - lagyear)) %>%
  mutate(Y_l_medi_carf_al_grow = lagmedi * (Y_g_medi_carf_al_dire + 1) ^
           (year - lagyear)) %>%
  mutate(Y_l_medi_carf_co_grow = lagmedi * (Y_g_medi_carf_co_dire + 1) ^
           (year - lagyear)) %>%
  mutate(Y_l_medi_carf_ri_grow = lagmedi * (Y_g_medi_carf_ri_dire + 1) ^
           (year - lagyear)) %>%
  mutate(Y_l_medi_carf_po_grow = lagmedi * (Y_g_medi_carf_po_dire + 1) ^
           (year - lagyear)) %>%
  mutate(Y_l_gini_carf_al_grow = laggini * (Y_g_gini_carf_al_dire + 1) ^
           (year - lagyear)) %>%
  mutate(Y_l_gini_carf_co_grow = laggini * (Y_g_gini_carf_co_dire + 1) ^
           (year - lagyear))
  mutate(Y_l_gini_carf_ri_grow = laggini * (Y_g_gini_carf_ri_dire + 1) ^
         (year - lagyear)) %>%
  mutate(Y_l_gini_carf_po_grow = laggini * (Y_g_gini_carf_po_dire + 1) ^
           (year - lagyear))
# Works for remaining extrapolation years
for (reps in seq(1, 21)) {
  data <- data %>%
    group_by(code, datatype) %>%
    mutate(
      Y_l_head_carf_al_grow = ifelse(
        is.na(Y_l_head_carf_al_grow),
        lag(Y_l_head_carf_al_grow) + Y_c_head_carf_al_dire * (year - lagyear),
        Y_l_head_carf_al_grow
      )
    ) %>%
    mutate(
      Y_l_head_carf_co_grow = ifelse(
        is.na(Y_l_head_carf_co_grow),
        lag(Y_l_head_carf_co_grow) + Y_c_head_carf_co_dire * (year - lagyear),
        Y_l_head_carf_co_grow
      )
    ) %>%
    mutate(
      Y_l_head_carf_ri_grow = ifelse(
        is.na(Y_l_head_carf_ri_grow),
        lag(Y_l_head_carf_ri_grow) + Y_c_head_carf_ri_dire * (year - lagyear),
        Y_l_head_carf_ri_grow
      )
    ) %>%
    mutate(
      Y_l_head_carf_po_grow = ifelse(
        is.na(Y_l_head_carf_po_grow),
        lag(Y_l_head_carf_po_grow) + Y_c_head_carf_po_dire * (year - lagyear),
        Y_l_head_carf_po_grow
      )
    ) %>%
    mutate(Y_l_mean_carf_al_grow = ifelse(
      is.na(Y_l_mean_carf_al_grow),
      lag(Y_l_mean_carf_al_grow) * (Y_g_mean_carf_al_dire + 1) ^ (year - lagyear),
      Y_l_mean_carf_al_grow
    )) %>%
    mutate(Y_l_mean_carf_co_grow = ifelse(
      is.na(Y_l_mean_carf_co_grow),
      lag(Y_l_mean_carf_co_grow) * (Y_g_mean_carf_co_dire + 1) ^ (year - lagyear),
      Y_l_mean_carf_co_grow
    )) %>%
    mutate(Y_l_mean_carf_ri_grow = ifelse(
      is.na(Y_l_mean_carf_ri_grow),
      lag(Y_l_mean_carf_ri_grow) * (Y_g_mean_carf_ri_dire + 1) ^ (year - lagyear),
      Y_l_mean_carf_ri_grow
    )) %>%
    mutate(Y_l_mean_carf_po_grow = ifelse(
      is.na(Y_l_mean_carf_po_grow),
      lag(Y_l_mean_carf_po_grow) * (Y_g_mean_carf_po_dire + 1) ^ (year - lagyear),
      Y_l_mean_carf_po_grow
    )) %>%
    mutate(Y_l_medi_carf_al_grow = ifelse(
      is.na(Y_l_medi_carf_al_grow),
      lag(Y_l_medi_carf_al_grow) * (Y_g_medi_carf_al_dire + 1) ^ (year - lagyear),
      Y_l_medi_carf_al_grow
    )) %>%
    mutate(Y_l_medi_carf_co_grow = ifelse(
      is.na(Y_l_medi_carf_co_grow),
      lag(Y_l_medi_carf_co_grow) * (Y_g_medi_carf_co_dire + 1) ^ (year - lagyear),
      Y_l_medi_carf_co_grow
    )) %>%
    mutate(Y_l_medi_carf_ri_grow = ifelse(
      is.na(Y_l_medi_carf_ri_grow),
      lag(Y_l_medi_carf_ri_grow) * (Y_g_medi_carf_ri_dire + 1) ^ (year - lagyear),
      Y_l_medi_carf_ri_grow
    )) %>%
    mutate(Y_l_medi_carf_po_grow = ifelse(
      is.na(Y_l_medi_carf_po_grow),
      lag(Y_l_medi_carf_po_grow) * (Y_g_medi_carf_po_dire + 1) ^ (year - lagyear),
      Y_l_medi_carf_po_grow
    )) %>%
    mutate(Y_l_gini_carf_al_grow = ifelse(
      is.na(Y_l_gini_carf_al_grow),
      lag(Y_l_gini_carf_al_grow) * (Y_g_gini_carf_al_dire + 1) ^ (year - lagyear),
      Y_l_gini_carf_al_grow
    )) %>%
    mutate(Y_l_gini_carf_co_grow = ifelse(
      is.na(Y_l_gini_carf_co_grow),
      lag(Y_l_gini_carf_co_grow) * (Y_g_gini_carf_co_dire + 1) ^ (year - lagyear),
      Y_l_gini_carf_co_grow
    )) %>%
    mutate(Y_l_gini_carf_ri_grow = ifelse(
      is.na(Y_l_gini_carf_ri_grow),
      lag(Y_l_gini_carf_ri_grow) * (Y_g_gini_carf_ri_dire + 1) ^ (year - lagyear),
      Y_l_gini_carf_ri_grow
    )) %>%
    mutate(Y_l_gini_carf_po_grow = ifelse(
      is.na(Y_l_gini_carf_po_grow),
      lag(Y_l_gini_carf_po_grow) * (Y_g_gini_carf_po_dire + 1) ^ (year - lagyear),
      Y_l_gini_carf_po_grow
    ))
}

# In case some growth predictions are outside of logical intervals
data$Y_l_head_carf_al_grow[data$Y_l_head_carf_al_grow > 100] = 100
data$Y_l_head_carf_co_grow[data$Y_l_head_carf_co_grow > 100] = 100
data$Y_l_head_carf_ri_grow[data$Y_l_head_carf_ri_grow > 100] = 100
data$Y_l_head_carf_po_grow[data$Y_l_head_carf_po_grow > 100] = 100
data$Y_l_gini_carf_al_grow[data$Y_l_gini_carf_al_grow > 70]  = 70
data$Y_l_gini_carf_co_grow[data$Y_l_gini_carf_co_grow > 70]  = 70
data$Y_l_gini_carf_ri_grow[data$Y_l_gini_carf_ri_grow > 70]  = 70
data$Y_l_gini_carf_po_grow[data$Y_l_gini_carf_po_grow > 70]  = 70
data$Y_l_head_carf_al_grow[data$Y_l_head_carf_al_grow < 0]   = 0
data$Y_l_head_carf_co_grow[data$Y_l_head_carf_co_grow < 0]   = 0
data$Y_l_head_carf_ri_grow[data$Y_l_head_carf_ri_grow < 0]   = 0
data$Y_l_head_carf_po_grow[data$Y_l_head_carf_po_grow < 0]   = 0

# Dropping the lag columns
data = data[, -grep("lag", colnames(data))]

# SAVES DATASET WITH THE FINAL PREDICTIONS
write_dta(data, "03.intermediatedata/Predictions/cartforest.dta")

