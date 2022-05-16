####################
### INTRODUCTION ###
####################

# This script predicts the level of and annualized growth in headcount rates, mean consumption, median consumption and the Gini coefficient using conditional inference random forests.
# Running this script takes about XX hours

############################
### LOAD PACKAGES & DATA ###
############################

# Loading packages
#install.packages("sjlabelled")
#install.packages("dplyr")
#install.packages("haven")
#install.packages("party")
#install.packages("stringr")
#install.packages("ggplot2")
library(sjlabelled)
library(dplyr)
library(haven)
library(party)
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


# Storing lagged mean, median, headcount, gini, year. Will be useful later
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

  # For each, using both all data points and only comparable spells
  for (sample in c("al", "co")) {
    # Show which combination is being processed at the moment
    print(c(yvar, sample))

    ##################################################
    ### PREPARING FOR LOOPING OVER VARIOUS FORESTS ###
    ##################################################

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

    ##################################################
    ### PREPARING FOR LOOPING OVER VARIOUS FORESTS ###
    ##################################################

    # Creating a grid of forest specifications
    # Decides what the minimum number of observations for a split to be allowed
    (minsplitseq = round(seq(
      nrow(modeldata) * 0.005,
      nrow(modeldata) * 0.025,
      nrow(modeldata) * 0.01
    )))
    # Decides the fraction of features to be considered in each forest
    (mtryseq = round(
      seq(numbfeatures * 0.05, 0.45 * numbfeatures, numbfeatures * 0.2)
    ))

    # Creates a matrix that will store the mean absolute deviation for each forest
    RESULTS = data.frame(matrix(
      NA,
      nrow = 5 * length(mtryseq),
      ncol = length(minsplitseq) + 2
    ))
    colnames(RESULTS) <- c("fold", "mtryseq", minsplitseq)
    RESULTS$fold <- rep(seq(1, 5), each = length(mtryseq))
    RESULTS$mtryseq <- rep(c(mtryseq), 5)
    # Creates a matrix that will contain the optimal parameter values by fold
    OPT = data.frame(matrix(NA, nrow = 5, ncol = 3))
    colnames(OPT) <- c("fold", "mtryseq", "minsplitseq")
    OPT$fold <- seq(1, 5)

    ####################################
    ### LOOPING OVER VARIOUS FORESTS ###
    ####################################

    # LOOPING OVER FOLDS
    for (folds in seq(1, 5)) {
      column = 3

      # LOOPING OVER NODE SIZE ITERATIONS
      for (j in minsplitseq) {
        row    = 1 + length(mtryseq) * (folds - 1)

        # LOOPING OVER MTRY ITERATIONS
        for (i in mtryseq) {
          print(c(folds, j, i))

          forest = cforest(
            as.formula(paste(yvar, '~', features)),
            # Outcome variables and features, defined above,
            data = modeldata[modeldata$outerfolds != folds, ],
            # Name of dataset
            control = cforest_control(
              mincriterion = 0,
              # 1 minus p-value, governs when to make splits. With forests, it often makes sense to continue splitting regardless of the p-value
              mtry = i,
              # Number of circumstances to consider at each splitting point. Good choices are usually approximiately equivalent to the squareroot of the number of circumstances.
              fraction = 0.5,
              # Fraction of observations to be used for each tree
              replace = FALSE,
              # Whether you sample with replacement when choosing the observations to be used for each tree.
              trace = TRUE,
              # Whether a status bar showing how many of the trees have been calculated is shown in the ouput.
              ntree = 200,
              # Number of trees in the forest. This greatly influences computing time.
              testtype = "Bonferroni",
              # Corrects for multiple hypothesis testing.
              maxsurrogate = 5,
              # Number of surrogate variables to be used to allocate observations into buckets when they are missing in the splitting variable.
              minsplit = j
            )
          ) # The minimum number of observations there has to be in a type before a further split is allowed. Governs the depth of the tree together with alpha.

          # Inner fold out-of-sample predictions
          modeldata$PRED[modeldata$outerfolds != folds] <-
            predict(forest, OOB = TRUE)
          modeldata$AD[modeldata$outerfolds != folds]   <-
            abs(modeldata[modeldata$outerfolds != folds, ][[yvar]] - modeldata$PRED[modeldata$outerfolds !=
                                                                                      folds])
          # Renames the forest so it can be called later on
          assign(paste('forest', folds, j, i, sep = "_"), forest)
          # STORES MAD IN THE RELEVANT CELL
          RESULTS[row, column] <-
            mean(modeldata$AD[modeldata$outerfolds != folds], na.rm = TRUE)

          row = row + 1
        }
        column = column + 1
      }

      #####################################
      ### STORES THE OPTIMAL PARAMETERS ###
      #####################################

      # Finds the cell with the minimum mean absolute deviation and reports the associated mtry and minsplit
      index = which(RESULTS[RESULTS$fold == folds, ] == min(RESULTS[RESULTS$fold ==
                                                                      folds, 3:length(RESULTS)]), arr.ind = TRUE)
      OPT[folds, 2] = RESULTS[RESULTS$fold == folds, 2][index[1]]
      OPT[folds, 3] = as.numeric(colnames(RESULTS)[index[2]])


      # Renames the matrices with the results such that all result matrix will be kept
      assign(paste('RESULTS', yvar, sample, sep = "_"), RESULTS)
      assign(paste('OPT',    yvar, sample, sep = "_"), OPT)
      # End cross-validation loop
    }

    ##############################
    ### RUNNING OPTIMAL FOREST ###
    ##############################

    # In principle, we should retune using the entire dataset. Here I used the average optimal tuning parameters instead
    mtry_opt     = mean(OPT[, 2])
    minsplit_opt = mean(OPT[, 3])

    # Running optimal forest (look at mtry_opt and minsplit_opt)
    forest = cforest(
      as.formula(paste(yvar, '~', features)),
      # Outcome variables and features, defined above,
      data = modeldata,
      # Name of dataset
      control = cforest_control(
        mincriterion = 0,
        # 1 minus p-value, governs when to make splits. With forests, it often makes sense to continue splitting regardless of the p-value
        mtry = mtry_opt,
        # Number of circumstances to consider at each splitting point. Good choices are usually approximiately equivalent to the squareroot of the number of circumstances.
        fraction = 0.5,
        # Fraction of observations to be used for each tree
        replace = FALSE,
        # Whether you sample with replacement when choosing the observations to be used for each tree.
        trace = TRUE,
        # Whether a status bar showing how many of the trees have been calculated is shown in the ouput.
        ntree = 500,
        # Number of trees in the forest. This greatly influences computing time.
        testtype = "Bonferroni",
        # Corrects for multiple hypothesis testing.
        maxsurrogate = 5,
        # Number of surrogate variables to be used to allocate observations into buckets when they are missing in the splitting variable.
        minsplit = minsplit_opt
      )
    ) # The minimum number of observations there has to be in a type before a further split is allowed. Governs the depth of the tree together with alpha.

    # Stores the optimal forest
    # assign(paste('forest',yvar,sample,sep="_"),forest)

    ###############################
    ### PlOTTING OPTIMAL FOREST ###
    ###############################

    # Calculates the importance of each circumstance in predicting the outcome
    # NB: Takes some time to run.
    varimp_absolute = varimp(forest)

    # Adjust the variable importante measure, such that the circumstance with the highest importantce gets a value of 1
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
        '05.figures/varimp/varimp_cirf_',
        noquote(yvar),
        '_',
        noquote(sample),
        '.jpg',
        sep = ""
      ),
      width = 18,
      units = "in"
    )


    #######################################################
    ### (OUT-OF-SAMPLE) PREDICTIONS FROM OPTIMAL FOREST ###
    #######################################################

    # Imputing missing values by putting in the mode/median. For this to work we first need to remove character columns
    datacopy = data[, !sapply(data, is.character)]
    # Now the out-of-sample predictions.
    predname = paste(noquote(yvar), '_cirf_', noquote(sample), '_dire', sep =
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
    write_dta(data, "03.intermediatedata/Predictions/ciforest.dta")

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
    colnames(data)[grepl("lag", names(data))]
  )]

#Works for first extrapolation year
data <- data %>%
  group_by(code, datatype) %>%
  mutate(Y_l_head_cirf_al_grow = laghead + Y_c_head_cirf_al_dire * (year -
                                                                      lagyear)) %>%
  mutate(Y_l_head_cirf_co_grow = laghead + Y_c_head_cirf_co_dire * (year -
                                                                      lagyear)) %>%
  mutate(Y_l_mean_cirf_al_grow = lagmean * (Y_g_mean_cirf_al_dire + 1) ^
           (year - lagyear)) %>%
  mutate(Y_l_mean_cirf_co_grow = lagmean * (Y_g_mean_cirf_co_dire + 1) ^
           (year - lagyear)) %>%
  mutate(Y_l_medi_cirf_al_grow = lagmedi * (Y_g_medi_cirf_al_dire + 1) ^
           (year - lagyear)) %>%
  mutate(Y_l_medi_cirf_co_grow = lagmedi * (Y_g_medi_cirf_co_dire + 1) ^
           (year - lagyear)) %>%
  mutate(Y_l_gini_cirf_al_grow = laggini * (Y_g_gini_cirf_al_dire + 1) ^
           (year - lagyear)) %>%
  mutate(Y_l_gini_cirf_co_grow = laggini * (Y_g_gini_cirf_co_dire + 1) ^
           (year - lagyear))

# Works for remaining extrapolation years
for (reps in seq(1, 21)) {
  data <- data %>%
    group_by(code, datatype) %>%
    mutate(
      Y_l_head_cirf_al_grow = ifelse(
        is.na(Y_l_head_cirf_al_grow),
        lag(Y_l_head_cirf_al_grow) + Y_c_head_cirf_al_dire * (year - lagyear),
        Y_l_head_cirf_al_grow
      )
    ) %>%
    mutate(
      Y_l_head_cirf_co_grow = ifelse(
        is.na(Y_l_head_cirf_co_grow),
        lag(Y_l_head_cirf_co_grow) + Y_c_head_cirf_co_dire * (year - lagyear),
        Y_l_head_cirf_co_grow
      )
    ) %>%
    mutate(Y_l_mean_cirf_al_grow = ifelse(
      is.na(Y_l_mean_cirf_al_grow),
      lag(Y_l_mean_cirf_al_grow) * (Y_g_mean_cirf_al_dire + 1) ^ (year - lagyear),
      Y_l_mean_cirf_al_grow
    )) %>%
    mutate(Y_l_mean_cirf_co_grow = ifelse(
      is.na(Y_l_mean_cirf_co_grow),
      lag(Y_l_mean_cirf_co_grow) * (Y_g_mean_cirf_co_dire + 1) ^ (year - lagyear),
      Y_l_mean_cirf_co_grow
    )) %>%
    mutate(Y_l_medi_cirf_al_grow = ifelse(
      is.na(Y_l_medi_cirf_al_grow),
      lag(Y_l_medi_cirf_al_grow) * (Y_g_medi_cirf_al_dire + 1) ^ (year - lagyear),
      Y_l_medi_cirf_al_grow
    )) %>%
    mutate(Y_l_medi_cirf_co_grow = ifelse(
      is.na(Y_l_medi_cirf_co_grow),
      lag(Y_l_medi_cirf_co_grow) * (Y_g_medi_cirf_co_dire + 1) ^ (year - lagyear),
      Y_l_medi_cirf_co_grow
    )) %>%
    mutate(Y_l_gini_cirf_al_grow = ifelse(
      is.na(Y_l_gini_cirf_al_grow),
      lag(Y_l_gini_cirf_al_grow) * (Y_g_gini_cirf_al_dire + 1) ^ (year - lagyear),
      Y_l_gini_cirf_al_grow
    )) %>%
    mutate(Y_l_gini_cirf_co_grow = ifelse(
      is.na(Y_l_gini_cirf_co_grow),
      lag(Y_l_gini_cirf_co_grow) * (Y_g_gini_cirf_co_dire + 1) ^ (year - lagyear),
      Y_l_gini_cirf_co_grow
    ))
}

# In case some growth predictions are outside of logical intervals
data$Y_l_head_cirf_al_grow[data$Y_l_head_cirf_al_grow > 100] = 100
data$Y_l_head_cirf_co_grow[data$Y_l_head_cirf_co_grow > 100] = 100
data$Y_l_gini_cirf_al_grow[data$Y_l_gini_cirf_al_grow > 70]  = 70
data$Y_l_gini_cirf_co_grow[data$Y_l_gini_cirf_co_grow > 70]  = 70
data$Y_l_head_cirf_al_grow[data$Y_l_head_cirf_al_grow < 0]   = 0
data$Y_l_head_cirf_co_grow[data$Y_l_head_cirf_co_grow < 0]   = 0

# Dropping the lag columns
data = data[, -grep("lag", colnames(data))]

# SAVES DATASET WITH THE FINAL PREDICTIONS
write_dta(data, "03.intermediatedata/Predictions/ciforest.dta")

