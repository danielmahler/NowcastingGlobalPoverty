# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# project:       Lasso using MI data
# Author:        R.Andres Castaneda and Daniel Mahler
# Dependencies:  The World Bank
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Creation Date:    2020-07-19
# Modification Date:
# Script version:    01
# References:
#
#
# Output:             output
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                   Load Libraries   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

here::i_am("01.code/03f.mi_lasso.R")
library(here)
library(glue)
library(data.table)
library(tidymodels)
library(tidyverse)
library(furrr)
library(skimr)
tidymodels_prefer()

#NOTE: I'll use :: calls so we all know where the functions are coming from



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                   Subfunctions   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

source(here("01.code/00.mi_functions.R"))

# Run long processes?
run_LC          = FALSE
run_inner_tune  = FALSE
run_outer_yhat  = FALSE
run_imp_now     = FALSE

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                   Set up   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Y vars
yvars <-
  c(
    "Y_l_head",
    "Y_c_head",
    "Y_l_mean",
    "Y_g_mean",
    "Y_l_medi",
    "Y_g_medi",
    "Y_l_gini",
    "Y_g_gini"
  )

# yvar <- "Y_l_mean"


# Find number of imputations
mi_files <- list.files(here("02.inputdata/mi/"),
                       pattern = "mi_[0-9]+.dta")


# No. of imputations
m <- str_extract(string = max(mi_files),
                 pattern = "([0-9]+)")


# load original data to leave only actual Y variables
O <- haven::read_dta(here(glue("02.inputdata/FinalInputData.dta"))) %>%
  haven::zap_label() %>%
  mutate(datatype  = ifelse(is.na(datatype), 1, datatype),
         datatype  = as.character(datatype),
         orig_year = year,
         year      = floor(year)) %>%
  select(wbcode = code,
         year,
         datatype,
         starts_with("Y_l"))


yvars_O     <- grep("^Y_", names(O), value = TRUE)
new_yvars_O <- gsub("_l_", "_o_", yvars_O)

O <- O %>%
  rename_at(all_of(yvars_O), ~new_yvars_O)


# load working data
W <- map(.x = mi_files,
         .f = ~{
           w <- haven::read_dta(here(glue("02.inputdata/mi/{.x}"))) %>%
             haven::zap_label()

           # remove class label from variables
           nw <- names(w)
           for (i in seq_along(nw)) {
             nn <- nw[i]

             attr(w[[nn]], "class") <- NULL
             attr(w[[nn]], "format.stata") <- NULL

           }

           # merge with original variables
           w <- joyn::merge(w, O,
                       by         = c("wbcode", "year", "datatype"),
                       match_type = "1:1",
                       reportvar  = FALSE,
                       verbose    = FALSE)
           setDT(w)
           w    <- w[, temp_id := .I]
           return(w)
         })


# anchor for sbcodes and temp_id
wbcode_id <- map(.x = W,
                 ~{
                   .x[, c("wbcode", "temp_id")]
                 })


# check number of missing values
# walk(W,
#      ~{
#        wvars <- names(.x)[!grepl("^sample|^weight", names(.x)) ]
#        dd <- dim(na.omit(.x[, ..wvars ]))
#        print(dd)
#      })

#
#
# vars  <-  names(O)[names(O) %in% wvars]
#
# O <- O %>%
#   select(all_of(vars))
#
# dim(na.omit(O))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  data manipulation     ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# create data frame with different possibilities in the parameters
tb_par <- expand_grid(yvar = yvars,
                      sample = c("al", "co"))

# list of combinations  just with the right variables
# tb_par <- tb_par[3:4, ]

if (run_LC) {
  LC <- pmap(.l = tb_par,
             .f = create_yx,
             W  = W)
  # Add names``
  names(LC) <- paste(tb_par$yvar, tb_par$sample, sep = "-")

  readr::write_rds(LC, here("02.inputdata/mi/LC.rds"))
  pushoverr::pushover("finish creating LC")

} else {

  LC <- readr::read_rds(here("02.inputdata/mi/LC.rds"))

}




# Failed subsets
failed_LC <- LC %>%
  keep(is.null) %>%
  names()

# list of imputed data for which there was no error in `create_xy`. Each object
# of this list should be a list of five elements. Once for each imputation
good_yvars <- names(LC)[!(names(LC)  %in% failed_LC)]
good_LC <- LC[good_yvars]


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Prepare workflow               ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Prepare data --------


# yvar <- "Y_l_mean"
# x <- good_LC[[paste0(yvar, "-al")]]

nest_recipe <-
  map(.x = good_yvars,
      .f = ~{
        set.seed(908070)

        # get name of yvar for recipe
        yvar   <- gsub("(.*)(\\-[a-z]{2}$)", "\\1", .x)


        imp <- good_LC[[.x]] %>% # the five imputations  in .x
          map(~{

            # Nested resampling
            x_nest <- rsample::nested_cv(data    = .x$training,
                                         outside = vfold_cv(data = .x, v = 5),
                                         inside  = vfold_cv(data = .x, v = 5))

            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            ## build the recipe --------
            # Here .x is each imputation in each outcome vriable data set.
            recipe <- build_recipe(yvar, .x$training)

            return(list(object = x_nest,
                        recipe = recipe))
          })

        names(imp) <- paste0("imp_", 1:length(imp))
        return(imp)

      })

names(nest_recipe) <- good_yvars

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# execute the models  to find tune the hyper parameters ---
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if (run_inner_tune) {

  plan(multisession, workers = 4)
  inner_tune <-
    map(.x = good_yvars,
        .f = ~ {

          cli::cli_progress_step("Working on {.x}")
          yvar   <- gsub("(.*)(\\-[a-z]{2}$)", "\\1", .x)

          # Extract first level. `y` has five elemts, one for each imputation.
          # Each iteration the y is one different model varying `yvars`
          y         <- nest_recipe[[.x]]
          imp_names <- names(y)

          res <- map(.x = cli::cli_progress_along(imp_names),
                     .f = ~ {
                       z   <- y[[.x]]$object
                       rec <- y[[.x]]$recipe

                       future_map(
                         # .x = z$object$inner_resamples,
                         .x       = z$inner_resamples,
                         .f       = summarize_tune_results,
                         levels   = c(10, 1), # change to c(1,3) for testing
                         recipe   = rec,
                         y        = yvar,
                         .options = furrr_options(seed = 123)
                       )
                     })

          names(res) <- imp_names
          return(res)

        })

  names(inner_tune) <- good_yvars
  readr::write_rds(inner_tune, here("02.inputdata/mi/inner_tune.rds"))
  pushoverr::pushover("finish inner tuning")
} else {

  # inner_tune <- readr::read_rds(here("02.inputdata/mi/l_tuned_results.rds"))
  inner_tune <- readr::read_rds(here("02.inputdata/mi/inner_tune.rds"))

}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# find best tuning and apply to outer folds   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Get the best hyper parameters in each inner fold

metric <- "mae"

l_hyperpar_vals <-
  map(inner_tune,
      ~{
        imp_names <- names(.x)
        res <-
          map(.x,
            ~{
              vars <- c(metric, "penalty", "mixture")
              map_df(.x, ~{.x[which.min(.x[[metric]]),]}) %>%
                select(all_of(vars))
            })
        names(res) <- imp_names
        return(res)

      })


# To select the hyper parameters in each imputation, we could either tune the
# parameters in the outer folds and then run the best parameter in each
# imputation or we could get the average of the tuned parameters in the inner
# folds and use it  in the imputation. We will do the latter.

# Get average of parameters among inner folds
params <-
  l_hyperpar_vals %>%
  map( ~ {
    imp_names <- names(.x)
    res <-
      map(.x ,
          ~ {
              .x %>%
                ungroup() %>%
                summarise(across(c(penalty, mixture), mean))
            })
    names(res) <- imp_names
    return(res)
  })

#
# names(params) <- names(l_hyperpar_vals)


# bind (merge) best average hyperpar to each outer fold

l_outer <-
  map2(.x = nest_recipe,
       .y = params,
       .f = ~{
        y <- .y
         map2(.x = .x,
              .y = .y,
              .f = ~{
                vars <- c("id", "inner_resamples") #vars to remove

                l <- bind_cols(.x$object, .y) %>%
                  select(!all_of(vars)) %>%
                  rename(object = splits) %>% # change name to make it match in get_rmse()
                  mutate(recipe = list(.x$recipe))

                return(l)
              })
       })



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# calculate RMSE for outer folds using best hyper parameters   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if (run_outer_yhat) {
  temp_good_yvars <- good_yvars

  # For testing
  #  good_yvars <- temp_good_yvars
  # good_yvars <- good_yvars[1]
  # good_yvars <- good_yvars[1:2]

  outer_yhat <- good_yvars %>%
    map(~{
      cli::cli_progress_step("Working on {.x}")
      yvar   <- gsub("(.*)(\\-[a-z]{2}$)", "\\1", .x)

      # Extract first level. `y` has five elements, one for each imputation.
      # Each iteration the y is one different model varying `yvars`
      y         <- l_outer[[.x]]
      imp_names <- names(y)

      res <- map2(.x = cli::cli_progress_along(imp_names),
                  .y = wbcode_id,
                  .f = ~{
                   z <- as.list(y[[.x]]) # we need a list for pmap

                    # Here we run the lasso in the outer folds using the tuned
                    # penalty and  mixture found in the inner folds. Yet, this
                    # is done fold by fold,  not for the whole imputed data set

                    r <- pmap_df(.l = z,
                                 .f = get_yhat,
                                 y = yvar)

                    # This might not be necessary, but just in case there are repeated
                    # observations in the outer folds
                    r <- r %>%
                      group_by(temp_id, datatype) %>%
                      summarise_all(mean) %>%
                      ungroup() %>%

                      # join with country codes
                      joyn::merge(.y,
                                  by        = "temp_id",
                                  keep      = "left",
                                  reportvar = FALSE,
                                  verbose   = FALSE
                                  ) %>%
                      relocate(temp_id, wbcode , datatype, year)

                    return(r)

                    # get a tibble with rmse bind to it
                  })
      names(res) <- imp_names
      return(res)
    })

  names(outer_yhat) <- good_yvars

  readr::write_rds(outer_yhat, here("02.inputdata/mi/outer_yhat.rds"))
  pushoverr::pushover("finish outer fold prediction")

} else {

  # inner_tune <- readr::read_rds(here("02.inputdata/mi/l_tuned_results.rds"))
  outer_yhat <- readr::read_rds(here("02.inputdata/mi/outer_yhat.rds"))
}


yhat <-
  outer_yhat %>%
  map(~{

    df_app <- data.table::rbindlist(.x, use.names = TRUE, fill = TRUE)

    # get average of imputations per observation
    ave <-
      df_app %>%
      group_by(temp_id, wbcode, year, datatype) %>%
      summarise_all(mean, na.rm = TRUE) %>%
      mutate(sample = "training")
  })


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# run nowcasting for all data frames   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# training  <- good_LC$`Y_l_head-al`$imp_1$training
# testing   <- good_LC$`Y_l_head-al`$imp_1$testing
# recipe    <- nest_recipe$`Y_l_head-al`$imp_1$recipe
# penalty   <- params$`Y_l_head-al`$imp_1$penalty
# mixture   <- params$`Y_l_head-al`$imp_1$mixture
# y         <- gsub("(.*)(\\-[a-z]{2}$)", "\\1", names(nest_recipe[1]))

# good_yvars <- good_yvars[1:2]

if (run_imp_now) {


  imp_now <-
    good_yvars  %>%
    map( ~ {
      # first level of variables is Yvar
      g <- good_LC[[.x]]
      n <- nest_recipe[[.x]]
      p <- params[[.x]]

      y         <- gsub("(.*)(\\-[a-z]{2}$)", "\\1", .x)
      imp_names <- names(g)
      res <-
        map2(.x = imp_names,
             .y = wbcode_id,
             ~ {
               # second level of variables is imputation
               g2 <- g[[.x]]
               n2 <- n[[.x]]
               p2 <- p[[.x]]

               # parameters of functions

               training  <- g2$training
               testing   <- g2$testing
               recipe    <- n2$recipe
               penalty   <- p2$penalty
               mixture   <- p2$mixture

               now <- nowcast(
                 training = training,
                 testing  = testing,
                 recipe   = recipe,
                 y        =  y,
                 penalty  = penalty,
                 mixture  = mixture
               )

               now <- now %>%
                 # join with country codes
                 joyn::merge(
                   .y,
                   by        = "temp_id",
                   keep      = "left",
                   reportvar = FALSE,
                   verbose   = FALSE
                 ) %>%
                 relocate(temp_id, wbcode , datatype, year)

               return(now)

             })
      names(res) <- imp_names
      return(res)
    })
  names(imp_now) <- good_yvars

  readr::write_rds(imp_now, here("02.inputdata/mi/imp_now.rds"))
  pushoverr::pushover("finish outer fold prediction")

} else {
  # inner_tune <- readr::read_rds(here("02.inputdata/mi/l_tuned_results.rds"))
  imp_now <- readr::read_rds(here("02.inputdata/mi/imp_now.rds"))
}

# get average of esimations
now <-
  imp_now %>%
  map( ~ {
    df_app <- data.table::rbindlist(.x, use.names = TRUE, fill = TRUE)

    # get average of imputations per observation
    ave <-
      df_app %>%
      group_by(temp_id, wbcode, year, datatype) %>%
      summarise_all(mean, na.rm = TRUE) %>%
      mutate(sample = "nowcast")

    return(ave)

  })



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Put all prediction together   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

pred_y <-
  map(.x = good_yvars,
      .f = ~ {
        # predicted y
        py <- bind_rows(now[[.x]],
                        yhat[[.x]])

        # Fix variable name
        y <- gsub("(.*)(\\-[a-z]{2}$)", "\\1", .x)
        z <- gsub("(.*\\-)([a-z]{2}$)", "\\2", .x)

        oldvars <- grep("_p?l$", names(py), value = TRUE)
        newvars <- gsub(y, paste0(y, "_", z), oldvars)

        setnames(py, oldvars, newvars)
        return(py)

      })  %>%
  reduce(
    joyn::merge,
    by          = c("temp_id", "wbcode", "year", "datatype"),
    match_type  = "1:1",
    reportvar   = FALSE,
    verbose     = FALSE
  )

pred_y <- pred_y %>%
  relocate(c(wbcode, year, datatype, sample)) %>%
  arrange(temp_id) %>%
  select(-temp_id) # remove temporal id.



# Get original data frame
orig <-
  haven::read_dta(here(glue("02.inputdata/FinalInputData.dta"))) %>%
  # Get rid of attributes
  haven::zap_label() %>%
  # filter(sample_now  == 1) %>%  # This has to be fixed.
  mutate(datatype  = ifelse(is.na(datatype), 1, datatype),
         datatype  = as.character(datatype),
         orig_year = year,
         year      = floor(year)) %>%
  select(wbcode = code,
         economy,
         orig_year,
         year,
         datatype,
         starts_with("Y_"),
         starts_with("weight"),
         starts_with("sample"))

# Join original data base with predicted data.
orig_pred <-
  joyn::merge(x          = orig,
              y          = pred_y,
              by         = c("wbcode", "year", "datatype"),
              match_type = "1:1",
              reportvar = FALSE)



readr::write_rds(orig_pred, here("02.inputdata/mi/pred_lasso_mi.rds"))
haven::write_dta(orig_pred, here("02.inputdata/mi/pred_lasso_mi.dta"))
haven::write_dta(orig_pred, here("03.intermediatedata/Predictions/lasso.dta"))
pushoverr::pushover("Finish running everything")


