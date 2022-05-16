## Introduction

This repository contains the code necessary to replicate the paper [Nowcasting Global Poverty](https://openknowledge.worldbank.org/handle/10986/36636) by [Daniel Gerszon Mahler](https://sites.google.com/view/danielmahler/), [R. Andres Castaneda Aguilar](https://randrescastaneda.rbind.io/), and [David Newshouse](https://www.worldbank.org/en/about/people/d/david-newhouse). The repository also contains the final poverty predictions from the paper ([Predictions.csv](https://github.com/danielmahler/NowcastingGlobalPoverty/blob/master/Predictions.csv)) as well as the errors of those predictions ([Errors.csv](https://github.com/danielmahler/NowcastingGlobalPoverty/blob/master/Errors.csv)).

In what follows, we will guide you through the variable names in those files.

## **Nomenclature for variable names**

**Sample variables:**

-   *sample_al_l*: All rows with a survey estimate that can be used for predicting levels of variables
-   *sample_al_g*: All rows with a survey estimate that can be used for predicting growth/change of variables (equivalent to sample_al_l except for the first estimate for each country-datatype combination
-   *sample_co_l:* All rows with a survey estimate comparable to the previous one for the code-datatype that can be used for predicting levels of variables
-   *sample_co_g*: All rows with a survey estimate comparable to the previous one for the code-datatype that can be used for predicting growth/change of variables
-   *sample_now*: All rows used for nowcasting poverty beyond the latest survey estimate.

**Weight variables:**

Each of the four weight variables corresponds to one of the four first sample variables above. The weights add to 1 per country.

**Survey estimates:**

All survey estimates have the following format: *Y\_[outcome]. [outcome]* can be one of six options:

-   *l_head:* The level of the headcount rate (ranging from 0 to 100)
-   *c_head*: Annualized change in the headcount rate (in percentage points)
-   *l_mean:* The level of mean welfare (monthly 2011 USD)
-   *g_mean:* Annualized growth in mean welfare (0.01 = 1%)
-   *l_gini:* The level of the Gini coefficient (ranging from 0 to 100)
-   *g_gini:* Annualized growth in the Gini coefficient (0.01 = 1%)

**Each variable from a prediction takes the following format:**

*Y\_[outcome]\_[method]\_[comparability]\_[conversion\#1]\_[conversion\#2]*

[outcome] is defined as above.

[method] refers to the method used to obtain the predictions. It can take the following options:

-   *lagh*: Use the lagged headcount rate
-   *ngdp*: Applying growth from gdp with no passthrough rate
-   *ngni*: Applying growth from gni with no passthrough rate
-   *nhfc*: Applying growth from hfce with no passthrough rate
-   *pgdp*: Applying growth from gdp with a passthrough rate
-   *pgni*: Applying growth from gni with a passthrough rate
-   *phfc*: Applying growth from hfce with a passthrough rate
-   *dgdp*: Applying growth from gdp with a datatype specific passthrough rate
-   *dgni*: Applying growth from gni with a datatype specific passthrough rate
-   *dhfc*: Applying growth from hfce with a datatype specific passthrough rate
-   *rgdp*: Applying growth from gdp with passthrough rate by datatype and region
-   *igdp*: Applying growth from gdp with passthrough rate by datatype and income group
-   *sgdp*: Applying growth from gdp with passthrough rate by datatype and growth sign
-   *rlas*: Regular lasso
-   *plas*: Post-lasso
-   *carf*: CART random forest
-   *cirf*: Conditional inference random forest
-   *grbo*: Gradient boosting
-   *perf*: Hypothetical perfect predictions

[comparability] refers to the spells used. It can take the following options:

-   *al*: All spells
-   *co*: Only comparable spells

[conversion\#1] refers to how [outcome] predictions are obtained. It can take the following options:

-   *dire*: The outcome variable is predicted directly
-   *grow*: The outcome variable is derived from first predicting growth in the variable
-   *dnmu*: By predicting the mean and assuming distribution-neutrality
-   *gicl*: By predicting the mean and Gini and applying a linear growth incidence curve
-   *gicc*: By predicting the mean and Gini and applying a convex growth incidence curve
-   *lnmu*: By predicting the mean and Gini and assuming a log normal distribution
-   *llmu*: By predicting the mean and Gini and assuming a log logistic distribution

[conversion\#2] refers to how the mean/median and the Gini are predicted when both are used in [conversion\#1]:

-   *dire*: The mean and Gini are predicted directly
-   *grow*: The mean and Gini are predicted by predicting the growth in these variables

**Each error from a prediction takes the following format:**

[error]\_Y\_[outcome]\_[method]\_[comparability]\_[conversion\#1]\_[conversion\#2]

Everything but [error] are as specified above.

[error] refers to the loss function used. [error] can take the following values:

-   *se*: Squared error
-   *ad*: Absolute deviation
-   *dv*: Deviation (survey estimate minus prediction)
-   *oo*: Trend over-optimistically predicted
-   *op*: Trend over-pessimistically predicted

**Examples**

Examples of the entire nomenclature being used:

-   *Y_g\_mean_plas_al_dire:* Predicted growth in mean consumption using the post-lasso using all spells. The predictions are obtained by predicting growth in the mean directly.
-   *Y_l\_head_grbo_co_gicl_grow:* Predicting growth in the Gini and growth in the mean using gradient boosting using only comparable spells. Converting the predicted mean and Gini into predicted headcount rates using linear growth incidence curves.
-   se_Y\_l_head_dgdp_al_dnmu: The squared difference between the true poverty rate and the predicted poverty rate from growing the mean with growth in real GDP per capita while applying a passthrough rate by datatype.
