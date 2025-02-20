library(lme4)     
library(DHARMa)      
library(ggplot2)     
library(readxl)      
library(sjPlot)     

# path to data...
file_path <- "enter data file path"

# load the dataset and select the datasheet
data <- read_excel(file_path, sheet = "Mortality Data")

# exclude the 'Cell' treatment
data_no_cell <- data[data$Treatment != "Cell", ]

# Fit the final GLMM (WITHOUT cell treatment and interaction but WITH random intercept & slope for best AIC score)
glmm_final <- glmer(
  cbind(Alive, Number_Individuals_Start_Experiment - Alive) ~ 
    Treatment + Days_Post_Induction + (1 + Days_Post_Induction | Vessel),
  family = binomial(link = "logit"),
  data = data_no_cell,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)

# give model results
summary(glmm_final)

# diagnostic checks 
residuals_final <- simulateResiduals(fittedModel = glmm_final, n = 1000)

# residual diagnostics
plot(residuals_final)

# overdispersion
testDispersion(residuals_final)

# uniformity of residuals
testUniformity(residuals_final)

# zero inflation
testZeroInflation(residuals_final)

# model summary with additional information 
tab_model(glmm_final, show.re.var = TRUE, show.icc = TRUE, show.aic = TRUE)

# get your confidence intervals!
confint(glmm_final, method = "Wald")  # Uses Wald method for CI
