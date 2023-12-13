# load libraries
library(pls)
library(tidyverse)

# load dataset
data <- read_csv('final-data.csv')

# identify principal components
components <- prcomp(covariates, scale = TRUE)

# component variance plot
plot(components, main = 'Variance Explained by Each Principal Component')
axis(1, at = seq(0.75, 12.5, by = 1.2), labels = paste('PC', 1:10))

# model cross-validation
pls <- plsr(avg_days_mentally_unhealthy ~., data = data |> select(-fips, -state, -county), scale = TRUE, validation = 'CV')

# validation plot
validationplot(pls, val.type = 'RMSEP',
               main = "PLS Validation Plot")

# elbow criterion for number of components: threshold 0.001
rmsep <- RMSEP(pls)
rmsep
nComp <- 11

# fit final model
final_model <- plsr(avg_days_mentally_unhealthy ~., data = data |> select(-fips, -state, -county), ncomp = nComp, scale = TRUE, validation = 'CV')

# get model coefficients
final_coefficients <- final_model$coefficients[,1,]

# breakdown of each component
component_decomposition <- list()

for (component in 1:11) {
  component_decomposition[[component]] <- final_coefficients[order(abs(final_coefficients[,component]), decreasing = T),component][1:10]
}

component_decomposition