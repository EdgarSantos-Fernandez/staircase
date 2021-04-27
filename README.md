# staircase R package
For examples and datasets see https://github.com/EdgarSantos-Fernandez/hakuna

These statistical methods were developed in the paper: "Understanding the reliability of citizen science observational data using item response models" published in Methods in Ecology and Evolution. https://doi.org/10.1111/2041-210X.13623

# Installation:
devtools::install_github("https://github.com/EdgarSantos-Fernandez/staircase")


# Usage
library("staircase")

fit <- ir_spat(formula = site ~ -1 + Moving + Babies, # covariates affecting the difficulty 
               data = data, # a data frame
               spat_model = 'exp', # spatial covariance matrix 
               itemtype = '1PLUS', # item response model 
               abil = 'user', # participants ids
               diff = 'id', # item id
               y = 'correct', # binary response variable 
               coords = c("LocationX", "LocationY"), # coordinates
               iter = 8000, 
               warmup = 4000,
               chains = 3,
               refresh = 100,
               seed = seed 
)

See the complete example: https://github.com/EdgarSantos-Fernandez/hakuna/blob/main/simple_example.pdf