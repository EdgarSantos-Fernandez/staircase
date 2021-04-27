
#' A simple modeling function using a formula and data
#'
#' @param formula A formula as in lm()
#' @param data A data.frame containing the elements specified in the formula
#' @return A list of matrices
#' @importFrom stats model.matrix model.response
#' @export
#' @author Jay ver Hoef
#' @examples
#' #options(na.action='na.pass')
#' #out_list = mylm(formula = y ~ X1 + X2 + X3, data = data)
#'

mylm <- function(formula, data) {
  # get response as a vector
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data"), names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf$drop.unused.levels <- TRUE
  mf[[1L]] <- as.name("model.frame")
  mf <- eval(mf, parent.frame())
  y <- as.vector(model.response(mf, "numeric"))
  # create design matrix
  X <- model.matrix(formula, data)
  # return a list of response vector and design matrix
  return(list(y = y, X = X))
}


# A simple modeling function using a formula and data
# by Mitzi Morris

mungeCARdata4stan = function(adjBUGS,numBUGS) {
  N = length(numBUGS);
  nn = numBUGS;
  N_edges = length(adjBUGS) / 2;
  node1 = vector(mode="numeric", length=N_edges);
  node2 = vector(mode="numeric", length=N_edges);
  iAdj = 0;
  iEdge = 0;
  for (i in 1:N) {
    for (j in 1:nn[i]) {
      iAdj = iAdj + 1;
      if (i < adjBUGS[iAdj]) {
        iEdge = iEdge + 1;
        node1[iEdge] = i;
        node2[iEdge] = adjBUGS[iAdj];
      }
    }
  }
  return (list("N"=N,"N_edges"=N_edges,"node1"=node1,"node2"=node2));
}



#' Fits an spatial item response model using Stan
#' It requires
#' It requires
#' Missing values are not allowed in the response/covariates.
#'
#' @param formula A formula as in lm()
#' @param data A long data frame containing the locations, dates, covariates and the response variable. It has to have the locID and date. No missing values are allowed in the covariates.
#' @param spat_model A string with the spatial correlation structure based on Euclidean distance models. The options are: exponential (exp), spherical (sph), gaussian (gau), car (Conditional Autoregressive). For no spatial autocorrelation use "none".
#' @param itemtype A string with the kind of item response model. Options: 3PLUS, 2PLUS, 1PLUS, 3PL, 2PL and 1PL. PLUS and PL models are spatial and non-spatial respectivelly.
#' @param abil A column indicating the user id
#' @param diff A column indicating the item id
#' @param y binary response variable (1 = correct, 0 = otherwise).
#' @param coords A vector with the lon and lat for Euclidean distance models.
#' @param iter Number of iterations
#' @param warmup Warm up samples
#' @param chains Number of chains
#' @param seed (optional) A seed for reproducibility
#' @param loglik If the log-likelihood will be computed
#' @param refresh The refresh rate showing the progress
#' @return A list with the model fit
#' @details Missing values are not allowed in the covariates and they must be imputed before using ir_spat(). Many options can be found in https://cran.r-project.org/web/views/MissingData.html
#' @export
#' @importFrom dplyr mutate %>% distinct left_join case_when
#' @importFrom plyr .
#' @importFrom rstan stan
#' @importFrom stats dist
#' @importFrom rgeos gIntersection
#' @importFrom dismo voronoi
#' @examples
#'# ir_spat(formula = site ~ -1 + Standing + Resting + Moving + Eating + Interacting + Babies,
#'#                    data = s,
#'#                    spat_model = 'car',
#'#                    itemtype = '3PLUS',
#'#                    iter = 40,
#'#                    warmup = 20,
#'#                    chains = 3,
#'#                    refresh = 100
#'#                    )



ir_spat <- function(formula = formula,
                    data = data,
                    spat_model = 'exp',
                    itemtype = '3PLUS',
                    abil = 'user',
                    diff = 'id',
                    y = y,
                    coords = coords,
                    iter = 4000,
                    warmup = 2000,
                    chains = 3,
                    seed = seed,
                    refresh = max(1000/100, 1),
                    loglik = F){
  # to add:
  # @param slope A string with the column associated with the slope parameter e.g.
  # @param guess A string with the column associated with the guessing parameter e.g.


  lon <- lat <- NULL
  if(missing(y)){
    stop('Need to define the binary response variable y')
  }

  if(!missing(y)){
    data$correct <- data[,names(data) == y]
  }

  # checks
  if(missing(spat_model)){
    spat_model = 'exp'
    print('No spatial structure defined. Using an exponential model.
          Other options are: sph, gau, car. For no spatial autocorrelation use "none"')
  }

  if(missing(itemtype)){
    itemtype = '3PLUS'
    print('No item response model defined. Using a 3PLUS.
          Other options are: 2PLUS, 1PLUS, 3PL, 2PL and 1PL')
  }

  # Cov
  if(spat_model == 'none' &
     (itemtype == '1PLUS'| itemtype == '2PLUS' | itemtype == '3PLUS') ) stop("Cannot use spat_model == none with itemtype = 1PLUS, 2PLUS or 3PLUS. Use itemtype = 1PL, 2PL or 3PL")

  if((spat_model == 'exp' | spat_model ==  'car') &
     (itemtype == '1PL'| itemtype == '2PL' | itemtype == '3PL') ) stop("Cannot use spat_model == exp|car with itemtype = 1PL, 2PL or 3PL. Use itemtype = 1PLUS, 2PLUS or 3PLUS")

  if(missing(abil)){
    stop("Need to define the column containg the used id used to compute the ability")
  }

  if(missing(diff)){
    stop("Need to define the column containg the image/item id used to compute the difficulty")
  }

  if(missing(seed)) seed <- sample(1:1E6,1,replace=T)

  if(missing(coords)){ stop("Please, specify the columns in the data frame with
                                     the latitude and longitude (c('lon', 'lat'))") }

  data$lon <- data[,names(data) == coords[1]]
  data$lat <- data[,names(data) == coords[2]]



  if(missing(formula)){
    covariates <- 'no_cov'
  }

  if(!missing(formula)){
    covariates <- 'use_cov'
  }


# PARAMETERS
annot <- data[,abil]
id <- data[,diff]


#  model <- 'exp'
#  sph
#  gau
#  car


  cor_ed <- case_when(spat_model == "exp" ~ 1,
                      spat_model == "sph" ~ 2,
                      spat_model == "gau" ~ 3,
                      TRUE ~ 5)

  cor_ed <- sort(cor_ed)[1]

  cor_car <- case_when(spat_model == "car" ~ 1,
                      TRUE ~ 5)
  cor_car <- sort(cor_car)[1]


  data_com <-  'data {
    int<lower=1> N;                       // num elicitation points classified
    int<lower=1> M;                       // number of images and unique locations.
    int<lower=1> Nindiv;                  // number users
    int<lower=1> K;
    int<lower=1,upper=M> id[N];           // image id
    int<lower=1,upper=Nindiv> annot[N];   // user id
    int<lower=0,upper=1> y[N];            // outcome variable

    matrix[N,K] X ; //

    matrix[M, M] e ; // Euclidean dist mat
    matrix[M, M] I ; // diag matrix
    real<lower=1> alpha_max ;

    int<lower=1> Nspecies;                       // num species
    int<lower=1,upper=Nspecies> species_id[N];           // species id

    matrix[M, M] W ; // Adj mat
    //int<lower=0> N_edges;
    //int<lower=1, upper=N> node1[N_edges]; // node1[i] adjacent to node2[i]
    //int<lower=1, upper=N> node2[N_edges]; // and node1[i] < node2[i]
  }'


  param_com <- '
  parameters {

    vector[K] beta; // regression coef
    vector[Nindiv] abil;            // ability in the spatial model
    real<lower=0>sigma_abil;        // sd of the abilities spatial model
    vector<lower=0>[M] alpha;       // slope in the spatial model
    real<lower=0> sigma_alpha;       // sd of the slope

    vector<lower=0,upper=1>[Nspecies] eta; // guessing in the spatial model

    vector[Nspecies] diff_species; // difficulty based on species
    real<lower=0>sigma_diff_species; //

    vector[M] difficulty;           // difficulty in the spatial model
    '


  param_ed <- '
    real<lower=0> sigma_ed;
    real<lower=0> alpha_ed; // range of the Euclidean dist model
    real<lower=0> sigma_nug;
    '

  param_car <- '
    real<lower=0> sigma_nug;
    real<lower = 0> tau;
    real<lower = 0, upper = 1> rho; // amount of temporal dependence
    //real<lower=0> tau_phi;          // precision of spatial effects
    //vector[M] phi;                  // spatial effects
    '

  param_nospat <- '
   // vector[M] difficulty;           // difficulty in the spatial model
    real<lower=0>mu_diff;           // mean of the difficulty
    real<lower=0>sigma_diff;        // sd of the difficulty
  '

  tparam_com <- '
    transformed parameters {
    vector[M] mu; // mean
   '

  tparam_ed <- '
    real<lower=0> var_ed; //  Euclidean dist var
    matrix[M, M] C_ed ;// Euclidean cov
    real<lower=0> var_nug; // nugget

  '

  tparam_car <- '
  real<lower=0> var_nug; // nugget
  //  vector[M] difficulty;           // difficulty in the spatial model
 //   real<lower=0> sigma_phi = inv(sqrt(tau_phi));  // convert precision to sigma
 //   difficulty = phi * sigma_phi;
 '


  tparam_com2 <- '
  for (i in 1:N){
    mu[id[i]] = X[i,] * beta;
  }'

  tparam_com_nocovs <- ' // if no covariates
    for (i in 1:N){
    mu[id[i]] = 0;
    }
  '



  tparam_ed2 <- '
    var_nug = sigma_nug ^ 2; // variance nugget
	  //Euclidean distance models start
    var_ed = sigma_ed ^ 2; // var Euclidean dist
      C_ed = var_ed * exp(- 3 * e / alpha_ed); // exponential model
    //Euclidean distance models end
'

  tparam_car2 <- '
   var_nug = sigma_nug ^ 2; // variance nugget
    //sigma_phi = inv(sqrt(tau_phi));  // convert precision to sigma
    //difficulty = phi * sigma_phi;
'
  model_com <- '
    model {
    vector [N] pijk;                // prob of correct classification in the spatial model
    abil ~ normal(0, sigma_abil);  //  ; normal(0, 1) // informative prior for ability in spatial model

    sigma_abil ~ uniform(0,10);  // cauchy(0,5);      // flat prior on the abilities sd

   // mean(abil) ~ normal(0,0.001); // soft sum-to-zero constraint on abil

    alpha ~ normal(1,sigma_alpha); //lognormal(0,sigma_alpha);            //    // discrimination. lognormal distribution. Does not work well
    sigma_alpha ~  uniform(0,10);   //cauchy(0,5);      // // sd discrimination. Half Cauchy since sigma_alpha is defined above as possitive. See Stan manual

    eta ~ beta(1,5);         //(2,14) (5,20) (1,5) prior on guessing spatial

    diff_species ~ normal(0, sigma_diff_species);
    sigma_diff_species ~  cauchy(0,5); //uniform(0,10);       // flat prior on the diff_species sd
    mean(diff_species) ~ normal(0,0.001); // soft sum-to-zero constraint on diff_species

  '

  model_1PL <- '
    for (i in 1 : N) {
      pijk[i] = inv_logit( (abil[annot[i]] -
      (diff_species[species_id[i]] + difficulty[id[i]] )) ) ;
      y[i] ~ bernoulli( pijk[i]);
    }
  '

  model_2PL <- '
    for (i in 1 : N) {
      pijk[i] = inv_logit(alpha[id[i]] * (abil[annot[i]] -
      (diff_species[species_id[i]] + difficulty[id[i]] )) ) ;
      y[i] ~ bernoulli( pijk[i]);
    }
  '

  model_3PL <- '
    for (i in 1 : N) {
      pijk[i] = eta[species_id[i]] + (1 - eta[species_id[i]]) *
      (inv_logit(alpha[id[i]] * (abil[annot[i]] -
      (diff_species[species_id[i]] + difficulty[id[i]] )) ) );
      y[i] ~ bernoulli( pijk[i]);
    }
  '



  model_ed <- '
    sigma_ed ~ uniform(0,100); // sd Euclidean dist
    alpha_ed ~ uniform(0, alpha_max); // Euclidean dist range
    sigma_nug ~ uniform(0, 50); //

    target += multi_normal_cholesky_lpdf(difficulty | mu,
    cholesky_decompose(C_ed + var_nug * I + 1e-9) );
  '

  model_car <- '
    sigma_nug ~ uniform(0, 50); //
    tau ~ gamma(2, 2);
    //target += multi_normal_cholesky_lpdf(difficulty | mu,
    //cholesky_decompose(tau * (I - rho * W) + var_nug * I + 1e-9) );

   difficulty ~ multi_normal_prec(mu, tau * (I - rho * W) + var_nug * I + 1e-9); // using the precision

    //tau_phi ~ gamma(1, 1) ;         // NOTE:  no prior on phi_raw, it is used to construct phi
    // the following computes the prior on phi on the unit scale with sd = 1
   // target += -0.5 * dot_self(phi[node1] - phi[node2]);
  '

  model_nospat <- '
      difficulty ~ normal(mu_diff, sigma_diff); //informative true prior for ability in Rasch model
      mu_diff ~ normal(0,5);          // prior on the mean difficulty
      sigma_diff ~ cauchy(0,5);       // prior on the sd difficulty
  '

  generated_quantities_com <- 'generated quantities { // to compute the log likelihood
    vector [N] lin;
    vector[N] log_lik;
    for (i in 1 : N) {
  '

  generated_quantities_1PL <- '
      lin[i] = inv_logit(abil[annot[i]] -
      (diff_species[species_id[i]] + difficulty[id[i]] )) ;
      log_lik[i] = bernoulli_log(y[i], lin[i]);
    }
  }'

  generated_quantities_2PL <- '
      lin[i] = inv_logit(alpha[id[i]] * (abil[annot[i]] -
      (diff_species[species_id[i]] + difficulty[id[i]] )) );
      log_lik[i] = bernoulli_log(y[i], lin[i]);
    }
  }'

  generated_quantities_3PL <- '
      lin[i] = eta[species_id[i]] + (1 - eta[species_id[i]]) *
        (inv_logit(alpha[id[i]] * (abil[annot[i]] -
        (diff_species[species_id[i]] + difficulty[id[i]] )) ) );
      log_lik[i] = bernoulli_log(y[i], lin[i]);
    }
  }'

  if(cor_ed %in% 1:3) print('Fitting an Euclidean distance model (cor_ed)')
  if(cor_car %in% 1:3) print('Fitting a CAR model (cor_car)')
  if(spat_model == 'none') print('Fitting a no spatial model (no_spat)')

  # builds a string with the model
  ir_model <- paste(
    data_com,

    param_com,

    if(cor_ed %in% 1:3) param_ed,
    if(cor_car %in% 1:3) param_car,

    if(spat_model == 'none') param_nospat,
    '}',

    tparam_com,
    if(cor_ed %in% 1:3)tparam_ed,
    if(cor_car %in% 1:3)tparam_car,

    tparam_com2,

    if(cor_ed %in% 1:3) tparam_ed2,

    if(cor_car %in% 1:3) tparam_car2,

    '}',
    model_com,

    if(itemtype %in% '1PLUS' | itemtype %in% '1PL' ) model_1PL,
    if(itemtype %in% '2PLUS' | itemtype %in% '2PL' ) model_2PL,
    if(itemtype %in% '3PLUS' | itemtype %in% '3PL' ) model_3PL,

    if(cor_ed %in% 1:3) model_ed,
    if(cor_car %in% 1:3) model_car,
    if(spat_model == 'none') model_nospat,
    '}',

    generated_quantities_com,
    if(itemtype %in% '1PLUS' | itemtype %in% '1PL' ) generated_quantities_1PL,
    if(itemtype %in% '2PLUS' | itemtype %in% '2PL' ) generated_quantities_2PL,
    if(itemtype %in% '3PLUS' | itemtype %in% '3PL' ) generated_quantities_3PL
   )
  # see model in notepad++. replace \n by @. then replace back @ by \r\n and tick Extend()


  `%notin%` <- Negate(`%in%`)

  pars <- c(
    case_when(cor_ed %in% 1:3 ~ c('var_ed',
                                  'alpha_ed',
                                  'var_nug'),
              cor_ed %notin% 1:3 ~ ""),

    case_when(cor_car %in% 1:3 ~ c('tau', 'rho'),
              cor_car %notin% 1:3 ~ ""),

    'beta',
    'abil',
    'difficulty',
    'diff_species',
    'alpha',
    'eta',
    if(loglik == T) 'log_lik',
    'sigma_abil',
    'sigma_alpha'
  )
  pars <- pars[pars != '']


  if(covariates == 'use_cov'){
  # data part
    options(na.action='na.pass') # to preserve the NAs
    out_list <- mylm(formula = formula, data = data) # produces the design matrix

    response <- out_list$y # response variable
    X <- out_list$X # design matrix
  }

  if(covariates != 'use_cov'){
    # data part
    response <- NULL
    X <- matrix(1, nrow(data), 1)
  }


  M <- length(unique(data$id)) # locations

  data$True_Species_num <- as.numeric(as.factor(data$True_Species))


  # spatial elements
  # Euclidean distance
  e <- data %>%
    dplyr::select(lon, lat) %>% distinct() %>%
    dist(., method = "euclidean", diag = FALSE, upper = FALSE) %>% as.matrix()

  # Car prior
  # adjacency matrix
  nb2 <- voronoi(data[,c('lon','lat')])
  #https://gis.stackexchange.com/questions/237810/adjacency-matrix-not-including-vertices
  polyids = seq_along(nb2)
  adjMat = matrix(FALSE, ncol = length(nb2), nrow = length(nb2))
  for (ii in polyids) {
    for (jj in setdiff(polyids, seq_len(ii))) {
      adjMat[ii, jj] =
        ifelse(class(gIntersection(nb2[ii, ], nb2[jj, ])) == 'SpatialLines',1,0)
    }
  }
  #use symmetry for the other half
  adjMat[lower.tri(adjMat)] = t(adjMat)[lower.tri(adjMat)]

  W <- adjMat

  num <- apply(W,1,sum)
  adj <- c(unlist(apply(W,1,function(x) which(x == 1) ) ))
  munged_data <- mungeCARdata4stan(adjBUGS = adj,numBUGS = num)
  node1 = munged_data$node1
  node2 = munged_data$node2
  N_edges = munged_data$N_edges



  #if(!is.null(slope) ) slope <- data[,slope]
  #if(!is.null(slope) ) guessing <- data[,guessing]
  Nspecies <- length(unique(data$True_Species_num))


  data_list <- list(N = nrow(data), # total numb of anotations
               M = M, # numb sites
               Nindiv = length(unique(data$user)), # numb users
               K = ncol(X), # number of covariates + intercept
               id = id, # image id
               annot = annot, # users id
               X = X, # design matrix of covariates
               y = data$correct,
               Nspecies = Nspecies,
               species_id = data$True_Species_num,

               #car part
               W = W,
               #N_edges= N_edges,
               #node1 = node1,
               #node2 = node2,
               # Euc distance part
               e = e,
               I = diag(1, length(unique(data$SiteID)), length(unique(data$SiteID))),
               alpha_max = 4 * max(e)
  )


  #RE1 = RE1mm # random effect matrix

  ini <- function(){list(alpha = rep(1, M),
                         eta = rep(0.1, Nspecies)
  )}



  fit <- rstan::stan(model_code = ir_model,
                     model_name = "ir_model",
                     data = data_list,
                     pars = pars,
                     iter = iter,
                     warmup = warmup,
                     init = ini,
                     chains = chains,
                     verbose = F,
                     #seed = seed,
                     refresh = refresh
  )

  fit
}






#fit <- ir_spat(formula = site ~ -1 + Standing + Resting + Moving + Eating + Interacting + Babies,
#        data = s,
#        spat_model = 'exp',
#        itemtype = '3PLUS',
#        abil = 'user',
#        diff = c('id'),
#        slope = NULL,
#        guessing = NULL,
#        iter = 50,
#        warmup = 25,
#        chains = 3,
#        refresh = 1
#)


#library(loo)
#https://cran.r-project.org/web/packages/loo/vignettes/loo2-with-rstan.html
#log_lik <- extract_log_lik(fit, parameter_name = "log_lik", merge_chains = F)
#waic <- waic(log_lik)


#r_eff <- relative_eff(exp(log_lik))
#loo <- loo(log_lik, r_eff = r_eff, cores = 2)




