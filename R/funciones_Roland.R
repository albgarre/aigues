
#' Modelo de E.coli en planta
#'
#' @param Days numero de dias incluido en la simulacion.
#' @param P_Ec_Water prevalencia de Ecoli en el agua (usar 1 si C_Ecoli_Irr_Water ya incluye prevalencia).
#' @param C_Ecoli_Irr_Water Concentración de E.coli en el agua de irrigación (en CFU per ml of water).
#' @param Rain_days Numero de días de lluvia durante la simulación.
#' @param sun_hrs_mode Moda del número de horas de sol por día.
#'
#' @importFrom mc2d rpert rbetagen
#'
Ec_Leafy2 <- function(Days, P_Ec_Water, C_Ecoli_Irr_Water, Rain_Days, sun_hrs_mode){

  output.tmp <- c(NA)
  output <- c(NA)

  # Rain_Days <- rpert(1, 1, 5.6, 14, shape = 4) #Number of rainy days
  P_Sun <- 1 - Rain_Days/Days #Probability for sunny day
  Sun <- rbinom(Days, 1, P_Sun) #1 if sunny day, 0 if rainy day
  Ec0_soil <- rnorm(1, mean=0.5498624, sd = 0.8162920) # E.coli in soil (log10 cfu per g)
  Soil_Decay <- 0.14 #log units per day
  Sun_hrs <- rpert(Days, 5, sun_hrs_mode, 12) #Number of hours of sun per day
  Irr_Splashing <- rpert(Days, 0.02, 0.04, 0.06, shape = 4) #probability of irrigation splashing.
  P_Irr_Splashing <- rbinom(Days, 1, Irr_Splashing) #1 if irrigation splashing, 0 no splashing
  P_Rain_Spashing <- 1 #Assumption always splashing if raining
  Soil_Transfer <- rbetagen(Days, 0.4, 0.8, 0.05, 16.4) #Soil transfer to spinach by irrigation, g so
  P_Bug_Plant <- runif(Days, 0.35, 0.9) # Proportion of bacteria transferred from soil to plant
  # P_Ec_Water <- 0.35 #Assumption of proability of E. coli in irrigation water
  Ecoli_Irr_Water <- rbinom(Days, 1, P_Ec_Water) #1 if E. coli in water, 0 negative water
  # C_Ecoli_Irr_Water <- (10^(rnorm(Days,0.604, 0.357)))/100 #Concentration of E. coli (cfu per ml wate
  Trans_Irr_Water <- runif(Days, 1.8, 21.6) # Amount of water transfer via irrigation to spinach (mL
  C_Irr <- round(C_Ecoli_Irr_Water * Trans_Irr_Water*Ecoli_Irr_Water*Sun) #E. coli increase due to ir
  C_RSp <- round(10^(Ec0_soil)*Soil_Transfer*P_Bug_Plant*P_Rain_Spashing*(1-Sun)) #E. coli increase
  C_IrrSp <- round(10^(Ec0_soil)*Soil_Transfer*P_Bug_Plant*P_Irr_Splashing*Sun) #E. coli increase du
  C_load <- C_Irr + C_RSp + C_IrrSp #Total load of E. coli per day (cfu per g of produce)
  Sun_Decay <- -0.52 * (Sun_hrs/24) #log10 units

  i <- 1
  for (i in 1:Days) { #Loop to estimate final load taking sun inactivation into account
    if (i == 1) {
      output.tmp[i] <- log10(C_load[i]) + Sun_Decay[i]
    } else {
      output.tmp[i] <- log10(10^output.tmp[i-1] + C_load[i]) + Sun_Decay[i]
    }
    output[i] <- 10^output.tmp[i]
  }
  return(output=output)
}

#' Modelo de E.coli en planta considerando riego por goteo
#'
#' @param Days numero de dias incluido en la simulacion.
#' @param P_Ec_Water prevalencia de Ecoli en el agua (usar 1 si C_Ecoli_Irr_Water ya incluye prevalencia).
#' @param C_Ecoli_Irr_Water Concentración de E.coli en el agua de irrigación (en CFU per ml of water).
#' @param Rain_days Numero de días de lluvia durante la simulación.
#' @param sun_hrs_mode Moda del número de horas de sol por día.
#'
#' @importFrom mc2d rpert rbetagen
#'
Ec_Leafy2_drip <- function(Days, P_Ec_Water, C_Ecoli_Irr_Water, Rain_Days, sun_hrs_mode){

  output.tmp <- c(NA)
  output <- c(NA)

  # Rain_Days <- rpert(1, 1, 5.6, 14, shape = 4) #Number of rainy days
  P_Sun <- 1 - Rain_Days/Days #Probability for sunny day
  Sun <- rbinom(Days, 1, P_Sun) #1 if sunny day, 0 if rainy day
  Ec0_soil <- rnorm(1, mean=0.5498624, sd = 0.8162920) # E.coli in soil (log10 cfu per g)
  Soil_Decay <- 0.14 #log units per day
  Sun_hrs <- rpert(Days, 5, sun_hrs_mode, 12) #Number of hours of sun per day
  Irr_Splashing <- rpert(Days, 0.02, 0.04, 0.06, shape = 4) #probability of irrigation splashing.
  P_Irr_Splashing <- rbinom(Days, 1, Irr_Splashing) #1 if irrigation splashing, 0 no splashing
  P_Rain_Spashing <- 1 #Assumption always splashing if raining
  Soil_Transfer <- rbetagen(Days, 0.4, 0.8, 0.05, 16.4) #Soil transfer to spinach by irrigation, g so
  P_Bug_Plant <- runif(Days, 0.35, 0.9) # Proportion of bacteria transferred from soil to plant
  # P_Ec_Water <- 0.35 #Assumption of proability of E. coli in irrigation water
  Ecoli_Irr_Water <- rbinom(Days, 1, P_Ec_Water) #1 if E. coli in water, 0 negative water
  # C_Ecoli_Irr_Water <- (10^(rnorm(Days,0.604, 0.357)))/100 #Concentration of E. coli (cfu per ml wate
  Trans_Irr_Water <- runif(Days, 1.8, 21.6) # Amount of water transfer via irrigation to spinach (mL
  C_Irr <- round(C_Ecoli_Irr_Water * Trans_Irr_Water*Ecoli_Irr_Water*Sun) #E. coli increase due to ir
  C_RSp <- round(10^(Ec0_soil)*Soil_Transfer*P_Bug_Plant*P_Rain_Spashing*(1-Sun)) #E. coli increase
  # C_IrrSp <- round(10^(Ec0_soil)*Soil_Transfer*P_Bug_Plant*P_Irr_Splashing*Sun) #E. coli increase du
  C_IrrSp <- 0
  C_load <- C_Irr + C_RSp + C_IrrSp #Total load of E. coli per day (cfu per g of produce)
  Sun_Decay <- -0.52 * (Sun_hrs/24) #log10 units

  i <- 1
  for (i in 1:Days) { #Loop to estimate final load taking sun inactivation into account
    if (i == 1) {
      output.tmp[i] <- log10(C_load[i]) + Sun_Decay[i]
    } else {
      output.tmp[i] <- log10(10^output.tmp[i-1] + C_load[i]) + Sun_Decay[i]
    }
    output[i] <- 10^output.tmp[i]
  }
  return(output=output)
}

#' Modelo de E.coli en planta omitiendo la contaminación por splashing
#'
#' @param Days numero de dias incluido en la simulacion.
#' @param P_Ec_Water prevalencia de Ecoli en el agua (usar 1 si C_Ecoli_Irr_Water ya incluye prevalencia).
#' @param C_Ecoli_Irr_Water Concentración de E.coli en el agua de irrigación (en CFU per ml of water).
#' @param Rain_days Numero de días de lluvia durante la simulación.
#' @param sun_hrs_mode Moda del número de horas de sol por día.
#'
#' @importFrom mc2d rpert rbetagen
#'
Ec_Leafy2_noSplashing <- function(Days, P_Ec_Water, C_Ecoli_Irr_Water, Rain_Days, sun_hrs_mode){

  output.tmp <- c(NA)
  output <- c(NA)

  # Rain_Days <- rpert(1, 1, 5.6, 14, shape = 4) #Number of rainy days
  P_Sun <- 1 - Rain_Days/Days #Probability for sunny day
  Sun <- rbinom(Days, 1, P_Sun) #1 if sunny day, 0 if rainy day
  Ec0_soil <- rnorm(1, mean=0.5498624, sd = 0.8162920) # E.coli in soil (log10 cfu per g)
  Soil_Decay <- 0.14 #log units per day
  Sun_hrs <- rpert(Days, 5, sun_hrs_mode, 12) #Number of hours of sun per day
  Irr_Splashing <- rpert(Days, 0.02, 0.04, 0.06, shape = 4) #probability of irrigation splashing.
  P_Irr_Splashing <- rbinom(Days, 1, Irr_Splashing) #1 if irrigation splashing, 0 no splashing
  P_Rain_Spashing <- 1 #Assumption always splashing if raining
  Soil_Transfer <- rbetagen(Days, 0.4, 0.8, 0.05, 16.4) #Soil transfer to spinach by irrigation, g so
  P_Bug_Plant <- runif(Days, 0.35, 0.9) # Proportion of bacteria transferred from soil to plant
  # P_Ec_Water <- 0.35 #Assumption of proability of E. coli in irrigation water
  Ecoli_Irr_Water <- rbinom(Days, 1, P_Ec_Water) #1 if E. coli in water, 0 negative water
  # C_Ecoli_Irr_Water <- (10^(rnorm(Days,0.604, 0.357)))/100 #Concentration of E. coli (cfu per ml wate
  Trans_Irr_Water <- runif(Days, 1.8, 21.6) # Amount of water transfer via irrigation to spinach (mL
  C_Irr <- round(C_Ecoli_Irr_Water * Trans_Irr_Water*Ecoli_Irr_Water*Sun) #E. coli increase due to ir
  # C_RSp <- round(10^(Ec0_soil)*Soil_Transfer*P_Bug_Plant*P_Rain_Spashing*(1-Sun)) #E. coli increase
  C_RSp <- 0
  # C_IrrSp <- round(10^(Ec0_soil)*Soil_Transfer*P_Bug_Plant*P_Irr_Splashing*Sun) #E. coli increase du
  C_IrrSp <- 0
  C_load <- C_Irr + C_RSp + C_IrrSp #Total load of E. coli per day (cfu per g of produce)
  Sun_Decay <- -0.52 * (Sun_hrs/24) #log10 units

  i <- 1
  for (i in 1:Days) { #Loop to estimate final load taking sun inactivation into account
    if (i == 1) {
      output.tmp[i] <- log10(C_load[i]) + Sun_Decay[i]
    } else {
      output.tmp[i] <- log10(10^output.tmp[i-1] + C_load[i]) + Sun_Decay[i]
    }
    output[i] <- 10^output.tmp[i]
  }
  return(output=output)
}
