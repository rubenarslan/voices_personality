#load packages

library(MASS)
library(paran)
library(caret)
library(tidyverse)
library(AICcmodavg)
library(sn)
library(car)
library(dplyr)

###Lisas Skript für Variablen die miteinander korrelieren

multirnorm <- function(n, vars = 3, cors = 0, mu = 0, sd = 1,
                       varnames = NULL, empirical = FALSE) {
  if (length(mu) == 1) {
    mu <- rep(mu, vars)
  } else if (length(mu) != vars) {
    stop("the length of mu must be 1 or vars");
  }

  if (length(sd) == 1) {
    sd <- rep(sd, vars)
  } else if (length(sd) != vars) {
    stop("the length of sd must be 1 or vars");
  }

  # correlation matrix
  if (class(cors) == "numeric" & length(cors) == 1) {
    if (cors >=-1 & cors <=1) {
      cors = rep(cors, vars*(vars-1)/2)
    } else {
      stop("cors must be between -1 and 1")
    }
  }

  if (class(cors) == "matrix") {
    if (mean(dim(cors) == c(vars,vars)) == 1) {
      cor_mat <- cors
    } else {
      stop("matrix badly specified")
    }
  } else if (length(cors) == vars*vars) {
    cor_mat <- matrix(cors, vars)
  } else if (length(cors) == vars*(vars-1)/2) {
    cor_mat <- matrix(nrow=vars, ncol = vars)
    upcounter = 1
    lowcounter = 1
    for (col in 1:vars) {
      for (row in 1:vars) {
        if (row == col) {
          # diagoal
          cor_mat[row, col] = 1
        } else if (row < col) {
          # upper right triangle
          cor_mat[row, col] = cors[upcounter]
          upcounter <- upcounter + 1
        } else {
          # lower left triangle
          cor_mat[row, col] = cors[lowcounter]
          lowcounter <- lowcounter + 1
        }
      }
    }
  }

  sigma <- (sd %*% t(sd)) * cor_mat
  bvn <- MASS::mvrnorm(n, mu, sigma, empirical = empirical)
  df <- data.frame(bvn)

  if (length(varnames) == vars) {
    names(df) <- varnames
  }

  df
}



## Simulate data

voice_n <- 2124

### Voice data (uncorrelated)

simdata <- tibble(
  voice_id = 1:voice_n,
  f0 = rnorm(voice_n), # fundamental frequency
  pf = rnorm(voice_n) # formant position
)


### jitter and shimmer (correlated)

jitshim = c(.7)

jitshim <- multirnorm(voice_n, 2, cors = jitshim, mu = 0, sd = 1,
                    varnames = c("jitter", "shimmer")) %>%
  mutate(voice_id = 1:voice_n)

jitshim %>% select(-voice_id) %>% cor()

#merge voice datasets

simdata <- merge(simdata, jitshim, by ="voice_id")


#simulate personality data (correlated): big 5 and dominance

#Hinweis: Es wurden in den verschiedenen Datensaetzen je nach Lab unterschiedliche Antwortskalen verwendet
#(sowohl untersch. Frageboegen, wie BFI 10, 42, 44, als auch Antwortskalen, 5-stufig, 7- stufig, -2 bis +2, 1-5 etc.)
#Deshalb macht Likertskala nutzen nicht super viel Sinn und die Skalen werden sowieso z-transformiert am Ende


big5 <- multirnorm(voice_n, 6,
                   cors =c(-.3, -.1, -.3, -.3, .1,
                           .3, .2, .2, .5,
                           .2, .2, .1,
                           .3, .1,
                           -.1),
                   mu = 0, sd = 1,
                   varnames = c("neuro", "extra", "openn", "agree", "consc", "dominance")) %>%
  mutate(voice_id = 1:voice_n)

big5 %>% select(-voice_id) %>% cor()


#simulieren von Alter
age_data1 <- runif(400, 16, 30)
age_data2 <- runif(382, 18, 54)
age_data3 <- runif(284, 19, 30)
age_data4 <- runif(187, 18, 27)
age_data5 <- runif(186, 18, 56)
age_data6 <- runif(165, 18, 34)
age_data7 <- runif(157, 18, 30)
age_data8 <- runif(120, 18, 35)
age_data9 <- runif(108, 18, 23)
age_data10 <- runif(88, 19, 31)
age_data11 <- runif(47, 18, 23)

age <- as.data.frame(c(age_data1, age_data2, age_data3,age_data4, age_data5, age_data6,age_data7, age_data8, age_data9,age_data10, age_data11))

names(age) <- c("age")
#Ganzzahlig machen
age$age <- as.integer(age$age)

#Voice ID column hinzufuegen
age$voice_id <- big5$voice_id


#SOI-R Versuche
#auch hier wurden teilweise unterschiedliche Skalen verwendet(mit 5, 7 oder auch 9 Antwortmoeglichkeiten)

#Korrelierte normalverteilte Daten, um danach behavior und attitude in andere Verteilung zu pressen
# deshalb Korrelationen hier erst einmal höher angeben als sie eigentlich sind (.5, .5, .3)


soir <- multirnorm(voice_n, 3,
                   cors =c(.8, .6,
                           .5),
                   mu = 0, sd = 1,
                   varnames = c("behavior", "attitude", "desire")) %>%
  mutate(voice_id = 1:voice_n)

soir %>% select(-voice_id) %>% cor()


#numerisch machen (sonst gehen die Transformationen nicht)
soir$attitude <- as.numeric(soir$attitude)
soir$behavior <- as.integer(soir$behavior)
soir$desire <- as.numeric(soir$desire)


#desire ist laut vorheriger Daten annaehernd normalverteilt
#behavior ist laut vorheriger Daten eher exponential verteilt
#attitude ist eher poisson bis random

#Transformation behavior
soir$behavior <- exp(soir$behavior)

#Verteilung anschauen und Korrelation mit desire testen
hist(soir$behavior)
cor.test(soir$behavior, soir$desire)

#Transformation attitude

lambda <- fitdistr(soir$attitude, "poisson")
lambda

soir$attitude <- rpois(soir$attitude, 3)
hist(soir$attitude)
cor.test(soir$behavior, soir$attitude)
cor.test(soir$desire, soir$attitude)

#jetzt ist die Verteilung aber "falschrum". Deshalb alle Werte mal -1 nehmen, die Korrelationen sind sowieso zerstoert
soir$attitude <- soir$attitude*-1
hist(soir$attitude)
cor.test(soir$behavior, soir$attitude)
cor.test(soir$desire, soir$attitude)

#alternativ, weil es vielleicht sogar besser zu den vorherigen Daten passt (und die Korrelation eh nicht hinhaut), eine random Verteilung machen
soir$attitude2 <- runif(2124, 1, 7)
hist(soir$attitude2)

##Skalen Full Score fuer SOI, einmal mit attitude und einmal mit attitude 2

soir$soir_full <- rowMeans(soir[,1:3])
soir_full2_list <- c('behavior', 'attitude2', 'desire')
soir$soir_full2 <- rowMeans(soir[soir_full2_list])


#mergen der Datensaetze

simdata <- merge(simdata, big5, by ="voice_id")
simdata <- merge(simdata, soir, by ="voice_id")
simdata <- merge(simdata, age, by ="voice_id")


#z-Transformieren

#dafuer nochmal alles numerische ändern

for(i in c(2:18)){
  simdata[,i] <- as.numeric(as.character(simdata[,i]))
}

#names(simdata)
simdata2 <-simdata
simdata2 <- simdata2 %>% mutate_at(vars(f0:age),
                       funs(as.numeric(as.character(.))))

#Neue Spalte mit Datensatznummer
simdata2$dataset <- simdata2$voice_id

simdata2$dataset<-recode(simdata2$dataset,"1:400=1; 401:782=2; 783:1066=3; 1067:1253=4; 1254:1439=5; 1440:1605=6; 1606:1761=7; 1762:1881=8; 1882:1989=9; 1990:2077=10; 2078:2124 = 11")


#Neue Spalte mit Geschlechtern
simdata2$sex <- simdata2$voice_id
simdata2$sex<-recode(simdata2$sex,"1:400=-1; 401:590=1; 591:782=-1; 783:923=1; 924:1066=-1; 1067:1127=1; 1128:1253=-1; 1254:1439=1; 1440:1605=1; 1606:1761=-1; 1762:1881=-1; 1882:1925=1; 1926:1989=-1; 1990:2124=1")





#Raus mit den Missing values: Etwas umstaendlich, geht sicher besser, aber funktioniert

simdata2$neuro <- replace(simdata2$neuro,1:400, NA)
simdata2$neuro <- replace(simdata2$neuro,1067:1439, NA)
simdata2$neuro <- replace(simdata2$neuro,1747:1761, NA)

simdata2$extra <- replace(simdata2$extra,1:400, NA)
simdata2$extra <- replace(simdata2$extra,1067:1439, NA)
simdata2$extra <- replace(simdata2$extra,1747:1761, NA)

simdata2$openn <- replace(simdata2$openn,1:400, NA)
simdata2$openn <- replace(simdata2$openn,1067:1439, NA)
simdata2$openn <- replace(simdata2$openn,1747:1761, NA)

simdata2$agree <- replace(simdata2$agree,1:400, NA)
simdata2$agree <- replace(simdata2$agree,1067:1439, NA)
simdata2$agree <- replace(simdata2$agree,1747:1761, NA)

simdata2$consc <- replace(simdata2$consc,1:400, NA)
simdata2$consc <- replace(simdata2$consc,1067:1439, NA)
simdata2$consc <- replace(simdata2$consc,1747:1761, NA)

simdata2$dominance <- replace(simdata2$dominance,1:400, NA)
simdata2$dominance <- replace(simdata2$dominance,1067:1439, NA)
simdata2$dominance <- replace(simdata2$dominance,1762:2077, NA)


simdata2$attitude <- replace(simdata2$attitude,1882:2077, NA)
simdata2$behavior <- replace(simdata2$behavior,1882:2077, NA)
simdata2$desire <- replace(simdata2$desire,1882:2077, NA)
simdata2$soir_full <- replace(simdata2$soir_full,1882:2077, NA)
simdata2$attitude2 <- replace(simdata2$attitude2,1882:2077, NA)
simdata2$soir_full2 <- replace(simdata2$soir_full2,1882:2077, NA)

#Ordnen der Spalten
simdata2 <- simdata2[,c(1,19,20,18, 2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)]


#Datensatz exportieren
#library(WriteXLS)
WriteXLS(simdata2, "simulated_data_080319.xlsx", SheetNames = "sheet1", perl="C:/Strawberry/perl/bin/perl.exe",row.names = TRUE, col.names = TRUE)










library(tidyverse)
library(knitr)
library(brms)
library(sjPlot)
library(bayestestR)
library(labelled)
library(codebook)
