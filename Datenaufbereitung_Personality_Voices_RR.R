#Cleaning personality and voices RR

library(psych)
library(car)
library(ggplot2)
setwd("~/Google Drive/Personality_voices_RR/")

##############################################################################################
#Erster Schritt: Rekodieren von Persoenlichkeitsvariablen wenn noetig
#Nur Datensaetze 1,3,4,5,6,8,10,11 (also nicht 3,7,9, die sind schon richtig)


###Jones Dataset 1
SOI_data_Jones <- read.delim("Rohdaten_und_einzelne_Datensaetze/recode_PSK_bis_Z_390/Daten1/SOI_data_Jones.txt")
names(SOI_data_Jones)

soi_a <- cbind(SOI_data_Jones$attitude1, SOI_data_Jones$attitude2, SOI_data_Jones$attitude3)
soi_a <- as.data.frame(soi_a)

psych::alpha(soi_a)

#weitere SOIR Skalen
soi_b <- cbind(SOI_data_Jones$behavior1, SOI_data_Jones$behavior2, SOI_data_Jones$behavior3)
soi_b <- as.data.frame(soi_b)

psych::alpha(soi_b)

soi_d <- cbind(SOI_data_Jones$desire1, SOI_data_Jones$desire2, SOI_data_Jones$desire3)
soi_d <- as.data.frame(soi_d)

psych::alpha(soi_d)

soi_full <- cbind(soi_a, soi_b, soi_d)
colnames(soi_full) <- c("soi_a1", "soi_a2", "soi_a3", "soi_b1","soi_b2", "soi_b3", "soi_d1","soi_d2", "soi_d3")

psych::alpha(soi_full)

SOI_data_Jones$age <- as.integer(SOI_data_Jones$age)

SOI_data_Jones$attitude <- rowMeans(SOI_data_Jones[,c(6:8)])
SOI_data_Jones$behavior <- rowMeans(SOI_data_Jones[,c(3:5)])
SOI_data_Jones$desire <- rowMeans(SOI_data_Jones[,c(9:11)])
SOI_data_Jones$soi_full <- rowMeans(SOI_data_Jones[,c(3:11)])


data_Jones <- SOI_data_Jones[,c(1,2,12:15)]

#rio::export(data_Jones, "data_soi_jones_full.xlsx")



################################################################################################
#Data 5: Puts

Daten_Puts <- rio::import("Rohdaten_und_einzelne_Datensaetze/recode_PSK_bis_Z_390/Daten5/Daten_Puts.xlsx")
names(Daten_Puts)

Daten_Puts$attitude3 <- car::recode(Daten_Puts$attitude3, "1=9;2=8;3=7;4=6;5=5; 6=4;
                                    7=3; 8=2; 9=1")


soi_a <- cbind(Daten_Puts$attitude1,Daten_Puts$attitude2, Daten_Puts$attitude3)
soi_a <- as.data.frame(soi_a)

psych::alpha(soi_a)

#weitere SOIR Skalen
soi_b <- cbind(Daten_Puts$behavior1, Daten_Puts$behavior2, Daten_Puts$behavior3)
soi_b <- as.data.frame(soi_b)

psych::alpha(soi_b)

soi_d <- cbind(Daten_Puts$desire1, Daten_Puts$desire2, Daten_Puts$desire3)
soi_d <- as.data.frame(soi_d)

psych::alpha(soi_d)

soi_full <- cbind(soi_a, soi_b, soi_d)
colnames(soi_full) <- c("soi_a1", "soi_a2", "soi_a3", "soi_b1","soi_b2", "soi_b3", "soi_d1","soi_d2", "soi_d3")

psych::alpha(soi_full)
names(Daten_Puts)

Daten_Puts$attitude <- rowMeans(Daten_Puts[,c(7:9)])
Daten_Puts$behavior <- rowMeans(Daten_Puts[,c(4:6)])
Daten_Puts$desire <- rowMeans(Daten_Puts[,c(10:12)])
Daten_Puts$soi_full <- rowMeans(Daten_Puts[,c(4:12)])


data_Puts <- Daten_Puts[,c(1,2,3,13:16)]

rio::export(data_Puts, "Rohdaten_und_einzelne_Datensaetze/recode_PSK_bis_Z_390/Daten5/data_soi_Puts_full.xlsx")


################################################################################################
#Data 11: Hill
Daten_Hill <- rio::import("Rohdaten_und_einzelne_Datensaetze/recode_PSK_bis_Z_390/Daten11/daten_hill.xlsx")

names(Daten_Hill)

Daten_Hill$attitude3 <- car::recode(Daten_Hill$attitude3, "1=9;2=8;3=7;4=6;5=5; 6=4;
                                    7=3; 8=2; 9=1")


soi_a <- cbind(Daten_Hill$attitude1,Daten_Hill$attitude2, Daten_Hill$attitude3)
soi_a <- as.data.frame(soi_a)

psych::alpha(soi_a)

#weitere SOIR Skalen
soi_b <- cbind(Daten_Hill$behavior1, Daten_Hill$behavior2, Daten_Hill$behavior3)
soi_b <- as.data.frame(soi_b)

psych::alpha(soi_b)

soi_d <- cbind(Daten_Hill$desire1, Daten_Hill$desire2, Daten_Hill$desire3)
soi_d <- as.data.frame(soi_d)

psych::alpha(soi_d)

soi_full <- cbind(soi_a, soi_b, soi_d)
colnames(soi_full) <- c("soi_a1", "soi_a2", "soi_a3", "soi_b1","soi_b2", "soi_b3", "soi_d1","soi_d2", "soi_d3")

psych::alpha(soi_full)
names(Daten_Hill)

Daten_Hill$attitude <- rowMeans(Daten_Hill[,c(7:9)])
Daten_Hill$behavior <- rowMeans(Daten_Hill[,c(4:6)])
Daten_Hill$desire <- rowMeans(Daten_Hill[,c(10:12)])
Daten_Hill$soi_full <- rowMeans(Daten_Hill[,c(4:12)])


data_Hill <- Daten_Hill[,c(1,2,3,13:16)]

rio::export(data_Hill, "Rohdaten_und_einzelne_Datensaetze/recode_PSK_bis_Z_390/Daten11/data_soi_Hill_full.xlsx")


################################################################################################
#Data 6: Schild et al., 2020
Daten_Schild <- rio::import("Rohdaten_und_einzelne_Datensaetze/recode_PSK_bis_Z_390/Daten6/trust_data_goettingen_raw.xlsx")

names(Daten_Schild)
table(Daten_Schild$soi1)

#Alle SOI Behavior Items muessen rekodiert werden (da Skala 1-9, gehen aber 0-8)
Daten_Schild$soi1 <- car::recode(Daten_Schild$soi1, "0=1; 1=2; 2=3; 3=4; 4=5; 5=6; 6=7; 7=8; 8=9")
Daten_Schild$soi2 <- car::recode(Daten_Schild$soi2, "0=1; 1=2; 2=3; 3=4; 4=5; 5=6; 6=7; 7=8; 8=9")
Daten_Schild$soi3 <- car::recode(Daten_Schild$soi3, "0=1; 1=2; 2=3; 3=4; 4=5; 5=6; 6=7; 7=8; 8=9")


Daten_Schild$soi6 <- car::recode(Daten_Schild$soi6, "1=9;2=8;3=7;4=6;5=5; 6=4;
                                    7=3; 8=2; 9=1")


soi_a <- cbind(Daten_Schild$soi4,Daten_Schild$soi5, Daten_Schild$soi6)
soi_a <- as.data.frame(soi_a)

psych::alpha(soi_a)

#weitere SOIR Skalen
soi_b <- cbind(Daten_Schild$soi1, Daten_Schild$soi2, Daten_Schild$soi3)
soi_b <- as.data.frame(soi_b)

psych::alpha(soi_b)

soi_d <- cbind(Daten_Schild$soi7, Daten_Schild$soi8, Daten_Schild$soi9)
soi_d <- as.data.frame(soi_d)

psych::alpha(soi_d)

soi_full <- cbind(soi_a, soi_b, soi_d)
colnames(soi_full) <- c("soi_a1", "soi_a2", "soi_a3", "soi_b1","soi_b2", "soi_b3", "soi_d1","soi_d2", "soi_d3")

psych::alpha(soi_full)
names(Daten_Schild)

Daten_Schild$attitude <- rowMeans(Daten_Schild[,c(7:9)])
Daten_Schild$behavior <- rowMeans(Daten_Schild[,c(4:6)])
Daten_Schild$desire <- rowMeans(Daten_Schild[,c(10:12)])
Daten_Schild$soi_full <- rowMeans(Daten_Schild[,c(4:12)])


data_Schild <- Daten_Schild[,c(1,2,3,13:16)]

rio::export(data_Schild, "Rohdaten_und_einzelne_Datensaetze/recode_PSK_bis_Z_390/Daten6/data_soi_Schild_full.xlsx")


################################################################################################
#Data 4: Stern et al., in prep

Daten_Stern <- rio::import("Rohdaten_und_einzelne_Datensaetze/recode_PSK_bis_Z_390/Daten4/data_cycle2_voice_personality_raw.xlsx")

names(Daten_Stern)

#Alle SOI Behavior Items muessen rekodiert werden (da offenes Antwortformat)
table(Daten_Stern$soi_r_behavior_3)

Daten_Stern$soi_r_behavior_1 <- car::recode(Daten_Stern$soi_r_behavior_1, "0=1; 1=2; 2=3; 3=3; 4=4; 5=4; 6=4; 7=4; 8=5; 9=5;10=5;20=5")
Daten_Stern$soi_r_behavior_2 <- car::recode(Daten_Stern$soi_r_behavior_2, "0=1; 1=2; 2=3; 3=3; 4=4; 5=4; 6=4; 7=4; 8=5; 9=5;10=5;11=5; 12=5; 15=5;20=5; 30=5;31=5;40=5;90=5")
Daten_Stern$soi_r_behavior_3 <- car::recode(Daten_Stern$soi_r_behavior_3, "0=1; 1=2; 2=3; 3=3; 4=4; 5=4; 6=4; 7=4; 8=5; 9=5;10=5;12=5; 14=5; 15=5; 17=5; 20=5; 23=5;26=5;40=5;85=5")


Daten_Stern$soi_r_attitude_6r <- car::recode(Daten_Stern$soi_r_attitude_6r, "1=5;2=4;3=3;4=2;5=1")


soi_a <- cbind(Daten_Stern$soi_r_attitude_4,Daten_Stern$soi_r_attitude_5, Daten_Stern$soi_r_attitude_6r)
soi_a <- as.data.frame(soi_a)

psych::alpha(soi_a)

#weitere SOIR Skalen
soi_b <- cbind(Daten_Stern$soi_r_behavior_1, Daten_Stern$soi_r_behavior_2, Daten_Stern$soi_r_behavior_3)
soi_b <- as.data.frame(soi_b)

psych::alpha(soi_b)

soi_d <- cbind(Daten_Stern$soi_r_desire_7, Daten_Stern$soi_r_desire_8, Daten_Stern$soi_r_desire_9)
soi_d <- as.data.frame(soi_d)

psych::alpha(soi_d)

soi_full <- cbind(soi_a, soi_b, soi_d)
colnames(soi_full) <- c("soi_a1", "soi_a2", "soi_a3", "soi_b1","soi_b2", "soi_b3", "soi_d1","soi_d2", "soi_d3")

psych::alpha(soi_full)
names(Daten_Stern)

Daten_Stern$attitude <- rowMeans(Daten_Stern[,c(50:52)])
Daten_Stern$behavior <- rowMeans(Daten_Stern[,c(47:49)])
Daten_Stern$desire <- rowMeans(Daten_Stern[,c(53:55)])
Daten_Stern$soi_full <- rowMeans(Daten_Stern[,c(47:55)])

####################BFI
#Items rekodieren

Daten_Stern$bfi_agree_1r <- car::recode(Daten_Stern$bfi_agree_1r, "1=5;2=4;3=3;4=2;5=1")
Daten_Stern$bfi_agree_3r <- car::recode(Daten_Stern$bfi_agree_3r, "1=5;2=4;3=3;4=2;5=1")
Daten_Stern$bfi_agree_6r <- car::recode(Daten_Stern$bfi_agree_6r, "1=5;2=4;3=3;4=2;5=1")
Daten_Stern$bfi_agree_8r <- car::recode(Daten_Stern$bfi_agree_8r, "1=5;2=4;3=3;4=2;5=1")
Daten_Stern$bfi_consc_2r <- car::recode(Daten_Stern$bfi_consc_2r, "1=5;2=4;3=3;4=2;5=1")
Daten_Stern$bfi_consc_4r <- car::recode(Daten_Stern$bfi_consc_4r, "1=5;2=4;3=3;4=2;5=1")
Daten_Stern$bfi_consc_8r <- car::recode(Daten_Stern$bfi_consc_8r, "1=5;2=4;3=3;4=2;5=1")
Daten_Stern$bfi_consc_9r <- car::recode(Daten_Stern$bfi_consc_9r, "1=5;2=4;3=3;4=2;5=1")
Daten_Stern$bfi_extra_2r <- car::recode(Daten_Stern$bfi_extra_2r, "1=5;2=4;3=3;4=2;5=1")
Daten_Stern$bfi_extra_5r <- car::recode(Daten_Stern$bfi_extra_5r, "1=5;2=4;3=3;4=2;5=1")
Daten_Stern$bfi_extra_7r <- car::recode(Daten_Stern$bfi_extra_7r, "1=5;2=4;3=3;4=2;5=1")
Daten_Stern$bfi_neuro_2r <- car::recode(Daten_Stern$bfi_neuro_2r, "1=5;2=4;3=3;4=2;5=1")
Daten_Stern$bfi_neuro_5r <- car::recode(Daten_Stern$bfi_neuro_5r, "1=5;2=4;3=3;4=2;5=1")
Daten_Stern$bfi_neuro_6r <- car::recode(Daten_Stern$bfi_neuro_6r, "1=5;2=4;3=3;4=2;5=1")
Daten_Stern$bfi_open_7r <- car::recode(Daten_Stern$bfi_open_7r, "1=5;2=4;3=3;4=2;5=1")
Daten_Stern$bfi_open_9r <- car::recode(Daten_Stern$bfi_open_9r, "1=5;2=4;3=3;4=2;5=1")



agree <- cbind(Daten_Stern[,c(3:11)])
agree <- as.data.frame(agree)

psych::alpha(agree)

consc <- cbind(Daten_Stern[,c(12:20)])
consc <- as.data.frame(consc)

psych::alpha(consc)

extra <- cbind(Daten_Stern[,c(21:28)])
extra <- as.data.frame(extra)

psych::alpha(extra)

neuro <- cbind(Daten_Stern[,c(29:36)])
neuro <- as.data.frame(neuro)

psych::alpha(neuro)

open <- cbind(Daten_Stern[,c(37:46)])
open <- as.data.frame(open)

psych::alpha(open)

Daten_Stern$agree <- rowMeans(Daten_Stern[,c(3:11)])
Daten_Stern$consc <- rowMeans(Daten_Stern[,c(12:20)])
Daten_Stern$extra <- rowMeans(Daten_Stern[,c(21:28)])
Daten_Stern$neuro <- rowMeans(Daten_Stern[,c(29:36)])
Daten_Stern$openn <- rowMeans(Daten_Stern[,c(37:46)])

names(Daten_Stern)

data_Stern <- Daten_Stern[,c(1,2,56:64)]

rio::export(data_Stern, "Rohdaten_und_einzelne_Datensaetze/recode_PSK_bis_Z_390/Daten4/data_soi_Stern_full.xlsx")



################################################################################################
#Data 3: Penke & Asendorpf, 2008
Daten_Penke <- rio::import("Rohdaten_und_einzelne_Datensaetze/recode_PSK_bis_Z_390/Daten3/Daten_Soziosex.xlsx")


names(Daten_Penke)

Daten_Penke$geschl <- car::recode(Daten_Penke$geschl, "1=1; 2=-1")

#SOIR fullscale score
Daten_Penke$soir_full <- rowMeans(Daten_Penke[,c(10:12)])

data_Penke <- Daten_Penke

rio::export(data_Penke, "Rohdaten_und_einzelne_Datensaetze/recode_PSK_bis_Z_390/Daten3/data_soi_Penke_full.xlsx")



##################################################################################
#Daten Arslan (Daten10)
Daten_Arslan <- rio::import("Rohdaten_und_einzelne_Datensaetze/recode_PSK_bis_Z_390/Daten10/BFIsortiert.xlsx")

####################BFI
#Items rekodieren

Daten_Arslan$BFI_A1R <- car::recode(Daten_Arslan$BFI_A1R, "1=5;2=4;3=3;4=2;5=1")
Daten_Arslan$BFI_E2R <- car::recode(Daten_Arslan$BFI_E2R, "1=5;2=4;3=3;4=2;5=1")
Daten_Arslan$BFI_C2R <- car::recode(Daten_Arslan$BFI_C2R, "1=5;2=4;3=3;4=2;5=1")
Daten_Arslan$BFI_N2R <- car::recode(Daten_Arslan$BFI_N2R, "1=5;2=4;3=3;4=2;5=1")
Daten_Arslan$BFI_A3R <- car::recode(Daten_Arslan$BFI_A3R, "1=5;2=4;3=3;4=2;5=1")
Daten_Arslan$BFI_C4R <- car::recode(Daten_Arslan$BFI_C4R, "1=5;2=4;3=3;4=2;5=1")
Daten_Arslan$BFI_E5R <- car::recode(Daten_Arslan$BFI_E5R, "1=5;2=4;3=3;4=2;5=1")
Daten_Arslan$BFI_C5R <- car::recode(Daten_Arslan$BFI_C5R, "1=5;2=4;3=3;4=2;5=1")
Daten_Arslan$BFI_N5R <- car::recode(Daten_Arslan$BFI_N5R, "1=5;2=4;3=3;4=2;5=1")
Daten_Arslan$BFI_A6R <- car::recode(Daten_Arslan$BFI_A6R, "1=5;2=4;3=3;4=2;5=1")
Daten_Arslan$BFI_E7R <- car::recode(Daten_Arslan$BFI_E7R, "1=5;2=4;3=3;4=2;5=1")
Daten_Arslan$BFI_N7R <- car::recode(Daten_Arslan$BFI_N7R, "1=5;2=4;3=3;4=2;5=1")
Daten_Arslan$BFI_O7R <- car::recode(Daten_Arslan$BFI_O7R, "1=5;2=4;3=3;4=2;5=1")
Daten_Arslan$BFI_A8R <- car::recode(Daten_Arslan$BFI_A8R, "1=5;2=4;3=3;4=2;5=1")
Daten_Arslan$BFI_O9R <- car::recode(Daten_Arslan$BFI_O9R, "1=5;2=4;3=3;4=2;5=1")
Daten_Arslan$BFI_C9R <- car::recode(Daten_Arslan$BFI_C9R, "1=5;2=4;3=3;4=2;5=1")



names(Daten_Arslan)


agree <- cbind(Daten_Arslan[,c(5,10,15,20,25,30,35,40,45)])
agree <- as.data.frame(agree)

psych::alpha(agree)

consc <- cbind(Daten_Arslan[,c(6,11,16,21,26,31,36,41,46)])
consc <- as.data.frame(consc)

psych::alpha(consc)

extra <- cbind(Daten_Arslan[,c(4,9,14,19,24,29,34,39)])
extra <- as.data.frame(extra)

psych::alpha(extra)

neuro <- cbind(Daten_Arslan[,c(7,12,17,22,27,32,37,42)])
neuro <- as.data.frame(neuro)

psych::alpha(neuro)

open <- cbind(Daten_Arslan[,c(8,13,18,23,28,33,38,43,44,47)])
open <- as.data.frame(open)

psych::alpha(open)

Daten_Arslan$agree <- rowMeans(Daten_Arslan[,c(5,10,15,20,25,30,35,40,45)])
Daten_Arslan$consc <- rowMeans(Daten_Arslan[,c(6,11,16,21,26,31,36,41,46)])
Daten_Arslan$extra <- rowMeans(Daten_Arslan[,c(4,9,14,19,24,29,34,39)])
Daten_Arslan$neuro <- rowMeans(Daten_Arslan[,c(7,12,17,22,27,32,37,42)])
Daten_Arslan$openn <- rowMeans(Daten_Arslan[,c(8,13,18,23,28,33,38,43,44,47)])


rio::export(Daten_Arslan, "Rohdaten_und_einzelne_Datensaetze/recode_PSK_bis_Z_390/Daten10/data_bfi_Arslan_full.xlsx")


##############################################################################################
#Daten8
Daten_Junger <- rio::import("Rohdaten_und_einzelne_Datensaetze/recode_PSK_bis_Z_390/Daten8/personality_lab_Zyklus1.xlsx")

Daten_Junger <- Daten_Junger[,-c(5:14)]

Daten_Junger$stolz[which(Daten_Junger$stolz==9)] <- NA_real_
Daten_Junger$dominance <- rowMeans(Daten_Junger[,c(14:21)], na.rm = T)
Daten_Junger <- Daten_Junger[,-c(14:21)]

rio::export(Daten_Junger, "Rohdaten_und_einzelne_Datensaetze/recode_PSK_bis_Z_390/Daten8/data_bfi_Junger_full.xlsx")


##############################################################################################
#Zweiter Schritt: Ab hier wird es relevant!

##alles zu einem Datensatz machen
#alle zusammen gefuegten Rohdaten einlesen

daten1 <- rio::import("Rohdaten_und_einzelne_Datensaetze/Daten_PSK_Stimme_zusammen_roh/daten1.xlsx")
daten2 <- rio::import("Rohdaten_und_einzelne_Datensaetze/Daten_PSK_Stimme_zusammen_roh/daten2.xlsx")
daten3 <- rio::import("Rohdaten_und_einzelne_Datensaetze/Daten_PSK_Stimme_zusammen_roh/daten3.xlsx")
daten4 <- rio::import("Rohdaten_und_einzelne_Datensaetze/Daten_PSK_Stimme_zusammen_roh/daten4.xlsx")
daten5 <- rio::import("Rohdaten_und_einzelne_Datensaetze/Daten_PSK_Stimme_zusammen_roh/daten5.xlsx")
daten6 <- rio::import("Rohdaten_und_einzelne_Datensaetze/Daten_PSK_Stimme_zusammen_roh/daten6.xlsx")
daten7 <- rio::import("Rohdaten_und_einzelne_Datensaetze/Daten_PSK_Stimme_zusammen_roh/daten7.xlsx")
daten8 <- rio::import("Rohdaten_und_einzelne_Datensaetze/Daten_PSK_Stimme_zusammen_roh/daten8.xlsx")
daten9 <- rio::import("Rohdaten_und_einzelne_Datensaetze/Daten_PSK_Stimme_zusammen_roh/daten9.xlsx")
daten10 <- rio::import("Rohdaten_und_einzelne_Datensaetze/Daten_PSK_Stimme_zusammen_roh/daten10_new.xlsx")
daten11 <- rio::import("Rohdaten_und_einzelne_Datensaetze/Daten_PSK_Stimme_zusammen_roh/daten11.xlsx")

data_complete_all_raw <- rbind(daten1, daten2, daten3, daten4, daten5, daten6, daten7, daten8, daten9, daten10, daten11)

rio::export(data_complete_all_raw, "data_complete_untrans.xlsx")

############################################################################################
#Persoenlichkeitsdaten alle auf -2 bis +2 bringen (wie von Ruben empfohlen)
#Formel: (x-5)/10*5  fuer 9er Skala. Sonst immer die 9 austauschen durch Länge der Skala und die erste 5 in der Klammer durch den Skalenmittelpunkt

#Daten1: SOIR 1-5
psych::describe(daten1)
names(daten1)

daten1$behavior <- (daten1$behavior -3)/5*5
daten1$attitude <- (daten1$attitude -3)/5*5
daten1$desire <- (daten1$desire -3)/5*5
daten1$soir_full <- (daten1$soir_full -3)/5*5


##################################################################
#Daten2: SOIR 1-5
#dominance auch 1-5?
#BFI auch 1-5
psych::describe(daten2)

daten2$behavior <- (daten2$behavior -3)/5*5
daten2$attitude <- (daten2$attitude -3)/5*5
daten2$desire <- (daten2$desire -3)/5*5
daten2$soir_full <- (daten2$soir_full -3)/5*5

daten2$neuro <- (daten2$neuro -3)/5*5
daten2$extra <- (daten2$extra -3)/5*5
daten2$openn <- (daten2$openn -3)/5*5
daten2$agree <- (daten2$agree -3)/5*5
daten2$consc <- (daten2$consc -3)/5*5
daten2$dominance <- (daten2$dominance -3)/5*5


##################################################################
#Daten3: SOIR 1-5
#dominance -3 bis +3?
#BFI auch 1-5
psych::describe(daten3)

# turn sum scores into means
daten3$neuro <- daten3$neuro/7
daten3$extra <- daten3$extra/8
daten3$openn <- daten3$openn/10
daten3$agree <- daten3$agree/8
daten3$consc <- daten3$consc/9

psych::describe(daten3)

daten3$behavior <- (daten3$behavior -5)/10*5
daten3$attitude <- (daten3$attitude -5)/10*5
daten3$desire <- (daten3$desire -5)/10*5
daten3$soir_full <- (daten3$soir_full -5)/10*5

daten3$neuro <- (daten3$neuro -3)/5*5
daten3$extra <- (daten3$extra -3)/5*5
daten3$openn <- (daten3$openn -3)/5*5
daten3$agree <- (daten3$agree -3)/5*5
daten3$consc <- (daten3$consc -3)/5*5
daten3$dominance <- (daten3$dominance -0)/7*5


##################################################################
#Daten4: SOIR 1-5
#BFI auch 1-5
psych::describe(daten4)

daten4$behavior <- (daten4$behavior -3)/5*5
daten4$attitude <- (daten4$attitude -3)/5*5
daten4$desire <- (daten4$desire -3)/5*5
daten4$soir_full <- (daten4$soir_full -3)/5*5

daten4$neuro <- (daten4$neuro -3)/5*5
daten4$extra <- (daten4$extra -3)/5*5
daten4$openn <- (daten4$openn -3)/5*5
daten4$agree <- (daten4$agree -3)/5*5
daten4$consc <- (daten4$consc -3)/5*5

##################################################################
#Daten5: SOIR 1-5
psych::describe(daten5)
names(daten5)

daten5$behavior <- (daten5$behavior -5)/10*5
daten5$attitude <- (daten5$attitude -5)/10*5
daten5$desire <- (daten5$desire -5)/10*5
daten5$soir_full <- (daten5$soir_full -5)/10*5

##################################################################
#Daten6: SOIR 1-5
psych::describe(daten6)
names(daten6)

daten6$behavior <- (daten6$behavior -5)/10*5
daten6$attitude <- (daten6$attitude -5)/10*5
daten6$desire <- (daten6$desire -5)/10*5
daten6$soir_full <- (daten6$soir_full -5)/10*5

##################################################################
#Daten7: SOIR 1-9
#BFI auch 1-5
#dominance 1-5

psych::describe(daten7)

daten7$behavior <- (daten7$behavior -5)/10*5
daten7$attitude <- (daten7$attitude -5)/10*5
daten7$desire <- (daten7$desire -5)/10*5
daten7$soir_full <- (daten7$soir_full -5)/10*5

daten7$neuro <- (daten7$neuro -3)/5*5
daten7$extra <- (daten7$extra -3)/5*5
daten7$openn <- (daten7$openn -3)/5*5
daten7$agree <- (daten7$agree -3)/5*5
daten7$consc <- (daten7$consc -3)/5*5
daten7$dominance <- (daten7$dominance -3)/5*5

################
##################################################################
#Daten8: SOIR 1-5
#BFI auch 1-5
#dominance 1-5

psych::describe(daten8)

daten8$behavior <- (daten8$behavior -3)/5*5
daten8$attitude <- (daten8$attitude -3)/5*5
daten8$desire <- (daten8$desire -3)/5*5
daten8$soir_full <- (daten8$soir_full -3)/5*5

daten8$neuro <- (daten8$neuro -3)/5*5
daten8$extra <- (daten8$extra -3)/5*5
daten8$openn <- (daten8$openn -3)/5*5
daten8$agree <- (daten8$agree -3)/5*5
daten8$consc <- (daten8$consc -3)/5*5

###############
##################################################################
#Daten9:
#BFI auch 1-7

psych::describe(daten9)


daten9$extra <- daten9$extra +1

daten9$neuro <- (daten9$neuro -4)/7.5*5
daten9$extra <- (daten9$extra -4.5)/8.5*5
daten9$openn <- (daten9$openn -4)/7.5*5
daten9$agree <- (daten9$agree -4)/7.5*5
daten9$consc <- (daten9$consc -4)/7.5*5

##################################################################
#Daten10:
#BFI auch 1-5

psych::describe(daten10)


daten10$neuro <- (daten10$neuro -3)/5*5
daten10$extra <- (daten10$extra -3)/5*5
daten10$openn <- (daten10$openn -3)/5*5
daten10$agree <- (daten10$agree -3)/5*5
daten10$consc <- (daten10$consc -3)/5*5

##################################################################
#Daten11: SOIR 1-5

psych::describe(daten11)
names(daten11)

daten11$behavior <- (daten11$behavior -5)/10*5
daten11$attitude <- (daten11$attitude -5)/10*5
daten11$desire <- (daten11$desire -5)/10*5
daten11$soir_full <- (daten11$soir_full -5)/10*5



##############################################################################################
#erneut alles zusammen fuegen um zu z-transformieren

data_complete_all <- rbind(daten1, daten2, daten3, daten4, daten5, daten6, daten7, daten8, daten9, daten10, daten11)

#Pf bilden
data_complete_all$pf <- (scale(data_complete_all$f1) + scale(data_complete_all$f2) +
  scale(data_complete_all$f3) + scale(data_complete_all$f4))/4

rio::export(data_complete_all, "data_complete_all_anonym_PfZ.xlsx")

###########################################################################################
#f0 und psk noch z-transformieren
data_complete_all_anonym_PfZ <- rio::import("data_complete_all_anonym_PfZ.xlsx")

names(data_complete_all_anonym_PfZ)

data_complete_all_anonym_PfZ$f0 <- scale(data_complete_all_anonym_PfZ$f0)
data_complete_all_anonym_PfZ$neuro <- scale(data_complete_all_anonym_PfZ$neuro)
data_complete_all_anonym_PfZ$extra <- scale(data_complete_all_anonym_PfZ$extra)
data_complete_all_anonym_PfZ$openn <- scale(data_complete_all_anonym_PfZ$openn)
data_complete_all_anonym_PfZ$agree <- scale(data_complete_all_anonym_PfZ$agree)
data_complete_all_anonym_PfZ$consc <- scale(data_complete_all_anonym_PfZ$consc)
data_complete_all_anonym_PfZ$dominance <- scale(data_complete_all_anonym_PfZ$dominance)
data_complete_all_anonym_PfZ$behavior <- scale(data_complete_all_anonym_PfZ$behavior)
data_complete_all_anonym_PfZ$attitude <- scale(data_complete_all_anonym_PfZ$attitude)
data_complete_all_anonym_PfZ$desire <- scale(data_complete_all_anonym_PfZ$desire)
data_complete_all_anonym_PfZ$soir_full <- scale(data_complete_all_anonym_PfZ$soir_full)

psych::describe(data_complete_all_anonym_PfZ)


rio::export(data_complete_all_anonym_PfZ, "data_complete_all_anonym_allZ.xlsx")

#rio::export(data_complete_all_anonym_PfZ, "so_waere_richtig_gewesen.xlsx")

