library(foreign)

ddf <- read.dta("~/Downloads/ReplicationArchive/sourcedata/ccap_validated.dta")

ddf_tr <- ddf[,-c(56:65,67:76)]

ddf_tr2 <- ddf_tr[complete.cases(ddf_tr), ]

colnames(ddf_tr2) <- c(paste0("bfi_", 1:44), "AgainstUniversalHealthcare","AgainstTaxingTheRich", "ConservativeIdeology", "PoliticalInterest",
                       "Vote" ,"YearOfBirth", "Gender", "Race", "EducationLevel", "AgainstAbortion", "AgainstGayMarriage", "State","IncomeLevel")

col_filter <- c(paste0("bfi_", 34:44), "AgainstUniversalHealthcare","AgainstTaxingTheRich", "ConservativeIdeology", "PoliticalInterest",
                "Vote" , "AgainstAbortion", "AgainstGayMarriage","IncomeLevel")

ddf_tr3 <- ddf_tr2[apply(apply(ddf_tr2[, col_filter], 1, function(x) !(x %in% c("not asked",
                                                                                "prefer not to say", 
                                                                                "skipped",
                                                                                "i'm not sure, i haven't thought much about this", 
                                                                                "i'm not sure, i haven't thought much about this.",
                                                                                "not sure" ))), 2 ,sum) == length(col_filter),]

ddf_tr3$Race[ ddf_tr3$Race %in% c("asian", "native american", "mixed", "other")] <- 'other' 




ddf_tr3 <- ddf_tr3[, c(1:44, 54:55, 45:53,57,56)]

ddf_tr3 <- droplevels(ddf_tr3)

ddf_tr4 <- lapply(ddf_tr3, function(x) as.numeric(x))

ddf_tr4 <- as.data.frame(ddf_tr4)


write.table(ddf_tr3, "bfg.csv", sep="\t", row.names = FALSE)

write.table(ddf_tr4, "bfg_numeric.csv", sep="\t", row.names = FALSE)

ind <- c(2,6,21,31,12,27,37,8,18,23,43,9,24,34,35,41) 
keys <- rep(1,57)
keys[ind] <- -1

new <- psych::reverse.code(keys, ddf_tr4 , mini=1, maxi=5)



Extraversion: 1, , 11, 16, 21R, 26, 31R, 36
Agreeableness: 2R, 7, 12R, 17, 22, 27R, 32, 37R, 42
Conscientiousness: 3, 8R, 13, 18R, 23R, 28, 33, 38, 43R
Neuroticism: 4, 9R, 14, 19, 24R, 29, 34R, 39
Openness: 5, 10, 15, 20, 25, 30, 35R, 40, 41R, 44


bfg_numeric <- read.table( "/home/rick/Downloads/bfg_numeric.csv", sep="\t", header = TRUE)
datafile<-"/home/rick/Downloads/bfg_numeric.sav"
codefile<-"/home/rick/Downloads/bfg_numeric.cod"
write.foreign(bfg_numeric,datafile,codefile,package="SPSS")
file.show(codefile)


fa4_model <- 'fa4_1=~ bfi_1 + bfi_2 + bfi_3 + bfi_4 + bfi_5 + bfi_6 + bfi_7 + bfi_8 + bfi_9 + bfi_10 + bfi_11 
+ bfi_12 + bfi_13 + bfi_14 + bfi_15 + bfi_16 + bfi_17 + bfi_18 + bfi_19 + bfi_20 + bfi_21
+ bfi_22 + bfi_23 + bfi_24 + bfi_25 + bfi_26 + bfi_27 + bfi_28 + bfi_29 + bfi_30 + bfi_31 
+ bfi_32 + bfi_33 + bfi_34 + bfi_35 + bfi_36 + bfi_37 + bfi_38 + bfi_39 + bfi_40 + bfi_41 
+ bfi_42 + bfi_43 + bfi_44 
fa4_2=~ bfi_1 + 0*bfi_2 + bfi_3 + bfi_4 + bfi_5 + bfi_6 + bfi_7 + bfi_8 + bfi_9 + bfi_10 + bfi_11 
+ bfi_12 + bfi_13 + bfi_14 + bfi_15 + bfi_16 + bfi_17 + bfi_18 + bfi_19 + bfi_20 + bfi_21
+ bfi_22 + bfi_23 + bfi_24 + bfi_25 + bfi_26 + bfi_27 + bfi_28 + bfi_29 + bfi_30 + bfi_31 
+ bfi_32 + bfi_33 + bfi_34 + bfi_35 + bfi_36 + bfi_37 + bfi_38 + bfi_39 + bfi_40 + bfi_41 
+ bfi_42 + bfi_43 + bfi_44 
fa4_3=~ bfi_1 + bfi_2 + bfi_3 + bfi_4 + bfi_5 + 0*bfi_6 + bfi_7 + bfi_8 + bfi_9 + 0*bfi_10 + bfi_11 
+ bfi_12 + bfi_13 + bfi_14 + bfi_15 + bfi_16 + bfi_17 + bfi_18 + bfi_19 + bfi_20 + bfi_21
+ bfi_22 + bfi_23 + bfi_24 + bfi_25 + bfi_26 + bfi_27 + bfi_28 + bfi_29 + bfi_30 + bfi_31 
+ bfi_32 + bfi_33 + bfi_34 + bfi_35 + bfi_36 + bfi_37 + bfi_38 + bfi_39 + bfi_40 + bfi_41 
+ bfi_42 + bfi_43 + bfi_44 
fa4_4=~ bfi_1 + bfi_2 + bfi_3 + bfi_4 + bfi_5 + bfi_6 + bfi_7 + bfi_8 + bfi_9 + bfi_10 + bfi_11 
+ bfi_12 + 0*bfi_13 + 0*bfi_14 + 0*bfi_15 + bfi_16 + bfi_17 + bfi_18 + bfi_19 + bfi_20 + bfi_21
+ bfi_22 + bfi_23 + bfi_24 + bfi_25 + bfi_26 + bfi_27 + bfi_28 + bfi_29 + bfi_30 + bfi_31 
+ bfi_32 + bfi_33 + bfi_34 + bfi_35 + bfi_36 + bfi_37 + bfi_38 + bfi_39 + bfi_40 + bfi_41 
+ bfi_42 + bfi_43 + bfi_44' 

bfg_numeric_r <- as.data.frame(psych::reverse.code(keys, bfg_numeric , mini=1, maxi=5))
colnames(bfg_numeric_r) <- colnames(bfg_numeric)
fa4_model_fit <-cfa(fa4_model, data = bfg_numeric_r, std.lv = TRUE)
lavInspect(fa4_model_fit)
summary (fa4_model_fit)
fitmeasures(fa4_model_fit, "rmsea")
saveRDS(bfg_numeric,"bfg_numeric.rds")

bfg <- readRDS("bfg_numeric.rds")
xx <-ddf_tr6$bfi_1 
as.numeric(xx )
str(xx)
ddf_tr5 <- ddf_tr4[apply(apply(ddf_tr4[, col_filter], 1, function(x) !(x %in% c("not asked", "skipped"))), 2 ,sum) == length(col_filter),]



ddf_tr5 <- droplevels(ddf_tr5)
paste0("PolIdea_", 1:44,54:55, 45:53,56:57)
paste0()
political ideology,
social attitudes
economic attitudes

bcap20 Individualist Health Care 
bcap24 Oppose Taxing the Rich


bcap700 Self-Reported Ideology 
bcap813 Political Interest
pcap600  Vote

profile15 Against Abortion 
profile19 Oppose Civil Unions





