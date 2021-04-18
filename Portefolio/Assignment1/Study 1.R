#Supplemental Materials
#Measuring the Evolutionarily Important Goals of Situations: Situational Affordances for Adaptive Problems
#Brown, N.A., Neel, R., & Sherman, R. A.
#Florida Atlantic University
#nbrown60@fau.edu

##Study 1##

##EDIT ik heb er ingezet dat het automatische de packages installed
install.packages('readxl')
install.packages('multicon')
install.packages('lavaan')
install.packages('GPArotation')
install.packages('readxl')

##EDIT ik heb readxl er in gezet
library(readxl)


   #Load the multicon package
library(multicon)

   #Load the lavaan package
library(lavaan)

   #Load the GPArotation package
library(GPArotation)

   #Load the dataset: Select Study 1 data.csv EDIT: ik heb de code hieronder vervangen door mijn eigen zodat hun document er gelijk in komt te staan. EDIT: ze gebruiken saapv met kleine en grote letters. ik verander het een beetje hier en alles groot gemaakt
## saap <- read.csv(file.choose())

SAAP <- read_excel("Lesson1/Study 1 data.xlsx")
           
           
  #Subset the dataset for valid cases , EDIT: ik heb dit ook grote letters gemaakt
##saapV <- saap[saap$Valid==1,]

SAAPV <- SAAP[SAAP$Valid==1,]

   #Check the dimension of the dataset EDIT: ik heb dit ook grote letters gemaakt
## dim(saapV)
dim(SAAPV)

  #Subset the SAAP variables  
SAAPset <- SAAPV[,grep("SAAP", names(SAAPV))] 


  #Subset the data by each SAAP subscale

AF <-SAAPset[,1:7] #Affiliation 
ST <-SAAPset[,23:34] #Status
SP <-SAAPset[,35:42] #Self-Protection
MS <-SAAPset[,43:53] #Mate Selection 
MR <-SAAPset[,54:60] #Mate Retention
DA <-SAAPset[,65:76] #Disease Avoidance
KC <-SAAPset[,77:85] #Kin Care

   #One-factor Principal Components Analysis of each subscale
pc.AF=principal(AF, nfactors=1)
pc.ST=principal(ST, nfactors=1)
pc.SP=principal(SP, nfactors=1)
pc.MS=principal(MS, nfactors=1)
pc.MR=principal(MR, nfactors=1)
pc.DA=principal(DA, nfactors=1)
pc.KC=principal(KC, nfactors=1)

   #View the results of the PCA
print(pc.SP, sort=T)
print(pc.DA, sort=T)
print(pc.AF, sort=T)
print(pc.ST, sort=T)
print(pc.MS, sort=T)
print(pc.MR, sort=T)
print(pc.KC, sort=T)

   #Create 4-Item subsets (One Prinicipal Component method)
SP.subset.onepc=SP[,c(7,3,5,2)]
DA.subset.onepc=DA[,c(1,2,11,8)]
AF.subset.onepc=AF[,c(5,3,2,6)]
ST.subset.onepc=ST[,c(1,6,7,5)]
MS.subset.onepc=MS[,c(3,4,1,9)]
MR.subset.onepc=MR[,c(6,3,1,5)]
KC.subset.onepc=KC[,c(9,2,3,7)]

   #Create a dataframe with all 28-Items  (One Principal Component method)

SAAPmeasure.onepc=data.frame(SP.subset.onepc, DA.subset.onepc, AF.subset.onepc, ST.subset.onepc, MS.subset.onepc, MR.subset.onepc, KC.subset.onepc) 

   #Score up the SAAP (One Principal Component method)

SAAP.onepc.list <-
 list(SelfProtection=c(1,2,3,4), DiseaseAvoidance=c(5,6,7,8), Affiliation=c(9,10,11,12), Status=c(13,14,15,16),MateSelection=c(17,18,19,20), MateRetetion=c(21,22,23,24), KinCare=c(25,26,27,28))
SAAP.onepc.keys=make.keys(28,SAAP.onepc.list,item.labels=colnames(SAAPmeasure.onepc)[1:28])
SAAP.onepc.scores=data.frame(scoreItems(SAAP.onepc.keys, SAAPmeasure.onepc[,1:28], impute="none",totals=FALSE)$scores)
SAAP.onepc.alphas=data.frame(scoreItems(SAAP.onepc.keys, SAAPmeasure.onepc[,1:28], impute="none", totals=FALSE)$alpha)
round(cor(SAAP.onepc.scores),2)

   #Calulate the correlation between the 4-itemscale with the full scales (One Principal Component method)

SP.cor.onepc=cor(SAAP.onepc.scores[,1], composite(SP))
DA.cor.onepc=cor(SAAP.onepc.scores[,2], composite(DA))
AF.cor.onepc=cor(SAAP.onepc.scores[,3], composite(AF))
ST.cor.onepc=cor(SAAP.onepc.scores[,4], composite(ST))
MS.cor.onepc=cor(SAAP.onepc.scores[,5], composite(MS))
MR.cor.onepc=cor(SAAP.onepc.scores[,6], composite(MR))
KC.cor.onepc=cor(SAAP.onepc.scores[,7], composite(KC))

corrwithfullscale.onepc=data.frame(SP.cor.onepc, DA.cor.onepc, AF.cor.onepc, ST.cor.onepc, MS.cor.onepc, MR.cor.onepc, KC.cor.onepc)

names(corrwithfullscale.onepc)<- c("Self-Protection", "Disease Avoidance", "Affiliation", "Status", "Mate Selection", "Mate Retention", "Kin Care")
   
   #Round the correlations to two decimals
round(corrwithfullscale.onepc,2)

   #Calculate the intercorrelates of the factors (One Principal Component method)
onepc.intercorrelates=round((cor(SAAP.onepc.scores)),2)

   #Four Principal Components Analysis of each motive
SP.4pc=principal(SP, nfactors=4)
DA.4pc=principal(DA, nfactors=4)
AF.4pc=principal(AF, nfactors=4)
ST.4pc=principal(ST, nfactors=4)
MS.4pc=principal(MS, nfactors=4)
MR.4pc=principal(MR, nfactors=4)
KC.4pc=principal(KC, nfactors=4)

   #View results of the PCA (Four Principal Components method)
print(SP.4pc, sort=T)
print(DA.4pc, sort=T)
print(AF.4pc, sort=T)
print(ST.4pc, sort=T)
print(MS.4pc, sort=T)
print(MR.4pc, sort=T)
print(KC.4pc, sort=T)

   #Create 4-Item subsets (Four Prinicipal Components method)
SP.subset.fourpc=SP[,c(1,2,8,3)]
DA.subset.fourpc=DA[,c(8,7,2,1)]
AF.subset.fourpc=AF[,c(3,5,2,7)]
ST.subset.fourpc=ST[,c(10,11,9,5)]
MS.subset.fourpc=MS[,c(2,8,4,3)]
MR.subset.fourpc=MR[,c(1,6,3,4)]
KC.subset.fourpc=KC[,c(7,9,6,5)]

   #Create a dataframe with all 28-Items  (Four Principal Components method)
SAAPmeasure.fourpc= data.frame(SP.subset.fourpc, DA.subset.fourpc, AF.subset.fourpc, ST.subset.fourpc, MS.subset.fourpc, MR.subset.fourpc, KC.subset.fourpc)

  #Score up the SAAP (Four Principal Components method)

SAAP.fourpc.list <-
 list(SelfProtection=c(1,2,3,4), DiseaseAvoidance=c(5,6,7,8), Affiliation=c(9,10,11,12), Status=c(13,14,15,16),MateSelection=c(17,18,19,20), MateRetention=c(21,22,23,24), KinCare=c(25,26,27,28))
SAAP.fourpc.keys=make.keys(28,SAAP.fourpc.list,item.labels=colnames(SAAPmeasure.fourpc)[1:28])
SAAP.fourpc.scores=data.frame(scoreItems(SAAP.fourpc.keys,SAAPmeasure.fourpc[,1:28], impute="none",totals=FALSE)$scores)
SAAP.fourpc.alphas=data.frame(scoreItems(SAAP.fourpc.keys,SAAPmeasure.fourpc[,1:28], impute="none",totals=FALSE)$alpha)

   #Calulate the correlation between the 4-itemscale with the full scales (Four Principal Components method)

SP.cor.fourpc=cor(SAAP.fourpc.scores[,1], composite(SP))
DA.cor.fourpc=cor(SAAP.fourpc.scores[,2], composite(DA))
AF.cor.fourpc=cor(SAAP.fourpc.scores[,3], composite(AF))
ST.cor.fourpc=cor(SAAP.fourpc.scores[,4], composite(ST))
MS.cor.fourpc=cor(SAAP.fourpc.scores[,5], composite(MS))
MR.cor.fourpc=cor(SAAP.fourpc.scores[,6], composite(MR))
KC.cor.fourpc=cor(SAAP.fourpc.scores[,7], composite(KC))

corrwithfullscale.fourpc=c(SP.cor.fourpc, DA.cor.fourpc, AF.cor.fourpc, ST.cor.fourpc, MS.cor.fourpc, MR.cor.fourpc, KC.cor.fourpc)

names(corrwithfullscale.fourpc)<- c("Self-Protection", "Disease Avoidance", "Affiliation", "Status", "Mate Selection", "Mate Retention", "Kin Care")

   #Round the correlations to two decimals
round(corrwithfullscale.fourpc,2)

   #Calculate the Intercorrelates of factors (Four Principal Components method)
fourpc.intercorrelates=round((cor(SAAP.fourpc.scores)),2)

fourpc.intercorrelates

   #Part-whole correlation method
SP.pw=partwhole(SP, 4)
DA.pw=partwhole(DA, 4)
AF.pw=partwhole(AF,4)
ST.pw=partwhole(ST,4)
MS.pw=partwhole(MS, 4)
MR.pw=partwhole(MR, 4)
KC.pw=partwhole(KC, 4)

   #Determine which 4-item combination has the highest correlation with total scale
which.max(partwhole(SP,4)[1,]) 
which.max(partwhole(DA,4)[1,]) 
which.max(partwhole(AF,4)[1,]) 
which.max(partwhole(ST,4)[1,]) 
which.max(partwhole(MS,4)[1,])
which.max(partwhole(MR,4)[1,])
which.max(partwhole(KC,4)[1,])

   #Calculate the correlation for those combinations
SP.pw[which.max(SP.pw)]
DA.pw[which.max(DA.pw)]  
AF.pw[which.max(AF.pw)]
ST.pw[which.max(ST.pw)]
MS.pw[which.max(MS.pw)]
MR.pw[which.max(MR.pw)]
KC.pw[which.max(KC.pw)] 

   #Create 4-Item subsets (Part-Whole method)
   
SP.pw.subset=SP[,c(3,4,6,7)]
DA.pw.subset=DA[,c(1,2,4,11)]
AF.pw.subset=AF[,c(3,4,5,7)]
ST.pw.subset=ST[,c(1,5,7,11)]
MS.pw.subset=MS[,c(2,3,6,10)]
MR.pw.subset=MR[,c(3,4,6,7)]
KC.pw.subset=KC[,c(2,4,6,9)]

   #Create a dataframe with all 28-Items  (Part Whole method)
SAAPmeasure.pw=cbind(SP.pw.subset, DA.pw.subset, AF.pw.subset, ST.pw.subset, MS.pw.subset, MR.pw.subset, KC.pw.subset)

   #Score up the SAAP (Part-Whole method)

SAAP.pw.list <-
 list(SelfProtection=c(1,2,3,4), DiseaseAvoidance=c(5,6,7,8), Affiliation=c(9,10,11,12), Status=c(13,14,15,16),MateSelection=c(17,18,19,20), MateRetention=c(21,22,23,24), KinCare=c(25,26,27,28))
SAAP.pw.keys=make.keys(28,SAAP.pw.list,item.labels=colnames(SAAPmeasure.pw)[1:28])
SAAP.pw.scores=data.frame(scoreItems(SAAP.pw.keys,SAAPmeasure.pw[,1:28], impute="none",totals=FALSE)$scores)
SAAP.pw.alphas=data.frame(scoreItems(SAAP.pw.keys,SAAPmeasure.pw[,1:28], impute="none",totals=FALSE)$alpha)
SAAP.pw.scores

   #Calculate the Intercorrelates of factors (Part-Whole method)
pw.intercorrelates=round((cor(SAAP.pw.scores)),2)

pw.intercorrelates

#Exploratory Factor Analyses
  #This will produce the 5, 6, and 7 factor solutions

SAAPmeasure28items <- SAAPmeasure.onepc
colnames(SAAPmeasure28items) <- paste("SAAP", 1:28, sep="_")

  #Choose SAAPcontent.csv  EDIT: ik snap niet precies welk bestand ze willen dat ik er nu in zet dus ik heb het eerste data bestand hier neergezet.
##SAAP.content <- read.csv(file.choose())
SAAP.content <- SAAP

pc5 <- principal(SAAPmeasure28items, nfactors=5)
pc5Loadings=data.frame(pc5$loadings[],as.character(SAAP.content[,2]))      
pc5Loadings[order(pc5Loadings[,1],decreasing=T),]           

pc6 <- principal(SAAPmeasure28items, nfactors=6)
pc6Loadings=data.frame(pc6$loadings[],as.character(SAAP.content[,2]))      
pc6Loadings[order(pc6Loadings[,1],decreasing=T),]    

pc7 <- principal(SAAPmeasure28items, nfactors=7)
pc7Loadings=data.frame(pc7$loadings[],as.character(SAAP.content[,2]))      
pc7Loadings[order(pc7Loadings[,1],decreasing=T),]    

#Confirmatory Factor Analysis

study1.struct <- '
			Self Protection =~ SAAP036 + SAAP037 + SAAP039 + SAAP041
			Disease Avoidance =~ SAAP065 + SAAP066 + SAAP072 + SAAP075
			Affiliation =~ SAAP002 + SAAP003 + SAAP005 + SAAP006
			Status =~ SAAP023 + SAAP027 + SAAP028 + SAAP029
			Mate Selection =~ SAAP043 + SAAP045 + SAAP046 + SAAP051
			Mate Retention =~ SAAP054 + SAAP056 + SAAP058 + SAAP059
			Kin Care =~ SAAP078 + SAAP079 + SAAP083 + SAAP085
			'
study1.fit <-sem(study1.struct,SAAPV, missing="fiml")
summary(study1.fit, fit.measures=T, standardized=T)

#Descriptives for final SAAP measure
describe(SAAP.onepc.scores)

## extra ) aan toegevoegd
#Descriptive statistics for the sample
  #Age
describe(c(SAAPV$Age))

  #Sex   EDIT: lijkt er op alsof dit de eerste keer is dat ze Sex in het stukje benoemen. ik pas het niet aan maar het geeft een error 
table(SAAPV$Sex)

  #Ethnicity
table(SAAPV$Ethn)
