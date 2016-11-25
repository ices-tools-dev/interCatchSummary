#---
#title: "Analyzing Age structures in IC"
#author: "Youen Vermard"
#date: '`r date()`'
#output: pdf_document
#---

#### ONLY PIECE TO BE CHANGED working directories
wd <- "F:\\D\\Expertise\\WGNSSK\\2016\\IC\\SOLIV\\"
output_wd <- "F:\\D\\Expertise\\WGNSSK\\2016\\IC\\SOLIV\\"

#This documet uses Table 2 from CatchAndSampleDataTables.txt from the InterCatch outputs to describe the raising procedures that were made.



### libraries
library(data.table)
library(gplots)
library(pander);library(captioner)
library(lattice)
#initialise les fonctions pour légender tables et figures

### read the data
test <- scan(paste(wd,"CatchAndSampleDataTables.txt",sep=""),what='character',sep='\t')
table2 <- test[(which(test=="TABLE 2.")+3):length(test)]
tmp<-table2[-c(1:56)]			  
			  
table2_bis<-data.frame(matrix(tmp,ncol=27,byrow=T), stringsAsFactors =F)
colnames(table2_bis) <- table2[1:27]
table2_bis <- data.table(table2_bis)
table2_bis <- table2_bis[,CATON:=as.numeric(as.character(CATON))]
table2_bis <- table2_bis[,CANUM:=as.numeric(as.character(CANUM))]
table2_bis <- table2_bis[,WECA:=as.numeric(as.character(WECA))]
table2_bis <- table2_bis[,AgeOrLength:=as.numeric(as.character(AgeOrLength))]

table2_bis <- table2_bis[,Area:=as.factor(Area)]
table2_bis <- table2_bis[,Fleet:=factor(Fleet)]
table2_bis <- table2_bis[,Season:=factor(Season)]
table2_bis <- table2_bis[,Country:=factor(Country)]


table2_bis <- table2_bis[,id:=paste(Stock,Country,Area,Season,Fleet)]

table2_bis[Area=="IIIaN                                                       ",'Area'] <- "IIIaN"


colnames(table2_bis)[colnames(table2_bis)=='CATONRaisedOrImported'] <- 'RaisedOrImported'


## simple stats on imported data
		SummaryReport <- table2_bis[,list(CATON=sum(CANUM*WECA)/1000000), by=c('CatchCategory','RaisedOrImported')]
		SummaryReport <- SummaryReport[, perc:=round(CATON/sum(CATON)*100), by='CatchCategory']
		SummaryReport <- SummaryReport[order(CatchCategory),]		

		SOP <- table2_bis[,list(SOP=sum(CANUM*WECA)/1000, CATON=sum(unique(CATON))), by=c('CatchCategory')]
		SOP <-SOP[,list(SOP=SOP/CATON), by=c('CatchCategory')]
		
		
		SampledOrEstimated <- table2_bis[,list(CATON=sum(CANUM*WECA/1000000)), by=c('CatchCategory','RaisedOrImported','SampledOrEstimated')]
		SampledOrEstimated <- SampledOrEstimated[, perc:=round(CATON/sum(CATON)*100), by='CatchCategory']
		SampledOrEstimated <- SampledOrEstimated[order(CatchCategory, perc, decreasing=T),]

		SampledOrEstimatedArea <- table2_bis[,list(CATON=sum(CANUM*WECA/1000000)), by=c('CatchCategory','RaisedOrImported','SampledOrEstimated','Area')]
		SampledOrEstimatedArea <- SampledOrEstimatedArea[, perc:=round(CATON/sum(CATON)*100), by=c('CatchCategory','Area')]
		SampledOrEstimatedArea <- SampledOrEstimatedArea[order(Area, CatchCategory,perc, decreasing=T),]


		### landings with associated discards
		
	landingsWithAssociatedDiscards <- table2_bis[,list(CATON=sum(CANUM*WECA/1000000)), by=c('id','RaisedOrImported','CatchCategory')]
	tmp <-  landingsWithAssociatedDiscards[(RaisedOrImported=="Imported_Data" & CatchCategory=="Discards")  , id]
	landingsWithAssociatedDiscards <- landingsWithAssociatedDiscards[id%in%tmp & CatchCategory=="Landings", sum(CATON)]/table2_bis[CatchCategory=="Landings",sum(CANUM*WECA/1000000)]
	

		## summary of the catch per gear
		table2_bis <- table2_bis[, Gear:=substr(table2_bis$Fleet,1,3)]
		summCatchPerGear <- table2_bis[, list(sumCatch=sum(CANUM*WECA/1000000)), by=c('Stock','Gear')]
		summCatchPerGear <- summCatchPerGear[, percCatch:=sumCatch/sum(sumCatch)*100, by=c('Stock')]
		summCatchPerGear <- summCatchPerGear[ order(percCatch,decreasing=T),]



### Age Structure

listVAR <- c("CatchCategory",'RaisedOrImported',"SampledOrEstimated","Country","Area","Season","Fleet")

## By Sex

listVAR <- c("CatchCategory",'RaisedOrImported',"SampledOrEstimated","Country","Area","Season","Fleet","Sex")

#table2_bis <- table2_bis[CANUM>0,]

AgeStrucSex <- table2_bis[,meanAge:=weighted.mean(AgeOrLength,CANUM, na.rm=T), by=c(listVAR)]


plotFunction <- function(AgeStrucSexLan,Estim,Sampled){
	if(unique(AgeStrucSexLan$AgeOrLengthType)=="Lngt") AgeTit <- "Length" else AgeTit <- "Age"
				plotTitle <- paste("Mean", AgeTit, "in the", catchCat, "by", Var, sep=" ")

	Formula <-paste('meanAge~',Var, sep="")

	boxplot(eval(parse(text=Formula)), data=AgeStrucSexLan,
             boxwex = 0.25, at = which(pos%in%unique(AgeStrucSexLan[,which(colnames(Sampled)==Var)])) + 0.3,
             col = "red",
             main = plotTitle,
             xlab = "",
             ylab = paste("Mean ",AgeTit, sep=""),
             xlim = c(0.5, length(unique(AgeStrucSexLan[,which(colnames(Sampled)==Var)]))+.5), ylim = c(min(AgeStrucSexLan$AgeOrLength), max(AgeStrucSexLan$AgeOrLength)), yaxs = "i", xaxt='n')
	boxplot(eval(parse(text=Formula)), data=Estim, add=TRUE,
             boxwex = 0.25, at = which(pos%in%unique(Estim[,which(colnames(Sampled)==Var)])),
             col = "orange")
	boxplot(eval(parse(text=Formula)), data=Sampled, add=TRUE,
             boxwex = 0.25, at = which(pos%in%unique(Sampled[,which(colnames(Sampled)==Var)])) - 0.3,
             col = "yellow", xaxt='n')
  legend(1, max(AgeStrucSexLan$AgeOrLength), c("Imported", "Raised", "All"),
            fill = c("yellow", "orange", "red"))
	}
	
	
plotFunctionMeanWeight <- function(AgeStrucSexLan=AgeStrucSexLan,Estim=Estim,Sampled=Sampled){
	if(unique(AgeStrucSexLan$AgeOrLengthType)=="Lngt") AgeTit <- "Length" else AgeTit <- "Age"
				plotTitle <- paste("Mean", AgeTit, "in the", catchCat, "by", Var, sep=" ")

	
AgeStrucSexLan <- data.frame(AgeStrucSexLan)
AgeStrucSexLan[,which(colnames(AgeStrucSexLan)==Var)] <- factor(AgeStrucSexLan[,which(colnames(AgeStrucSexLan)==Var)])
pos <- levels(AgeStrucSexLan[,which(colnames(AgeStrucSexLan)==Var)])

Estim <- AgeStrucSexLan[which(AgeStrucSexLan$SampledOrEstimated=='Estimated_Distribution'),]
Estim[,which(colnames(Estim)==Var)] <- factor(Estim[,which(colnames(Estim)==Var)])

Sampled <- AgeStrucSexLan[which(AgeStrucSexLan$SampledOrEstimated=='Sampled_Distribution'),]
Sampled[,which(colnames(Sampled)==Var)] <- factor(Sampled[,which(colnames(Sampled)==Var)])
	
	Formula <-paste('WECA~',Var, sep="")

	boxplot(eval(parse(text=Formula)), data=AgeStrucSexLan,
             boxwex = 0.25, at = which(pos%in%unique(AgeStrucSexLan[,which(colnames(Sampled)==Var)])) + 0.3,
             col = "red",
             main = plotTitle,
             xlab = "",
             ylab = paste("Mean ",AgeTit, sep=""),
             xlim = c(0.5, length(unique(AgeStrucSexLan[,which(colnames(Sampled)==Var)]))+.5), ylim = c(min(AgeStrucSexLan$WECA), max(AgeStrucSexLan$WECA)), yaxs = "i", xaxt='n')
	boxplot(eval(parse(text=Formula)), data=Estim, add=TRUE,
             boxwex = 0.25, at = which(pos%in%unique(Estim[,which(colnames(Sampled)==Var)])),
             col = "orange")
	boxplot(eval(parse(text=Formula)), data=Sampled, add=TRUE,
             boxwex = 0.25, at = which(pos%in%unique(Sampled[,which(colnames(Sampled)==Var)])) - 0.3,
             col = "yellow", xaxt='n')
  legend(1, max(AgeStrucSexLan$WECA), c("Imported", "Raised", "All"),
            fill = c("yellow", "orange", "red"))
}


## export SOP table
#write.csv(SOP, file=paste0(output_wd,"SOP.csv"), row.names=F)


#Raised and imported datas

##Raised discards

#In InterCatch, the first step consists in raising the discards volumes for strats with landings and no discards associated. These discards are called in the following table 'Raised_Discards'. The data called 'Imported_Data' are landings or discards volumes imported into InterCatch with or without length/age structure.

#The proportion of Landings with Discards associated (same strata) is **`r paste(round(landingsWithAssociatedDiscards*100), 'percent')`**


write.csv(c("Landings with discards associated in percent",round(landingsWithAssociatedDiscards*100)), file=paste0(output_wd,"landingsWithAssociatedDiscards.csv"), row.names=F)

#The volumes (and associated proportion) of landings and discards imported  (Imported_Data) or raised (Raised_Discards) are described in the following table.


write.csv(SummaryReport, file=paste0(output_wd,"Imported_Raised_discards.csv"), row.names=F)


##Total catch per gear

#The following table gives a summary of the catch (Landings+discards(imported+raised)) per gear (3 first letters of the metier)

write.csv(summCatchPerGear, file=paste0(output_wd,"Catch_Per_Gear.csv"), row.names=F)
##Length/Age distribution

#For the imported landings/discards and the raised discards without age distribution, the length or age distribution is then computed using the defined allocation scheme. *Sampled_distribution* means that the data (ladings or discards) were input with age/length distribution. *Estimated_distribution* means that the inputed/raised valoumes were estimated using the allocation scheme.

#In the following tables, CATON=WECA*CANUM/1000000 (in tonnes)

write.csv(SampledOrEstimated, file=paste0(output_wd,"Sampled_Estimated_L_D.csv"), row.names=F)

write.csv(SampledOrEstimatedArea, file=paste0(output_wd,"Sampled_Estimated_L_D_PerArea.csv"), row.names=F)

##Impact of the raising on the age/length structure

#Once the samples imported or raised are identified, it is possible to check the impact of the allocation scheme on the mean age/length of the final age/length distribution of the stock.
#The following figures compare the mean age (computed as the weighted mean of the age per strata("CatchCategory",'RaisedOrImported',"SampledOrEstimated","Country","Area","Season","Fleet","Sex")) of the estimated stratas compared to the imported ones and the final distribution. Each individual included in the boxplot corresponds to the weighted mean age of a strata.

###Global mean age

AgeStrucSexLan <- AgeStrucSex
png(filename=paste0(output_wd,"MeanAgeCatchCat.png"))
par(las=2)

		boxplot(meanAge~CatchCategory, data=AgeStrucSexLan,
             boxwex = 0.25, at = 1:length(unique(AgeStrucSexLan$CatchCategory)) + 0.3,
             col = "red",
             main = "Mean Age per Catch Category",
             xlab = "",
             ylab = "Mean Age",
             xlim = c(0.5, length(unique(AgeStrucSexLan$CatchCategory))+.5), ylim = c(min(AgeStrucSexLan$AgeOrLength), max(AgeStrucSexLan$AgeOrLength)), yaxs = "i")
		boxplot(meanAge~CatchCategory, data=AgeStrucSexLan[SampledOrEstimated=='Estimated_Distribution',], add=TRUE,
             boxwex = 0.25, at = 1:length(unique(AgeStrucSexLan$CatchCategory)),
             col = "orange")
		boxplot(meanAge~CatchCategory, data=AgeStrucSexLan[SampledOrEstimated=='Sampled_Distribution',], add=TRUE,
             boxwex = 0.25, at = 1:length(unique(AgeStrucSexLan$CatchCategory)) - 0.3,
             col = "yellow")
    legend(1, max(AgeStrucSexLan$AgeOrLength), c("Imported", "Raised", "All"),
            fill = c("yellow", "orange", "red"))
dev.off()



###Mean Age per sex
var <- "Landings"
AgeStrucSexLan <- AgeStrucSex[CatchCategory==var,]
png(filename=paste0(output_wd,"MeanAge",var,"_Sex.png"))
par(las=2)
		boxplot(meanAge~Sex, data=AgeStrucSexLan,
             boxwex = 0.25, at = 1:length(unique(AgeStrucSexLan$Sex)) + 0.3,
             col = "red",
             main = "Mean Age in the Landings",
             xlab = "",
             ylab = "Mean Age",
             xlim = c(0.5, length(unique(AgeStrucSexLan$Sex))+.5), ylim = c(min(AgeStrucSexLan$AgeOrLength), max(AgeStrucSexLan$AgeOrLength)), yaxs = "i")
		boxplot(meanAge~Sex, data=AgeStrucSexLan[SampledOrEstimated=='Estimated_Distribution',], add=TRUE,
             boxwex = 0.25, at = 1:length(unique(AgeStrucSexLan$Sex)),
             col = "orange")
		boxplot(meanAge~Sex, data=AgeStrucSexLan[SampledOrEstimated=='Sampled_Distribution',], add=TRUE,
             boxwex = 0.25, at = 1:length(unique(AgeStrucSexLan$Sex)) - 0.3,
             col = "yellow")
    legend(1, max(AgeStrucSexLan$AgeOrLength), c("Imported", "Raised", "All"),
            fill = c("yellow", "orange", "red"))
dev.off()


###Mean Age per area


catchCat <- "Landings"
Var <- "Area"
png(filename=paste0(output_wd,"MeanAge",catchCat,Var,".png"))


AgeStrucSexLan <- AgeStrucSex[CatchCategory==catchCat,]
AgeStrucSexLan <- data.frame(AgeStrucSexLan)
AgeStrucSexLan[,which(colnames(AgeStrucSexLan)==Var)] <- factor(AgeStrucSexLan[,which(colnames(AgeStrucSexLan)==Var)])
pos <- levels(AgeStrucSexLan[,which(colnames(AgeStrucSexLan)==Var)])

Estim <- AgeStrucSexLan[which(AgeStrucSexLan$SampledOrEstimated=='Estimated_Distribution'),]
Estim[,which(colnames(Estim)==Var)] <- factor(Estim[,which(colnames(Estim)==Var)])

Sampled <- AgeStrucSexLan[which(AgeStrucSexLan$SampledOrEstimated=='Sampled_Distribution'),]
Sampled[,which(colnames(Sampled)==Var)] <- factor(Sampled[,which(colnames(Sampled)==Var)])
par(las=2)
plotFunction(AgeStrucSexLan,Estim,Sampled)
dev.off()

###Mean Age per fleet

catchCat <- "Landings"
Var <- "Fleet"
png(filename=paste0(output_wd,"MeanAge",catchCat,Var,".png"))

AgeStrucSexLan <- AgeStrucSex[CatchCategory==catchCat,]
AgeStrucSexLan <- data.frame(AgeStrucSexLan)
AgeStrucSexLan[,which(colnames(AgeStrucSexLan)==Var)] <- factor(AgeStrucSexLan[,which(colnames(AgeStrucSexLan)==Var)])
pos <- levels(AgeStrucSexLan[,which(colnames(AgeStrucSexLan)==Var)])

Estim <- AgeStrucSexLan[which(AgeStrucSexLan$SampledOrEstimated=='Estimated_Distribution'),]
Estim[,which(colnames(Estim)==Var)] <- factor(Estim[,which(colnames(Estim)==Var)])

Sampled <- AgeStrucSexLan[which(AgeStrucSexLan$SampledOrEstimated=='Sampled_Distribution'),]
Sampled[,which(colnames(Sampled)==Var)] <- factor(Sampled[,which(colnames(Sampled)==Var)])
par(las=2, mar=c(15,4,4,5))
plotFunction(AgeStrucSexLan,Estim,Sampled)
dev.off()
###Resulting age structure

#The following plot shows the percentage of each age/length for the sampled strata, estimated and the final age structure for the landing and discard fractions.


listVAR <- c("CatchCategory","SampledOrEstimated")

AgeStrucSexProp <- AgeStrucSex[,list(sumCANUM=sum(CANUM)), by=c(listVAR,'AgeOrLength')]
AgeStrucSexProp <- AgeStrucSexProp[,propCanum:=sumCANUM/sum(sumCANUM), by=listVAR]

listVAR <- c("CatchCategory")
AgeStrucSexProp2 <- AgeStrucSex[,list(sumCANUM=sum(CANUM)), by=c(listVAR,'AgeOrLength')]
AgeStrucSexProp2 <- AgeStrucSexProp2[,propCanum:=sumCANUM/sum(sumCANUM), by=listVAR]
AgeStrucSexProp2$SampledOrEstimated <- "Final Distribution"

AgeStrucSexProp<- rbind(AgeStrucSexProp,AgeStrucSexProp2)

png(filename=paste0(output_wd,"AgeStruc.png"))
xyplot(propCanum~AgeOrLength|CatchCategory,groups = SampledOrEstimated, data=AgeStrucSexProp,auto.key = list(space = "right", points = TRUE, lines = FALSE))
dev.off()
##Mean weight at age/length

#the catchAndSampleData also provide the weight at age per strata for the Sampled/Estimated stratas.
#One would also want to check the sampled/estimated and resulting weight at length/age. This is produced in the following graph, each boxplot representing the distribution of the weight at age/length for the different stratas.

catchCat <- "Landings"
Var <- "AgeOrLength"

AgeStrucSexLan <- AgeStrucSex[CatchCategory==catchCat,]
AgeStrucSexLan <- data.frame(AgeStrucSexLan)
AgeStrucSexLan[,which(colnames(AgeStrucSexLan)==Var)] <- factor(AgeStrucSexLan[,which(colnames(AgeStrucSexLan)==Var)])
pos <- levels(AgeStrucSexLan[,which(colnames(AgeStrucSexLan)==Var)])

Estim <- AgeStrucSexLan[which(AgeStrucSexLan$SampledOrEstimated=='Estimated_Distribution'),]
Estim[,which(colnames(Estim)==Var)] <- factor(Estim[,which(colnames(Estim)==Var)])

Sampled <- AgeStrucSexLan[which(AgeStrucSexLan$SampledOrEstimated=='Sampled_Distribution'),]
Sampled[,which(colnames(Sampled)==Var)] <- factor(Sampled[,which(colnames(Sampled)==Var)])
Areas <- unique(AgeStrucSexLan$Area)

png(filename=paste0(output_wd,"MeanWeight",catchCat,Var,".png"))
par(las=2, mfrow=c(1, length(Areas)))
for(i in Areas){
	plotFunctionMeanWeight(AgeStrucSexLan=AgeStrucSexLan[AgeStrucSexLan$Area==i,],Estim=Estim[Estim$Area==i,],Sampled=Sampled[Sampled$Area==i,])

	}
dev.off()
#The outliers (more than 3 times the standard deviation) are extracted and can be investigated from the following table.


catchCat <- "Landings"

AgeStrucSexLan <- data.table(AgeStrucSexLan[AgeStrucSexLan$RaisedOrImported=="Imported_Data" & AgeStrucSexLan$CatchCategory==catchCat,])
AgeStrucSexLan <- AgeStrucSexLan[,AverageWtSize:=mean(WECA), by=list(RaisedOrImported,AgeOrLength,Area)]
AgeStrucSexLan <- AgeStrucSexLan[,stdWECA:=sd(WECA), by=list(RaisedOrImported,AgeOrLength,Area)]

write.csv(AgeStrucSexLan[WECA>AverageWtSize+3*stdWECA | WECA<AverageWtSize-3*stdWECA   ,c('Country','Fleet','CatchCategory','WECA','AverageWtSize','AgeOrLength','Area'), with=F], file=paste0(output_wd,"export_outliers.csv"), row.names=F)
