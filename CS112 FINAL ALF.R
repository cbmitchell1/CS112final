library(dplyr)

#Seperating MW/TT classes
mw_classes <- dplyr::filter(AY201617_Section_Descriptions ,grepl("MW",AY201617_Section_Descriptions$`Cerebus Assessmentreport Section Title`))
tt_classes <- dplyr::filter(AY201617_Section_Descriptions ,grepl("TT",AY201617_Section_Descriptions$`Cerebus Assessmentreport Section Title`))

# class_section <- AY201617_Section_Descriptions
# 
# install.packages("cem",repos="http://r.iq.harvard.edu", type="source")
# library(cem)

#Creating new data_frame                           
day_treat <- 1
tt_classes <- cbind(tt_classes,day_treat)
day_treat <- 0 
mw_classes <- cbind(mw_classes,day_treat)
sections_by_day <- rbind(mw_classes,tt_classes)

cleanData <- function(NS) {
  #more friendly column names
  NS <- setNames(NS, c("Course", "Grader", "Section", "Class", "Assignment", "Hashtag", "College"
                       , "1", "2", "3", "4", "5"))
  #replacing NAs with 0s in each HC score column
  NS[,8:12][is.na(NS[,8:12])] <- 0
  #excluding all assignment grades
  NS <- dplyr::filter(NS, is.na(NS$Assignment))
  return(NS)
}

#initializing data
NS <- cleanData(data.frame(AY201617_NS_Assesments))
SS <- cleanData(data.frame(AY201617_SS_Assessments))
CS <- cleanData(data.frame(AY201617_CS_Assessments))
B <- cleanData(data.frame(AY201617_B_Assessments))
AH <- cleanData(data.frame(AY201617_AH_Assessments))


#finding count of all grades for an HC in a section
HCsum <- function(AssessData) {
  AssessData <- aggregate(x = AssessData[8:12], by = list(Section = AssessData$Section, 
                                                          HC = AssessData$Hashtag), FUN = sum)
  return(AssessData)
}

#finding count of all grades in a section
HCsumall <- function(AssessData) {
  AssessData <- aggregate(x = AssessData[8:12], by = list(Section = AssessData$Section), FUN = sum)
  #sum new aggregated columns 8:12 for each row 
  gradeCount <- rowSums(AssessData[,2:6])
  AssessData <- cbind(AssessData, gradeCount)
  return(AssessData)
}

#finding total points awarded for an HC in a section
pointsum <- function(AssessData){
  totalPoints <- matrix(data = NA, nrow = nrow(AssessData), ncol = 1)
  for (i in 1:nrow(AssessData)) {
    rowPoint <- AssessData[i,3]+(2*AssessData[i,4])+(3*AssessData[i,5])+
      (4*AssessData[i,6])+(5*AssessData[i,7])
    totalPoints[i,1] <- rowPoint
  }
  AssessData <- cbind(AssessData, totalPoints)
  return(AssessData)
}

#finding total points awarded for all grades in a section
pointsumall <- function(AssessData){
  totalPoints <- matrix(data = NA, nrow = nrow(AssessData), ncol = 1)
  for (i in 1:nrow(AssessData)) {
    rowPoint <- AssessData[i,2]+(2*AssessData[i,3])+(3*AssessData[i,4])+
      (4*AssessData[i,5])+(5*AssessData[i,6])
    totalPoints[i,1] <- rowPoint
  }
  AssessData <- cbind(AssessData, totalPoints)
  return(AssessData)
}

#saving dataframes
n <- rbind(HCsum(NS), HCsum(SS), HCsum(CS), HCsum(B), HCsum(AH))
N <- rbind(HCsumall(NS), HCsumall(SS), HCsumall(CS), HCsumall(B), HCsumall(AH))
p <- pointsum(n)
P <- pointsumall(N)

#bringingittogether
AverageSectionScore <- matrix(data = NA, nrow = nrow(P), ncol = 2)
for (i in 1:nrow(P)){
  AverageSectionScore[i,1] <- P[i,1]
  AverageSectionScore[i,2] <- P[i,8]/P[i,7]
}
print(AverageSectionScore)


install.packages("Matching")
library(Matching)

#Setting up Matching 
#Variables to be matched on
X <- cbind(sections_by_day[,2],sections_by_day[,3])
#treatment
Tr <- sections_by_day[,6]

#Renaming Course Code to Numeric
X <- sub("AH",X,replacement =  17)
X <- sub("B",X,replacement =  2)
X <- sub("CS",X,replacement =  319)
X <- sub("NS",X,replacement =  1419)
X <- sub("SS",X,replacement =  1919)
names(X1) <- c("Course Code","Grader ID")
X1 <- cbind(as.numeric(X[,1]),as.numeric(X[,2]))

#Creating Outcome Variable
Y <- matrix(data = NA, nrow = nrow(X1), ncol = 1)
for (i in 1:nrow(Y)) {
  Y[i] <- AverageSectionScore[which(AverageSectionScore[,1]==sections_by_day[i,4]),2]
}

rr <- Match(Y=Y,Tr = Tr, X = X1,exact = TRUE )
summary(rr)
