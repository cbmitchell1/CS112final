
cleanData <- function(NS) {
  #more friendly column names
  NS <- setNames(NS, c("Course", "Grader", "Section", "Class", "Assignment", "Hashtag", "College"
                       , "1", "2", "3", "4", "6"))
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
  return(AssessData)
}

#finding total points awarded for an HC in a section
pointsum <- function(AssessData){
  for (i in 1:nrow(AssessData)) {
    points <- AssessData[i,8]+2*AssessData[i,9]+3*AssessData[i,10]+
      4*AssessData[i,11]+5*AssessData[i,12]
  }
  AssessData <- cbind(AssessData, points)
  return(AssessData)
}

#finding total points for awarded for an HC in all sections

#saving dataframes
n <- rbind(HCsum(NS), HCsum(SS), HCsum(CS), HCsum(B), HCsum(AH))
N <- rbind(HCsumall(NS), HCsumall(SS), HCsumall(CS), HCsumall(B), HCsumall(AH))
p <- pointsum(n)
P <- pointsum(N)

#bringingittogether

write.csv(NS_N)


