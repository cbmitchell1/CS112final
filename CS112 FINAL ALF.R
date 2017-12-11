
mw_classes <- dplyr::filter(AY201617_Section_Descriptions ,grepl("MW",AY201617_Section_Descriptions$`Cerebus Assessmentreport Section Title`))

tt_classes <- dplyr::filter(AY201617_Section_Descriptions ,grepl("TT",AY201617_Section_Descriptions$`Cerebus Assessmentreport Section Title`))

class_section <- AY201617_Section_Descriptions

install.packages("cem",repos="http://r.iq.harvard.edu", type="source")
library(cem)

day_treat <- 1
tt_classes <- cbind(tt_classes,day_treat)
day_treat <- 0 
mw_classes <- cbind(mw_classes,day_treat)
sections_by_day <- rbind(mw_classes,tt_classes)

mat <- cem(treatment = day_treat, data = sections_by_day,drop = c(sections_by_day$`Cerebus Assessmentreport Term Title`,sections_by_day$`Cerebus Assessmentreport Section ID`,sections_by_day$`Cerebus Assessmentreport Section Title`,sections_by_day$day_treat),keep.all = FALSE)

data_a <- NULL

for (i in 1:length(mw_classes)) {
  data_a[i,1] <- mw_classes[i,2] #course_code
  data_a[i,2] <- mw_classes[i,3] #instructor code
  for (j in 1:length(tt_classes)) {
    find()
  }
  
}

