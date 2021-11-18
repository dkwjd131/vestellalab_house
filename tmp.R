install.packages("digest")
library(digest)
txt<-"1"
txt1<-digest(txt,algo="md5",serialize = F)
txt2<-digest("0",algo="md5",serialize = F)

result_1<-result[result$GRADE=="A",]
result_2<-result[result$GRADE=="B",]
result_2<-result_2[row_number(result_2$TIME)<=1000,]
result_tmp<-bind_rows(result_2,result_1)
rm(result_1,result_2)
