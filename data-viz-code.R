library(tidyverse)
#sba_data0 <- read_csv("C:/Users/kinson2/Box/Employee Documents/Teaching Assistant Professor/datathon2023/SBAnational.csv")
sba_data <- read_csv("C:/Users/kinson2/Box/Employee Documents/Teaching Assistant Professor/datathon2023/SBAnational.csv", 
    col_types = cols(ApprovalDate = col_date(format = "%d-%b-%y"), 
        ChgOffDate = col_date(format = "%d-%b-%y"), 
        DisbursementDate = col_date(format = "%d-%b-%y"), 
        DisbursementGross = col_number(), 
        BalanceGross = col_number(), ChgOffPrinGr = col_number(), 
        GrAppv = col_number(), SBA_Appv = col_number()))

#table(sba_data$MIS_Status)

#months
sba_data$MonthsToDefault <- as.vector((sba_data$ChgOffDate-sba_data$ApprovalDate)/(365.25/12))
ids0<-which(sba_data$MonthsToDefault<0)
#print(sba_data[ids0,],width=Inf)
#print(sba_data0[ids0,],width=Inf)

#sba_data[ids0,"ChgOffDate"]
sba_data[ids0,"ApprovalDate"] <- as.Date(c("1961-12-07","1964-11-27","1966-05-18","1967-02-28","1967-07-20","1967-04-25","1968-02-09","1968-12-03","1968-09-24"))
sba_data$MonthsToDefault <- as.vector((sba_data$ChgOffDate-sba_data$ApprovalDate)/(365.25/12))

#which(sba_data$ChgOffDate%in%range(sba_data$ChgOffDate, na.rm=TRUE)[2])
#print(sba_data[which(sba_data$ChgOffDate%in%range(sba_data$ChgOffDate, na.rm=TRUE)[2]),],width=Inf)
#print(sba_data0[which(sba_data$ChgOffDate%in%range(sba_data$ChgOffDate, na.rm=TRUE)[2]),],width=Inf)
sba_data1<-sba_data[-which(sba_data$ChgOffDate%in%range(sba_data$ChgOffDate, na.rm=TRUE)[2]),]
rm(sba_data)

#survival plots: “How long are businesses taking to default on their loan?”
#hist(sba_data$MonthsToDefault)
#boxplot(sba_data$MonthsToDefault)

dates <- unique(c(sba_data1$ApprovalDate,sba_data1$ChgOffDate))
#range(dates, na.rm=TRUE)
ad <- sort(table(sba_data1$ApprovalDate))
tail(ad, 200)
sort(names(ad))[5500:6000]
#sum(sba_data1$ApprovalDate=="2004-01-01")
#ad[names(ad)=="2000-01-03"]
#ad[names(ad)=="2000-01-31"]
#"2000-01-01"
#[313] "2000-01-03" "2000-01-04" "2000-01-05" "2000-01-06"
#[317] "2000-01-07" "2000-01-10" "2000-01-11" "2000-01-12"
#[321] "2000-01-13" "2000-01-14" "2000-01-18" "2000-01-19"
#[325] "2000-01-20" "2000-01-21" "2000-01-24" "2000-01-25"
#[329] "2000-01-26" "2000-01-27" "2000-01-28" "2000-01-31"

#sba_data2 <- sba_data2 %>% filter(ApprovalDate=='2000-01-03') %>%
#  arrange(GrAppv) %>% mutate(idNum=row.names(.))
#print(sba_data2, width=Inf)
#write_csv(sba_data2, "C:/Users/kinson2/Box/Employee Documents/Teaching Assistant Professor/datathon2023/sba-small.csv")

  
library(tidyverse)
sba_data2 <- read_csv("C:/Users/kinson2/Box/Employee Documents/Teaching Assistant Professor/datathon2023/sba-small.csv")
sba_pif<- sba_data2 %>% filter(MIS_Status=="P I F")
#ids_pif <- runif(nrow(sba_pif))
sba_def<- sba_data2 %>% filter(MIS_Status=="CHGOFF")
#ids_def <- runif(nrow(sba_def))
#rm(dates)
#rm(sba_data1)
mean(sba_data2$MonthsToDefault,na.rm=TRUE)
median(sba_data2$MonthsToDefault,na.rm=TRUE)
xx<-seq(as.Date("2000-01-03"),as.Date("2010-01-03"),by=1)
#=paste0(2000:2009,"-01-03")
xx1<-cbind(sba_data2$idNum[sba_data2$MIS_Status=="P I F"],sba_pif$ApprovalDate+(sba_pif$Term)*(365.25/12),as.Date(xx[3654]),sba_data2$ChgOffDate[sba_data2$MIS_Status=="P I F"],sba_data2$ApprovalDate[sba_data2$MIS_Status=="P I F"])
colnames(xx1) <- c("idNum","Orig", "Fixed","ChgOffDate","ApprovalDate")
xx2<-as.data.frame(xx1) %>%
  mutate(New=pmin(Orig,Fixed)) %>%
  mutate(ApprovalDate=as.Date(ApprovalDate,"1970-01-01"),
         New2 = as.Date(New, "1970-01-01")) %>%
  mutate(Months2=as.vector(New2-ApprovalDate)/(365.25/12)) %>%
  mutate(status=0)

#sba_data2$MonthsToDefault2 <- sba_data2$MonthsToDefault
#if_else(sba_data2$MIS_Status=="P I F", ,)

#textbook version
plot(x=xx,y=rep(0.5,length(xx)), type="n", ylim=c(1,nrow(sba_data2)),xlim=c(xx[1],xx[3654]), ylab="",xlab="", axes=FALSE)
segments(sba_pif$ApprovalDate,as.numeric(sba_pif$idNum),xx2$New2,as.numeric(sba_pif$idNum), col=rgb(127.5/255,127.5/255,127.5/255,alpha=0.15), lwd=2)
segments(sba_def$ApprovalDate,as.numeric(sba_def$idNum),sba_def$ChgOffDate,as.numeric(sba_def$idNum), col="#007DBC", lwd=2)
title(expression("Time until" * phantom(" Default ") * "on Small Business Administration Loans"), adj=0, col.main="black", cex.main=1.5)
title(expression(phantom("Time until") * " Default " * phantom("on Small Business Administration Loans")), adj=0, col.main="#007DBC", cex.main=1.5)
#title(expression("title (" * phantom("slope=1)") * ")"), col.main = "black")
#title(expression(phantom("title (") * "slope=1"), col.main = "red")
title(xlab="Date")
title(main= "Grey lines represent censored data", col.main="lightgrey", line=0.5, cex.main=1, adj=0)
#title(ylab="Gross\nApproved\n\nIncreasing", las=1)
axis(1,at=xx[c(1,732,1462,2193,2923,3654)],labels=as.character(xx[c(1,732,1462,2193,2923,3654)]), col="lightgray" )
#c(1,367,732,1097,1462,1828,2193,2558,2923,3289) 10 years
axis(2, at=sba_data2$idNum[c(1,75,nrow(sba_data2))],
     labels=c(paste0("$",0.001*sba_data2$GrAppv[c(1)],"K"),"Gross\nApproved\nAmount\nIn\nIncreasing\nOrder",paste0("$",0.001*sba_data2$GrAppv[c(nrow(sba_data2))],"K")), 
     las=1, tick = FALSE,line = 1.25, hadj=0)
#grid("2010-01-03",NA,lwd=2)
#abline(v=xx[3654], lty=3)


###################
#kaplan-meier plot#
###################
library(survival)
sba_data3 <-bind_rows(transmute(sba_def,Months2=MonthsToDefault,status=1),select(xx2,Months2,status))
fit.surv <- survfit(Surv(Months2, status) ~ 1, data=sba_data3)
plot(fit.surv, xlab = "Months",
    main = "Estimated Probability of Compliance", axes=FALSE, col="#007DBC", lwd=2)
axis(2, at=seq(0,1,0.2), col = "lightgrey", las=1)
axis(1, at=c(1,seq(12,120,12)), col="lightgrey")
