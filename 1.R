
R version 3.4.1 (2017-06-30) -- "Single Candle"
Copyright (C) 2017 The R Foundation for Statistical Computing
Platform: x86_64-pc-linux-gnu (64-bit)

R is free software and comes with ABSOLUTELY NO WARRANTY.
You are welcome to redistribute it under certain conditions.
Type 'license()' or 'licence()' for distribution details.

  Natural language support but running in an English locale

R is a collaborative project with many contributors.
Type 'contributors()' for more information and
'citation()' on how to cite R or R packages in publications.

Type 'demo()' for some demos, 'help()' for on-line help, or
'help.start()' for an HTML browser interface to help.
Type 'q()' to quit R.

[Workspace loaded from ~/.RData]

> View(crime.data)
> crime.data <- subset(crime.data, !duplicated(crime.data$Case.Number))
> 
> summary(crime.data)
       ID             Case.Number                         Date                         Block       
 Min.   :   23059   G647504 :     1   01/01/2017 12:01:00 AM:   106   001XX N STATE ST    :   947  
 1st Qu.:10908410   HJ251782:     1   01/01/2017 12:00:00 AM:   102   008XX N MICHIGAN AVE:   521  
 Median :11009920   HY541341:     1   05/01/2017 09:00:00 AM:    48   0000X W TERMINAL ST :   429  
 Mean   :10980551   HZ178954:     1   03/01/2017 09:00:00 AM:    44   0000X N STATE ST    :   397  
 3rd Qu.:11104779   HZ244517:     1   01/01/2017 12:00:00 PM:    41   0000X S STATE ST    :   393  
 Max.   :11204417   HZ283873:     1   05/01/2017 12:00:00 PM:    40   006XX N MICHIGAN AVE:   311  
                    (Other) :265388   (Other)               :265013   (Other)             :262396  
      IUCR                    Primary.Type                    Description     Location.Description
 0820   : 24375   THEFT             :63931   SIMPLE                 : 28990   STREET   :59914     
 0486   : 23793   BATTERY           :49158   $500 AND UNDER         : 24375   RESIDENCE:44761     
 0460   : 16148   CRIMINAL DAMAGE   :29002   DOMESTIC BATTERY SIMPLE: 23793   APARTMENT:33012     
 0810   : 15219   ASSAULT           :19257   OVER $500              : 15219   SIDEWALK :20979     
 1310   : 13818   DECEPTIVE PRACTICE:17694   TO VEHICLE             : 14531   OTHER    :11020     
 1320   : 13539   OTHER OFFENSE     :16971   TO PROPERTY            : 13818   (Other)  :94856     
 (Other):158502   (Other)           :69381   (Other)                :144668   NA's     :  852     
   Arrest        Domestic           Beat         District          Ward       Community.Area 
 false:214449   false:223043   Min.   : 111   Min.   : 1.00   Min.   : 1.00   Min.   : 0.00  
 true : 50945   true : 42351   1st Qu.: 611   1st Qu.: 6.00   1st Qu.:10.00   1st Qu.:23.00  
                               Median :1031   Median :10.00   Median :24.00   Median :32.00  
                               Mean   :1146   Mean   :11.23   Mean   :23.28   Mean   :36.54  
                               3rd Qu.:1722   3rd Qu.:17.00   3rd Qu.:35.00   3rd Qu.:53.00  
                               Max.   :2535   Max.   :31.00   Max.   :50.00   Max.   :77.00  
                                                              NA's   :1                      
    FBI.Code      X.Coordinate      Y.Coordinate          Year                       Updated.On    
 06     :63931   Min.   :1094231   Min.   :1813894   Min.   :2017   02/14/2017 03:49:42 PM: 23674  
 08B    :41308   1st Qu.:1153388   1st Qu.:1859416   1st Qu.:2017   12/21/2017 03:46:09 PM: 11229  
 14     :29002   Median :1166692   Median :1894138   Median :2017   08/12/2017 03:49:53 PM:   804  
 26     :24440   Mean   :1164985   Mean   :1886628   Mean   :2017   08/11/2017 03:53:43 PM:   797  
 11     :15823   3rd Qu.:1176489   3rd Qu.:1909058   3rd Qu.:2017   08/26/2017 03:52:47 PM:   795  
 08A    :13635   Max.   :1205119   Max.   :1951535   Max.   :2017   08/01/2017 03:52:26 PM:   765  
 (Other):77255   NA's   :11808     NA's   :11808                    (Other)               :227330  
    Latitude       Longitude                               Location     
 Min.   :41.65   Min.   :-87.93   (41.883500187, -87.627876698):   525  
 1st Qu.:41.77   1st Qu.:-87.71   (41.897895128, -87.624096605):   281  
 Median :41.87   Median :-87.66   (41.754592961, -87.741528537):   255  
 Mean   :41.84   Mean   :-87.67   (41.742710224, -87.634088181):   218  
 3rd Qu.:41.91   3rd Qu.:-87.63   (41.868541914, -87.639235361):   213  
 Max.   :42.02   Max.   :-87.53   (Other)                      :252094  
 NA's   :11808   NA's   :11808    NA's                         : 11808  
> crime.data <- subset(crime.data, !is.na(crime.data$Latitude))
> 
> crime.data <- subset(crime.data, !is.na(crime.data$Longitude))
> 
> crime.data <- subset(crime.data, !is.na(crime.data$Ward))
> 
> head(crime.data$Date)
[1] 01/01/2017 01:00:00 AM 01/01/2017 01:23:00 AM 01/01/2017 12:01:00 AM 01/01/2017 01:00:00 AM 01/01/2017 01:45:00 AM 01/01/2017 02:46:00 AM
116550 Levels: 01/01/2017 01:00:00 AM 01/01/2017 01:00:00 PM 01/01/2017 01:04:00 AM 01/01/2017 01:05:00 AM 01/01/2017 01:05:00 PM ... 12/31/2017 12:55:00 PM
> crime.data$Date <- as.POSIXlt(crime.data$Date,format="%m/%d/%Y %H:%M")
> 
> head(crime.data$Date)
[1] "2017-01-01 01:00:00 IST" "2017-01-01 01:23:00 IST" "2017-01-01 12:01:00 IST" "2017-01-01 01:00:00 IST" "2017-01-01 01:45:00 IST" "2017-01-01 02:46:00 IST"
> library(chron)
> 
> crime.data$Time <- times(format(crime.data$Date, “%H:%M:%S”))
Error: unexpected input in "crime.data$Time <- times(format(crime.data$Date, �"
> 
> crime.data$Time <- times(format(crime.data$Date, "%H:%M:%S"))
> 
> time.tag <- chron(times = c("00:00:00", "06:00:00", "12:00:00", "18:00:00",
+                                "23:59:00"))
> 
> time.tag
[1] 00:00:00 06:00:00 12:00:00 18:00:00 23:59:00
> crime.data$time.tag <- cut(crime.data$Time, breaks=time.tag,
+                            labels=c("00-06","06-12", "12-18", "18-00"), include.lowest=TRUE)
> 
> table(crime.data$time.tag)

 00-06  06-12  12-18  18-00 
101837 135007  16741      0 
> crime.data$Date <- as.POSIXlt(strptime(crime.data$Date,format="%Y-%m-%d"))
> 
> crime.data$Day <- weekdays(crime.data$Date, abbreviate=TRUE)
> 
> crime.data$Month <- months(crime.data$Date, abbreviate=TRUE)
> 
> table(crime.data$Primary.Type)

                            ARSON                           ASSAULT                           BATTERY                          BURGLARY CONCEALED CARRY LICENSE VIOLATION 
                              439                             18867                             48163                             12445                                68 
                  CRIMINAL DAMAGE                 CRIMINAL TRESPASS               CRIM SEXUAL ASSAULT                DECEPTIVE PRACTICE                          GAMBLING 
                            28480                              6738                              1379                             14765                               191 
                         HOMICIDE                 HUMAN TRAFFICKING  INTERFERENCE WITH PUBLIC OFFICER                      INTIMIDATION                        KIDNAPPING 
                              612                                 6                              1075                               140                               188 
             LIQUOR LAW VIOLATION               MOTOR VEHICLE THEFT                         NARCOTICS                      NON-CRIMINAL  NON-CRIMINAL (SUBJECT SPECIFIED) 
                              177                             11138                              9715                                32                                 2 
                        OBSCENITY        OFFENSE INVOLVING CHILDREN          OTHER NARCOTIC VIOLATION                     OTHER OFFENSE                      PROSTITUTION 
                               65                              1897                                11                             16027                               719 
                 PUBLIC INDECENCY            PUBLIC PEACE VIOLATION                           ROBBERY                       SEX OFFENSE                          STALKING 
                                8                              1476                             11667                               823                               153 
                            THEFT                 WEAPONS VIOLATION 
                            61498                              4621 
> 
> length(unique(crime.data$Primary.Type))
[1] 32
> crime.data$Crime <- as.character(crime.data$Primary.Type)
> 
> crime.data$crime <- ifelse(crime.data$crime %in% c("CRIM SEXUAL ASSAULT", "PROSTITUTION", "SEX OFFENSE"), 'SEX', crime.data$Crime)
Error in `$<-.data.frame`(`*tmp*`, crime, value = logical(0)) : 
  replacement has 0 rows, data has 253585
> 
> crime.data$Crime <- ifelse(crime.data$Crime %in% c("CRIM SEXUAL ASSAULT", "PROSTITUTION", "SEX OFFENSE"), 'SEX', crime.data$Crime)
> 
> crime.data$Crime <- ifelse(crime.data$Crime %in% c("MOTOR VEHICLE THEFT"),"MVT", crime.data$Crime)
> 
> crime.data$Crime <- ifelse(crime.data$Crime %in% c("GAMBLING", "INTERFERENCE WITH PUBLIC OFFICER", "INTIMIDATION",
+                                                     "LIQUOR LAW VIOLATION", "OBSCENITY", "NON-CRIMINAL", "PUBLIC PEACE VIOLATION",
+                                                     "PUBLIC INDECENCY", "STALKING", "NON-CRIMINAL (SUBJECT SPECIFIED)"),"NONVIO", crime.data$Crime)
> 
>                                                     
> crime.data$crime <- ifelse(crime.data$crime=="CRIMINAL DAMAGE", "DAMAGE",crime.data$Crime)
Error in `$<-.data.frame`(`*tmp*`, crime, value = logical(0)) : 
  replacement has 0 rows, data has 253585
> 
> crime.data$Crime <- ifelse(crime.data$crime=="CRIMINAL DAMAGE", "DAMAGE",crime.data$Crime)
Error in `$<-.data.frame`(`*tmp*`, Crime, value = logical(0)) : 
  replacement has 0 rows, data has 253585
> 
> crime.data$Crime <- ifelse(crime.data$Crime=="CRIMINAL DAMAGE", "DAMAGE",crime.data$Crime)
> 
> crime.data$Crime <- ifelse(crime.data$Crime == "CRIMINAL TRESPASS","TRESPASS", crime.data$Crime)
> 
> crime.data$Crime <- ifelse(crime.data$Crime %in% c("NARCOTICS", "OTHER NARCOTIC VIOLATION", "OTHER NARCOTIC VIOLATION"), "DRUG", crime.data$Crime)
> 
> crime.data$Crime <- ifelse(crime.data$Crime == "DECEPTIVE PRACTICE", "FRAUD", crime.data$Crime)
> 
> crime.data$Crime <- ifelse(crime.data$Crime %in% c("OTHER OFFENSE", "OTHER OFFENSE"), "OTHER", crime.data$Crime)
> 
> crime.data$Crime <- ifelse(crime.data$Crime %in% c("KIDNAPPING", "WEAPONS VIOLATION", "OFFENSE INVOLVING CHILDREN","CONCEALED CARRY LICENSE VIOLATION"), "VIO", crime.data$Crime)
> 
> table(crime.data$Crime)

            ARSON           ASSAULT           BATTERY          BURGLARY            DAMAGE              DRUG             FRAUD          HOMICIDE HUMAN TRAFFICKING 
              439             18867             48163             12445             28480              9726             14765               612                 6 
              MVT            NONVIO             OTHER           ROBBERY               SEX             THEFT          TRESPASS               VIO 
            11138              3319             16027             11667              2921             61498              6738              6774 

> crime.data$Arrest <- ifelse(as.character(crime.data$Arrest) == "true", 1, 0)
> 
> library(ggplot2)
> qplot(crime.data$Crime, xlab="Crime", main="Crimes in Chicago")+scale_y_continuous("Number of crimes")
> 
> qplot(crime.data$time.tag, xlab="Time of day", main="Crimes by time of
+       day")+scale_y_continuous("Number of crimes")
> 
> crime.data$Day <- factor(crime.data$Day, levels=c("Mon", "Tue", "Wed","Thu", "Fri", "Sat","Sun"))
> 
> qplot(crime.data$Day, xlab="Day of week", main="Crimes by day of week")+scale_y_continuous("Number of crimes")
> 
> crime.data$Month <- factor(crime.data$Month, levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
> 
> qplot(crime.data$Month, xlab="Month",main="Crimes by month")+scale_y_continuous("Number of crimes")
> 
> temp <- aggregate(crime.data$Crime, by=list(crime.data$Crime,crime.data$time.tag),FUN=length)
> 
> names(temp) <- c("Crime","time.tag", "count")
> 
> ggplot(temp,aes(x=Crime, y=factor(time.tag)))+geom_tile(aes(fill=count))+scale_x_discrete("Crime", expand=c(0,0))+scale_y_discrete("Time of day",expand=c(0,-2))+scale_fill_gradient("Number of crimes",low="white",high="steelblue")+theme_bw()+ggtitle("Crimes by time of day")+theme(panel.grid.major=element_line(colour=NA), panel.grid.minor=element_line(colour=NA))
> temp <- aggregate(crime.data$Crime, by=list(crime.data$Crime,crime.data$Day),FUN=length)
> names(temp) <- c("Crime","Day", "count")
> ggplot(temp,aes(x=Crime, y=factor(Day)))+geom_tile(aes(fill=count))+scale_x_discrete("Crime", expand=c(0,0))+scale_y_discrete("Day of week",expand=c(0,-2))+scale_fill_gradient("Number of crimes",low="white",high="steelblue")+theme_bw()+ggtitle("Crimes by days of week")+theme(panel.grid.major=element_line(colour=NA), panel.grid.minor=element_line(colour=NA))
> temp <- aggregate(crime.data$Crime, by=list(crime.data$Crime,crime.data$Month),FUN=length)
> names(temp) <- c("Crime","Month", "count")
> ggplot(temp,aes(x=Crime, y=factor(Month)))+geom_tile(aes(fill=count))+scale_x_discrete("Crime", expand=c(0,0))+scale_y_discrete("Month",expand=c(0,-2))+scale_fill_gradient("Number of crimes",low="white",high="steelblue")+theme_bw()+ggtitle("Crimes by month")+theme(panel.grid.major=element_line(colour=NA), panel.grid.minor=element_line(colour=NA))
>  
> length(unique(crime.agg$Beat))
[1] 274
> length(unique(crime.agg$Date))
[1] 365
> 
> beats <- sort(unique(crime.agg$Beat))
> dates <- sort(as.character(unique(crime.agg$Date)))
> temp <- expand.grid(beats, dates)
> names(temp) <- c(“Beat”, “Date”)
Error: unexpected input in "names(temp) <- c(�"
> names(temp) <- c(“Beat”, “Date)
Error: unexpected input in "names(temp) <- c(�"
> names(temp) <- c("Beat", "Date")
> temp <- orderBy(~Beat, data=temp)
Error in orderBy(~Beat, data = temp) : could not find function "orderBy"
> library(doBy)
> temp <- orderBy(~Beat, data=temp)
> model.data <- aggregate(crime.agg[, c('count', 'Arrest')], by=list(crime.agg$Beat,as.character(crime.agg$Date)), FUN=sum)
> 
> names(model.data) <- c("Beat", "Date", "count" "Arrest")
Error: unexpected string constant in "names(model.data) <- c("Beat", "Date", "count" "Arrest""
> names(model.data) <- c("Beat", "Date", "count", "Arrest")
> model.data <- merge(temp, model.data, by=c('Beat', 'Date'), all.x=TRUE)
> 
> View(model.data)
> 
> model.data$count[is.na(model.data$count)] <- 0
> model.data$Arrest[is.na(model.data$Arrest)] <- 0
> model.data$Day <- weekdays(as.Date(model.data$Date), abbreviate=TRUE)
> model.data$Month <- months(as.Date(model.data$Date), abbreviate=TRUE)
> pastDays <- function(x) {
+     c(0,rep(1,x))
+ }
> model.data$past.crime.1 <- ave(model.data$count, model.data$Beat,FUN=function(x) filter(x, pastDays(1), sides=1))
> model.data$past.crime.7 <- ave(model.data$count, model.data$Beat,FUN=function(x) filter(x, pastDays(7), sides=1))
> model.data$past.crime.30 <- ave(model.data$count, model.data$Beat,FUN=function(x) filter(x, pastDays(30), sides=1))
> meanNA <- function(x){
+     mean(x, na.rm=TRUE)
+ }
> 
> model.data$past.crime.1 <- ifelse(is.na(model.data$past.crime.1),meanNA(model.data$past.crime.1), model.data$past.crime.1)
> 
> model.data$past.crime.7 <- ifelse(is.na(model.data$past.crime.7),meanNA(model.data$past.crime.7), model.data$past.crime.7)
> 
> model.data$past.crime.30 <- ifelse(is.na(model.data$past.crime.30),meanNA(model.data$past.crime.30), model.data$past.crime.30)
> 
> model.data$past.arrest.30 <- ave(model.data$Arrest, model.data$Beat,FUN=function(x) filter(x, pastDays(30), sides=1))
> 
> model.data$past.arrest.30 <- ifelse(is.na(model.data$past.arrest.30),meanNA(model.data$past.arrest.30), model.data$past.arrest.30)
> 
> cor(model.data$past.crime.30, model.data$past.arrest.30)
[1] 0.7287231
> model.data$policing <- ifelse(model.data$past.crime.30==0, 0,model.data$past.arrest.30/model.data$past.crime.30)
> 
> model.data$crime.trend <- ifelse(model.data$past.crime.30==0, 0,model.data$past.crime.7/model.data$past.crime.30)
> 
> 
> model.data$season <- as.factor(ifelse(model.data$Month %in% c("Mar", "Apr","May"),"spring",ifelse(model.data$Month %in% c("Jun","Jul","Aug"),"summer",ifelse(model.data$Month %in% c("Sep","Oct","Nov"), "fall","winter"))))
> 
> library(psych)
> model.cor <- cor(model.data[, c('count','past.crime.1','past.crime.7','past.crime.30','policing','crime.trend')])
> 
> cor.plot(model.cor)
> model.data <- orderBy(~Date, data=model.data)
> rows <- c(1:floor(nrow(model.data)*0.9))
> 
> test.data <- model.data[-rows, ]
> model.data <- model.data[rows, ]
> mean(model.data$count)
[1] 2.561088
> 
> var(model.data$count)
[1] 4.232272
> 
> library(MASS)
> crime.model <- glm.nb(count ~ past.crime.1 + past.crime.7 + past.crime.30 + policing + crime.trend + factor(Day)+ season, data=model.data)
> 
> summary(crime.model)

Call:
glm.nb(formula = count ~ past.crime.1 + past.crime.7 + past.crime.30 + 
    policing + crime.trend + factor(Day) + season, data = model.data, 
    init.theta = 11.18973973, link = log)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-6.1097  -0.8303  -0.1327   0.5336   9.7746  

Coefficients:
                 Estimate Std. Error z value Pr(>|z|)    
(Intercept)     0.0278106  0.0180036   1.545 0.122413    
past.crime.1    0.0162958  0.0012419  13.122  < 2e-16 ***
past.crime.7    0.0026826  0.0006721   3.991 6.57e-05 ***
past.crime.30   0.0083654  0.0001680  49.794  < 2e-16 ***
policing        0.0891092  0.0261925   3.402 0.000669 ***
crime.trend     0.5710397  0.0641552   8.901  < 2e-16 ***
factor(Day)Mon -0.0349725  0.0086440  -4.046 5.21e-05 ***
factor(Day)Sat -0.0366363  0.0086655  -4.228 2.36e-05 ***
factor(Day)Sun -0.0511096  0.0086706  -5.895 3.76e-09 ***
factor(Day)Thu -0.0561990  0.0086835  -6.472 9.68e-11 ***
factor(Day)Tue -0.0521381  0.0086738  -6.011 1.84e-09 ***
factor(Day)Wed -0.0627775  0.0086934  -7.221 5.15e-13 ***
seasonspring    0.0206372  0.0064656   3.192 0.001414 ** 
seasonsummer    0.0231829  0.0062871   3.687 0.000227 ***
seasonwinter    0.0008776  0.0072905   0.120 0.904182    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for Negative Binomial(11.1897) family taken to be 1)

    Null deviance: 123412  on 90008  degrees of freedom
Residual deviance:  99585  on 89994  degrees of freedom
AIC: 337386

Number of Fisher Scoring iterations: 1


              Theta:  11.190 
          Std. Err.:  0.269 

 2 x log-likelihood:  -337354.054 
> crime.model.pred <- predict(crime.model, test.data, type="response")
> 
> sqrt(mean((test.data$count - crime.model.pred)^ 2))
[1] 1.725265
> crime.model <- glm.nb(count~ past.crime.1 + past.crime.7 + past.crime.30 +crime.trend+policing + factor(day) + season + I(past.crime.30^ 2),data=model.data)
Error in factor(day) : object 'day' not found
> 
> crime.model <- glm.nb(count~ past.crime.1 + past.crime.7 + past.crime.30 +crime.trend+policing + factor(Day) + season + I(past.crime.30^ 2),data=model.data)
> 
> summary(crime.model)

Call:
glm.nb(formula = count ~ past.crime.1 + past.crime.7 + past.crime.30 + 
    crime.trend + policing + factor(Day) + season + I(past.crime.30^2), 
    data = model.data, init.theta = 14.07650647, link = log)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-3.3604  -0.8503  -0.1461   0.5433   9.9105  

Coefficients:
                     Estimate Std. Error z value Pr(>|z|)    
(Intercept)        -3.866e-01  2.122e-02 -18.220  < 2e-16 ***
past.crime.1        1.648e-02  1.232e-03  13.378  < 2e-16 ***
past.crime.7        1.354e-03  8.123e-04   1.667 0.095466 .  
past.crime.30       1.728e-02  2.667e-04  64.796  < 2e-16 ***
crime.trend         6.930e-01  7.231e-02   9.584  < 2e-16 ***
policing           -8.658e-02  2.669e-02  -3.243 0.001182 ** 
factor(Day)Mon     -3.540e-02  8.467e-03  -4.181 2.90e-05 ***
factor(Day)Sat     -3.587e-02  8.485e-03  -4.227 2.37e-05 ***
factor(Day)Sun     -5.055e-02  8.492e-03  -5.952 2.64e-09 ***
factor(Day)Thu     -5.672e-02  8.507e-03  -6.668 2.60e-11 ***
factor(Day)Tue     -5.270e-02  8.498e-03  -6.202 5.58e-10 ***
factor(Day)Wed     -6.401e-02  8.518e-03  -7.514 5.72e-14 ***
seasonspring        2.670e-02  6.340e-03   4.211 2.54e-05 ***
seasonsummer        2.139e-02  6.153e-03   3.476 0.000508 ***
seasonwinter       -1.252e-02  7.158e-03  -1.750 0.080184 .  
I(past.crime.30^2) -3.660e-05  8.002e-07 -45.737  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for Negative Binomial(14.0765) family taken to be 1)

    Null deviance: 127731  on 90008  degrees of freedom
Residual deviance: 100644  on 89993  degrees of freedom
AIC: 335144

Number of Fisher Scoring iterations: 1


              Theta:  14.077 
          Std. Err.:  0.425 

 2 x log-likelihood:  -335110.031 
> crime.model.pred <- predict(crime.model, test.data, type="response")
> sqrt(mean((test.data$count - crime.model.pred)^ 2))
[1] 1.688651
> validate <- data.frame(test.data$count, crime.model.pred)
> names(validate) <- c("actual", "predicted")
> 
> validate$bucket <- with(validate, cut(predicted, breaks=quantile(predicted, probs=seq(0, 1, 0.1)),include.lowest=TRUE, labels=c(1:10)))
> validate <- aggregate(validate[, c('actual', 'predicted')], by=list(validate$bucket), FUN=mean)
> plot(validate$predicted, col= "red", type="l", lwd=1.5, ylab="No. of Crashes", xlab="Predicted Crimes Decile", main="Actual vs. Predicted")
> 
> lines(validate$actual, col="blue", lwd=1.5)
> legend("topright", c("Actual","Predicted"), col=c("blue","red"), lwd=c(1.5, 1.5),bty="n")
>