library(data.table)
library(ggpubr)
library(ggplot2)
setwd("C:\\Users\\KHG\\Desktop\\치안 방범대\\분석\\ANOVA")

####################################################################
####################################################################
## elder accident ##
elder <- fread("elder_accratio.csv", encoding = "UTF-8")
elder$V2=as.factor(elder$V2)


# boxplot and summary
attach(elder)
boxplot(V1 ~ V2, 
        main = "Boxplot of Elder Accident Ratio", 
        xlab = "Factor Levels : City", 
        ylab = "Monthly Accident Ratio")

ggplot(data=elder, aes(x=V2, y=V1))+
        ggtitle("Boxplot of Elder Accident Ratio") +
        labs(y="Elder Accident Ratio", x="City")+
        geom_boxplot(alpha=0.1,aes(col=V2,fill=V2))+
        geom_jitter(alpha=0.3, aes(col=V2),width=0.05)+theme_bw()+
        scale_x_discrete(labels = c("세종시","아산시","익산시","춘천시","경산시"))+
        theme(text=element_text(size=14,face="bold"), plot.title=element_text(hjust=0.5, size=16,face="bold"))


tapply(V1, V2, summary)
detach(elder)

summary(aov(V1 ~ V2, data = elder)) #p-value = 0.043

elder_2 = elder[V2=="A" | V2=="B"|V2=="D" | V2=="E"]
summary(aov(V1 ~ V2, data = elder_2)) #p-value=0.891

####################################################################
####################################################################
## total accident ## 
total <- fread("totalaccratio.csv")

# boxplot and summary
attach(total)
boxplot(V1 ~ V2, 
        main = "Boxplot of Total Accident Count", 
        xlab = "Factor Levels : City", 
        ylab = "Monthly Accident Count")

ggplot(data=total, aes(x=V2, y=V1))+
        ggtitle("Boxplot of Total Accident Ratio") +
        labs(y="Total Accident Ratio", x="City")+
        geom_boxplot(alpha=0.1,aes(col=V2,fill=V2))+
        geom_jitter(alpha=0.3, aes(col=V2),width=0.05)+theme_bw()+
        scale_x_discrete(labels = c("세종시","아산시","익산시","춘천시","경산시"))+
        theme(text=element_text(size=14,face="bold"), plot.title=element_text(hjust=0.5, size=16,face="bold"))

tapply(V1, V2, summary)
detach(total)

summary(aov(V1 ~ V2, data = total)) #p-value is very small

total_2 = total[V2=="A" | V2=="B"]
summary(aov(V1 ~ V2, data = total_2)) #p-value=0.364


####################################################################
####################################################################
## bike accident ## 
bike <- fread("bike_accratio.csv")

# boxplot and summary
attach(bike)
boxplot(V1 ~ V2, 
        main = "Boxplot of Bike Accident Ratio", 
        xlab = "Factor Levels : City", 
        ylab = "Monthly Accident Ratio")

ggplot(data=bike, aes(x=V2, y=V1))+
        ggtitle("Boxplot of Bike Accident Ratio") +
        labs(y="Bike Accident Ratio", x="City")+
        geom_boxplot(alpha=0.1,aes(col=V2,fill=V2))+
        geom_jitter(alpha=0.3, aes(col=V2),width=0.05)+theme_bw()+
        scale_x_discrete(labels = c("세종시","아산시","익산시","춘천시","경산시"))+
        theme(text=element_text(size=14,face="bold"), plot.title=element_text(hjust=0.5, size=16,face="bold"))


tapply(V1, V2, summary)
detach(bike)

summary(aov(V1 ~ V2, data = bike)) #p-value is very small

bike_2 = bike[V2=="A" | V2=="C"| V2=="E"]
summary(aov(V1 ~ V2, data = bike_2)) #p-value=0.608
