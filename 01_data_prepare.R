#Load the messy data from qualtrics
d<-read.csv("Qexport.csv",sep=",",header=T,stringsAsFactors = F)

dim(d)

names(d)
names(d)[18]<-"Agreement"

questions<-d[1,]
questions[which(names(d)=="age")]

#View(d)

names(d)[which(names(d)=="Q12")]<-"Age"
names(d)[which(names(d)=="Q13")]<-"Gender"

names(d)[which(names(d)=="Q49_1")]<-"danger.for.participant"
names(d)[which(names(d)=="Q10_1")]<-"donation"
names(d)[which(names(d)=="Q4_38")]<-"move.to.middle"
names(d)[which(names(d)=="Q11")]<-"email"
names(d)[which(names(d)=="isNew")]<-"is.new"
names(d)[which(names(d)=="Q_TotalDuration")]<-"duration"
names(d)[which(names(d)=="age")]<-"age.cohort"

names(d)

summary(as.numeric(as.character((d[3:100,which(names(d)=="donation")]))))

nrow(d)

dsave<-d[-c(1,2),]

nrow(dsave)

#View(dsave)
names(dsave)

#Create questions vector again with up to date colum names
questions<-d[1,]

write.table(dsave,"raw_data.txt",sep="\t",row.names = F)
write.table(questions,"questions.txt",sep="\t",row.names = F)


#Load the raw text data anew for a fresh start
d<-read.table("raw_data.txt",sep="\t",header=T,stringsAsFactors = F)
questions<-read.table("questions.txt",sep="\t",header=T,stringsAsFactors = F)


summary(d)
questions

#function that takes last N characters
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

minmax<-function(x){
  max<-max(x,na.rm=T)
  min<-min(x,na.rm=T)
  if(max==min){
    return(max)
  }else{
    return("conflict")
  }
}

whichcond<-function(x){
  max<-max(x,na.rm=T)
  min<-min(x,na.rm=T)
  if(max==min){
    which(x==max)
  }else{
    return("conflict")
  }
}

names(d)

nam<-names(d)

questions[which(names(d)=="X5_Q7_1")]


ATTENTION_CHECK<-apply(d[,substrRight(nam,5)=="Q50_1"],1,minmax)
INT_DIR_CONTACT<-apply(d[,substrRight(nam,4)=="Q7_1"],1,minmax)
INT_QUARANTINE<-apply(d[,substrRight(nam,4)=="Q7_2"],1,minmax)
INT_STAY_HOME<-apply(d[,substrRight(nam,4)=="Q7_3"],1,minmax)
INT_WASH_HANDS<-apply(d[,substrRight(nam,5)=="Q7_11"],1,minmax)
INT_CANCEL_VAC<-apply(d[,substrRight(nam,5)=="Q7_12"],1,minmax)
INT_NEIGHBOR_SHOP<-apply(d[,substrRight(nam,5)=="Q7_13"],1,minmax)
INT_NOT_HOARD<-apply(d[,substrRight(nam,5)=="Q7_14"],1,minmax)
INT_NEIGHBOR_CHILD<-apply(d[,substrRight(nam,5)=="Q7_15"],1,minmax)
INT_SUPPORT_COMP<-apply(d[,substrRight(nam,5)=="Q7_16"],1,minmax)

ATTENTION_CHECK
INT_DIR_CONTACT
INT_QUARANTINE
INT_STAY_HOME
INT_WASH_HANDS
INT_CANCEL_VAC
INT_NEIGHBOR_SHOP
INT_NOT_HOARD
INT_NEIGHBOR_CHILD
INT_SUPPORT_COMP

COND<-apply(d[,substrRight(nam,5)=="Q50_1"],1,whichcond)
CONDlabel<-as.factor(COND)
levels(CONDlabel)<-c("Control","Prudence","General","Family","Group","Reciprocity","Heroism","Deference","Fainess","Property")

c(1:ncol(d))[substr(names(d),1,2)=="Q4"]
nam[substr(names(d),1,2)=="Q4"]
            
nam[substr(names(d),1,2)=="Q4"]<-
  c("FAM1_MAC","LOY1_MAC","REC1_MAC","HER1_MAC","DEF1_MAC","FAI1_MAC","PRO1_MAC",              
    "FAM2_MAC","LOY2_MAC","REC2_MAC","HER2_MAC","DEF2_MAC","FAI2_MAC","PRO2_MAC",
    "FAM3_MAC","LOY3_MAC","REC3_MAC","HER3_MAC","DEF3_MAC","FAI3_MAC","PRO3_MAC")

MAC.fam<-rowMeans(d[,substr(nam,1,3)=="FAM"])
MAC.gro<-rowMeans(d[,substr(nam,1,3)=="LOY"])
MAC.rec<-rowMeans(d[,substr(nam,1,3)=="REC"])
MAC.her<-rowMeans(d[,substr(nam,1,3)=="HER"])
MAC.def<-rowMeans(d[,substr(nam,1,3)=="DEF"])
MAC.fai<-rowMeans(d[,substr(nam,1,3)=="FAI"])
MAC.pro<-rowMeans(d[,substr(nam,1,3)=="PRO"])

MAC.fam
MAC.gro
MAC.rec
MAC.her
MAC.def
MAC.fai
MAC.pro


names(d)[substr(names(d),1,2)=="Q4"]<-
  c("FAM1_MAC","LOY1_MAC","REC1_MAC","HER1_MAC","DEF1_MAC","FAI1_MAC","PRO1_MAC",              
    "FAM2_MAC","LOY2_MAC","REC2_MAC","HER2_MAC","DEF2_MAC","FAI2_MAC","PRO2_MAC",
    "FAM3_MAC","LOY3_MAC","REC3_MAC","HER3_MAC","DEF3_MAC","FAI3_MAC","PRO3_MAC")
names(d)

dnew<-data.frame(d[,c(1:44)],
                 MAC.fam,
                 MAC.gro,
                 MAC.rec,
                 MAC.her,
                 MAC.def,
                 MAC.fai,
                 MAC.pro,
                 ATTENTION_CHECK,
                 INT_DIR_CONTACT,
                 INT_QUARANTINE,
                 INT_STAY_HOME,
                 INT_WASH_HANDS,
                 INT_CANCEL_VAC,
                 INT_NEIGHBOR_SHOP,
                 INT_NOT_HOARD,
                 INT_NEIGHBOR_CHILD,
                 INT_SUPPORT_COMP,
                 COND,
                 CONDlabel,
                 d[,c(145,147:157)]
)

nrow(dnew)

summary(dnew$duration)
summary(dnew$move.to.middle)

hist(dnew$duration[dnew$duration<1000])

#We will exclude those who took so little time to finish. They probably did not pay enough attention
dnew2<-dnew[dnew$duration>240,]
nrow(dnew2)

summary(dnew2$move.to.middle)

d<-dnew2

library(psych)

intentions<-d[,substr(names(d),1,3)=="INT"]
names(intentions)

#2 factor structure suggested
fa.parallel(intentions)

#We allow for oblimin rotation
INT_fact<-fa(intentions,2,rotate="oblimin",scores="tenBerge")

#Summary
INT_fact

#Quick visualization
diagram(INT_fact)

d$Precaution<-INT_fact$scores[,1]
d$Prosociality<-INT_fact$scores[,2]

#This is just for the pourpose of code checking, we sample the country randomly
set.seed(1234)
d$Country<-sample(c("USA","India"),nrow(d),replace=T)
d$CountC<-as.numeric(as.integer(as.factor(d$Country))-1.5) #country contrast

#write the data for futher use in structural models
write.table(d,"data.clean.txt",sep="\t",row.names = F)


#Check the correlaton between MAC dimensions
MAC<-d[,substr(names(d),1,3)=="MAC"]
cor(MAC)

