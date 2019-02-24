install.packages("foreign")
library(foreign)
library(dplyr)
library(ggplot2)
library(readxl)


raw_welfare<-read.spss(file="D:/may/day04/Koweps_hpc10_2015_beta1 (1).sav",to.data.frame = T)

welfare<-raw_welfare
head(welfare)
View(welfare)
summary(welfare)

welfare <- rename(welfare, 
                  gender = h10_g3,
                  birth = h10_g4,
                  marriage = h10_g10,
                  religion = h10_g11,
                  income = p1002_8aq1,
                  code_job=h10_eco9,
                  code_region=h10_reg7
)
head(welfare$gender,10)

count(welfare,gender)

welfare$gender<-ifelse(welfare$gender==9,NA,welfare$gender)

table(is.na(welfare$gender))

welfare$gender<-ifelse(welfare$gender==1,"남","여")
welfare$gender
table(welfare$gender)

qplot(welfare$gender)


class(welfare$income)
summary(welfare$income)
qplot(welfare$income)+xlim(0,1000)

table(is.na(welfare$income))
welfare$income<-ifelse(welfare$income %in% c(0,9999),NA,welfare$income)
welfare$income
table(is.na(welfare$income))


#<성별 월급의 비교> 
#1.na빼고 분석 
#2.성별 그룹(dplyr)
#3.평균 

gender_income<-welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(gender) %>% 
  summarise(mean_income=mean(income))

head(gender_income)

ggplot(data=gender_income,aes(x=gender,y=mean_income))+geom_col()


qplot(welfare$birth)

welfare$age<-2015-welfare$birth+1
summary(welfare$age)
qplot(welfare$age)

#나이별 평균 월급 

age_income<-welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age) %>% 
  summarise(mean_income2=mean(income))


ggplot(data=age_income,aes(x=age,y=mean_income2))+geom_line()

age<-welfare %>% 
  mutate(age2=ifelse(age<30,"young",
                     ifelse(age<=59,"middle","old")))

class(welfare$age2)
table(welfare$age2)

qplot(welfare$age2)

ggplot(data=age_income2,aes(x=age2,y=mean_income2))+geom_col()+scale_x_discrete(limits=c("young","middle","old"))
#순서대로 바꾸기 

#연령대 성별 평균 수입 
age3_income<-welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age2,gender) %>% 
  summarise(mean_income4=mean(income))
age3_income

#나이+성별 평균 월급의 흐름을 알고싶다.
age4_income<-welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age,gender) %>% 
  summarise(mean_income5=mean(income))


#분석후 시각화. 시계열 
ggplot(data=age4_income,aes(x=age,y=mean_income5,col=sex))+geom_line()
#시계열인 경우 col= 사용 


#####################################

library(readxl)
list_job <-read.excel("D:/may/day04/Koweps_Codebook.xlsx",col_names=T,sheet=2)
head(list_job)

welfare<-left_join(welfare,list_job,id="code_job")

welfare %>% 
  filter(!is.na(code_job)) %>% 
  select(code_job,job) %>% 
  head(10)


