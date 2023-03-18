


install.packages("RMySQL")
install.packages("dplyr")
install.packages("tidyr")
install.packages("stringr")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("googleVis")
install.packages("readxl")
library("readxl")
install.packages("forecast")
library("forecast")
library(ggplot2)
library(dplyr)
my_data <- read_excel("/Users/sunju/R/steamplayeruser.xlsx",col_names=T, col_types = "guess",na="NA")
################## 1. 데이터 전처리 ##################
my_data$Game_Name<-as.factor(my_data$Game_Name)

# 결측치 대체 #
my_data$Gain<-ifelse(is.na(my_data$Gain),0.355,my_data$Gain)
my_data$Percent_Gain<-ifelse(is.na(my_data$Percent_Gain),0.0012,my_data$Percent_Gain)

# 게임의 종류가 너무 많아서 2012년부터 데이터가 존재하는 게임
# 3개를 뽑아서 3개의 게임에 대한 시계열 분석을 진행했다.Globalstrike, Dota2, TeamFortress
GlobalStrike<-subset(my_data, subset=(my_data$Game_Name=='Counter Strike: Global Offensive'))
Dota2<-subset(my_data,subset=(my_data$Game_Name=='Dota 2'))
TeamFortress<-subset(my_data,subset=(my_data$Game_Name=='Team Fortress 2'))

# Month_Year을 정렬한다. (과거-->미래)
GlobalStrike<-GlobalStrike[order(as.Date(GlobalStrike$Month_Year,format="%Y-%m-%d")),]
Dota2<-Dota2[order(as.Date(Dota2$Month_Year,format="%Y-%m-%d")),]
TeamFortress<-TeamFortress[order(as.Date(TeamFortress$Month_Year,format="%Y-%m-%d")),]

#Avg_players만 예측하고자 나머지 열을 제거한다.
globalstrike<-subset(GlobalStrike,select= -c(Month_Year,Game_Name,Gain, Percent_Gain,Peak_Players))
dota2<-subset(Dota2, select = -c(Month_Year,Game_Name,Gain,Percent_Gain, Peak_Players))
teamfortress<-subset(TeamFortress, select = -c(Month_Year,Game_Name,Gain,Percent_Gain,Peak_Players))

# 세 데이터프레임을 합친다
final<-cbind(globalstrike,dota2,teamfortress)
#이름변환
colnames(final)<-c("GlobalStrike","Dota2","TeamFortress")
final<-as.matrix(final)


##################### 시계열 분석 시작 #####################
ts_new<-ts(final,start=c(2012,7),frequency=12) #월 단위를 만들어준다
ts_globalstrike <- ts(globalstrike, start=c(2012,7), frequency=12)
ts_dota2 <- ts(dota2, start=c(2012,7), frequency = 12)
ts_teamfortress <- ts(teamfortress, start=c(2012,7), frequency = 12)
theme_set(theme_gray(base_family='NanumGothic'))
autoplot(ts_new,ylab="avg_players",main="평균게임유저수동향") # teamfortress 를 제외하고 나머지 두 게임은 비정상시계열로 확인된다. 



# 데이터를 분해해본다.(승법모형 가정)--> 계절성이 있다면 추세는 남기고 계절성만 없앤다.
decompose(ts_globalstrike, type = 'multiplicative') %>% autoplot() # 평균과 분산 모두 비정상이다. 로그변환 후 계절성 차분한다. 
decompose(ts_dota2, type='multiplicative') %>% autoplot() # 평균과 분산 일정하지 않다. 변환과 계절성 차분 모두 진행한다. 
decompose(ts_teamfortress, type = 'multiplicative') %>% autoplot() # 평균과 분산 일정하지 않다. 변환과 계절성 차분 모두 진행한다. 



ts_globalstrike1<-diff(log(ts_globalstrike),lag=4) # 로그 변환,계절성 차분
ts_dota2_1<-diff(log(ts_dota2),lag=4) 
ts_teamfortress1 <- diff(log(ts_teamfortress),lag=4)

# adf.test 함수로 정상시계열인지 확인한다. 
library("tseries")
adf.test(ts_globalstrike1, alternative='stationary',k=0) # p-value 가 유의수준 0.05 하에서 0.01보다 작으므로 로그변환 및 차분한 데이터는 정상 시계열인걸 확인했다.
adf.test(ts_dota2_1, alternative = 'stationary', k=0) # 마찬가지로 정상시계열이다.
adf.test(ts_teamfortress1, alternative = 'stationary', k=0) # 정상시계열


##################### 정상 시계열 만들기 ####################
####### 1. GlobalStrike #######
# 정상성과 ACF, PACF 모두 확인하기 : ggtsdisplay()
ts_globalstrike1 %>% ggtsdisplay()  # acf 그래프를 통해 시차 3까지의 지수적 감소 그래프가 비계절성 ar(3) 성분을 암시. 
# globalstrike의 acf의 계절성 시차가 지수적으로 감소 -> 계절성 ar(1)

####### 2. Dota2 #######
ts_dota2_1 %>% ggtsdisplay() # 아직 차분의 여지가 보인다.
ts_dota2_1 %>% diff() %>% ggtsdisplay() # 두번 차분하니 정상시계열이 됐다. acf 그래프를 통해 시차 4의 뾰족한 막대가 비계절성 ma(4) 성분을 암시.

####### 3. TeamFortress #######
ts_teamfortress1 %>% ggtsdisplay() # 아직 차분의 여지가 보인다. 
ts_teamfortress1 %>% diff() %>% ggtsdisplay()


##################### 최적의 모델 찾기 #####################

###### 1. GlobalStrike ######
# 직접 설정한 arima(0,1,0) 의 결과와 비교해보자.
ts_globalstrike %>% log() %>% Arima(order=c(3,1,0),seasonal = c(1,1,0)) %>% residuals() %>% ggtsdisplay() # 수동으로 맞춘 모델의 잔차도 확인
# 시차 4에서까지 유의미하게 뾰족한 막대가 나타나기 때문에  arima(3,0,4)(1,1,0)[12]를 시도해보고 비교해보자
ts_globalstrike %>% log() %>% Arima(order=c(3,1,4),seasonal = c(1,1,0)) %>% residuals() %>% ggtsdisplay() # 더 나아지지 않는다.

# auto.arima의 결과 확인
auto.arima(ts_globalstrike1) # arima(0,1,0) , AICc = 51.36
# AICc 값으로 비교하기
# install.packages("wiqid")
library(wiqid)
ts_globalstrike %>% log() %>% Arima(order=c(3,1,0),seasonal = c(1,1,0)) %>% AICc() # AICc = 36.65
# 최종 모델은 arima(3,1,0)(1,1,0)[12] 모델이다. (GlobalStrike)
fit_globalstrike <- Arima(log(ts_globalstrike), order=c(3,1,0), seasonal = c(1,1,0))
checkresiduals(fit_globalstrike) # 잔차가 백색잡음인지 확인(by 융박스 검정), 유의수준 0.05 하에 유의확률 0.5 이상이므로 자기상관없음확인

###### 2. Dota2 ######
ts_dota2 %>% log() %>% Arima(order=c(0,1,4),seasonal = c(0,1,0)) %>% residuals() %>% ggtsdisplay() # ACF의 시차 12에서 유의미한 뾰족한 막대가 나타난다. 계절성 MA(1) 성분을 암시 
ts_dota2 %>% log() %>% Arima(order=c(0,1,4),seasonal = c(0,1,1)) %>% residuals() %>% ggtsdisplay() # 위의 결과보다 좋아졌다
# auto.arima의 결과 확인
auto.arima(ts_dota2_1)# ARIMA(0,1,0)(2,0,0)[12] , AICc=-165.58
# AICc 값으로 비교하기
ts_dota2 %>% log() %>% Arima(order=c(0,1,4), seasonal = c(0,1,1)) %>% AICc() # AICc = -195.2565
# 최종 모델은 arima(0,1,4)(0,1,1)[12] 모델이다. (Dota2)
fit_dota2 <- Arima(log(ts_dota2), order=c(0,1,4), seasonal = c(0,1,1)) 
checkresiduals(fit_dota2) # 융박스 검정 : 유의수준 0.05 하에 유의확률 0.4 이상, 자기상관없음 확인


###### 3. TeamFortress ######
ts_teamfortress %>% log() %>% Arima(order=c(1,1,0), seasonal = c(0,1,1)) %>% residuals() %>% ggtsdisplay()
# auto.arima의 결과 확인
auto.arima(ts_teamfortress1) # ARIMA(1,0,0)(0,0,1)[12], AICc = -185.38
ts_teamfortress %>% log() %>% Arima(order=c(1,1,0), seasonal = c(0,1,1)) %>% AICc() # AICc = -188.89
# 최종 모델은 arima(1,1,0)(0,1,1)[12] 모델이다. (TeamFortress)
fit_teamfortress <- Arima(log(ts_teamfortress), order=c(1,1,0), seasonal = c(0,1,1))
checkresiduals(fit_teamfortress) # 유의수준 0.05 하에 유의확률 0.9 이상. 자기상관 없음 확인


##################### 예측하기 #####################
# 미래 1년의 유저수를 예측해보자
fit_globalstrike %>% forecast(h=12) %>% autoplot() + ggtitle("ARIMA(3,1,0)(1,1,0)으로 얻은 GlobalStrike의 미래 1년 유저수 예측값") # 80% 와 95% 예측구간
fit_dota2 %>% forecast(h=12) %>% autoplot() + ggtitle("ARIMA(0,1,4)(0,1,1)으로 얻은 Dota2의 미래 1년 유저수 예측값")
fit_teamfortress %>% forecast(h=12) %>% autoplot() + ggtitle("ARIMA(1,1,0)(0,1,1)으로 얻은 TeamFortress의 미래 1년 유저수 예측값")
