install.packages("readxl")
library(readxl)
library(forecast)
library(ggplot2)
library(dplyr)
mydata<-read_excel("C:/R/R/시계열분석 data/steamplayeruser.xlsx",
                   col_names=T,col_types="guess",na="NA")

################## 1. 데이터 전처리 ##################
mydata$Game_Name<-as.factor(mydata$Game_Name)
# 결측치 대체 #
mydata$Gain<-ifelse(is.na(mydata$Gain),0.355,mydata$Gain)
mydata$Percent_Gain<-ifelse(is.na(mydata$Percent_Gain),0.0012,mydata$Percent_Gain)

# 게임의 종류가 너무 많아서 2012년부터 데이터가 존재하는 게임
# 3개를 뽑아서 3개의 게임에 대한 시계열 분석을 하겠다.Globalstrike, Dota2, TeamFortress
GlobalStrike<-subset(mydata, subset=(mydata$Game_Name=='Counter Strike: Global Offensive'))
Dota2<-subset(mydata,subset=(mydata$Game_Name=='Dota 2'))
TeamFortress<-subset(mydata,subset=(mydata$Game_Name=='Team Fortress 2'))

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
################ 2. 시계열 분석 시작 ###############
ts_new<-ts(final,start=c(2012,7,1),frequency=12) #월 단위를 만들어준다
plot(ts_new,ylab="avg_players",main="평균게임유저수동향")
# 세데이터 모두 계절성은 없어 보인다.
# 먼저 자기상관관계를 확인한다.
acf(ts_new) # 세 데이터 모두 자기상관관계가 매우 높게 나온다.
pacf(ts_new) # pacf는 상대적으로 안정되어 있다.

# 변수변환을 해준다.(변동성 안정화)
ts_new[,1]<-log(as.vector(ts_new[,1]))
ts_new[,2]<-log(as.vector(ts_new[,2]))
ts_new[,3]<-log(as.vector(ts_new[,3]))
acf(ts_new) # acf 값이 너무 큼--> 자기상관 존재
pacf(ts_new)


# 이제 arima fitting을 하기 위해서 수동적으로 p,q를 찾는 방법과 auto.arima를 통해 자동으로 찾는 방법을 비교해보겠다.
# 먼저 auto.arima() fitting
# GobalStrike에 대해서 먼저 fitting
auto_GlobalStrike<-auto.arima(ts_new[,1]) # arima(2,2,2) 가 나온다.
summary(auto_GlobalStrike)
confint(auto_GlobalStrike) 
par(mar=c(2.1,2.1,2.6,2.1))
tsdiag(auto_GlobalStrike)# acf그래프도 만족스럽지 않고, 융박스검정을 통해서 자기상관이 있다는 것을 확인했다.모델이 적합하지 않다는 결론--> 수정할 필요가 있어보인다.

# 먼저, 차분을 수행하고 acf와 pacf를 그려본다.
autocorrelation<-acf(diff(ts_new[,1]),plot=FALSE)
plot(autocorrelation,main="d=1한 GlobalStrike의 ACF")
partialcorrelation<-pacf(diff(ts_new[,1]),plot=FALSE) # 한번만 차분해도 안정적으로 변했다. 자기상관이 존재하지 않는다.
plot(partialcorrelation,main="d=1한 GlobalStrike의 PACF")
fit<-Arima(ts_new[,1],order=c(0,1,0))
summary(fit)
tsdiag(fit) # 모형적합성이 좋아졌다. 이 모형을 최종모형으로 결정한다. 


# Dota2에 대해서 fitting
auto_Dota2 = auto.arima(ts_new[,2])
summary(auto_Dota2) # ARIMA(0,2,1)(1,0,2) 가 적합하다고 나왔다.
confint(auto_Dota2) # sma2에 대한 계수값은 유의하지 않다고 나온다.
tsdiag(auto_Dota2) # 모형의 적합도는 좋다고 나온다. 
# 이제 수동으로 모수를 찾아보겠다.
ts_new[,2] %>% diff(lag=12) %>% ggtsdisplay() # 계절성차분--> 여전히 비정상, 일반차분 시행
ts_new[,2] %>% diff(lag=12) %>% diff() %>% ggtsdisplay() # acf, pacf를 통해 시차 1에 해당하는 부분이 유의미하게 뾰족하므로 계절성ma(1) 성분을 암시한다는 것으로 판단. arima(0,1,1)(0,1,1)[12] 를 적합시켜보기로 함.
ts_new[,2] %>% Arima(order=c(0,1,1), seasonal=c(0,1,1)) %>% residuals() %>% ggtsdisplay() # 적합시키고 난 잔차도표와 acf,pacf 확인
fit2<-Arima(ts_new[,2],order=c(0,1,1), seasonal=c(0,1,1))
summary(fit2)
tsdiag(fit2)
confint(fit2) # 계수들이 모두 유의하게 나왔다. 최종모형을 arima(0,1,1)(0,1,1)[12]로 결정한다.

# TeamFortress에 대해서 fitting
auto_TeamFortress = auto.arima(ts_new[,3])
summary(auto_TeamFortress) # ARIMA(1,1,1)(1,0,0)[12] 모형판정
confint(auto_TeamFortress) # 계수값 유의하다.
tsdiag(auto_TeamFortress) # 모형적합결과도 좋다.
# 수동으로 모수를 찾아본다.
# 계절성이 크게 나타나지 않아서 일반차분을 한다.
ts_new[,3] %>% diff() %>% ggtsdisplay() # 정상성을 만족하는 것을 확인. 시차 1에서 한계선을 조금 넘어간것은 무시해도 좋을 듯 하다. --> arima(0,1,0) 을 먼저 적합시켜본다.
fit3<-Arima(ts_new[,3],order=c(0,1,0))
summary(fit3)
tsdiag(fit3) 
confint(fit3) # 상대적으로 arima(1,1,1)(1,0,0)[12]의 모형이 더 좋으므로 이 모형을 최종모형으로 결정한다.


################# 3. 예측과 평가 ###############
# GlobalStrike
plot(fit$x,lty=1,main="GlobalStrike의 관측값과 예측값",ylab="Average number of players",xlab="Date")
lines(fitted(fit),lty=2,lwd=2,col="red")
legend("topleft",legend=c("관측값","예측값"),col=c("black","red"),lty=c(1,2),box.lty=1,cex=1)

# Dota2
plot(fit2$x,lty=1,main="Dota2의 관측값과 예측값",ylab="Average number of players",xlab="Date")
lines(fitted(fit2),lty=2,lwd=2,col="red")
legend("topleft",legend=c("관측값","예측값"),col=c("black","red"),lty=c(1,2),box.lty=1,cex=1)

# TeamFortress
plot(auto_TeamFortress$x,lty=1,main="TeamFortress의 관측값과 예측값",ylab="Average number of players",xlab="Date")
lines(fitted(auto_TeamFortress),lty=2,lwd=2,col="red")
legend("topleft",legend=c("관측값","예측값"),col=c("black","red"),lty=c(1,2),box.lty=1,cex=1)


#모델의 정확도를 수치값으로 확인한다.
accuracy(fit)
accuracy(fit2)
accuracy(auto_TeamFortress)

# 이제 미래 20 시차를 예측해본다.
GlobalStrike_pred<-predict(fit,n.ahead=20)
exp(GlobalStrike_pred$pred) # 원본값으로 돌려놓는다.
fit %>% forecast(h=20) %>% autoplot() + ggtitle("GlobalStrike의 20시차 예측값")

Dota2_pred<-predict(fit2,n.ahead=20)
exp(Dota2_pred$pred)
fit2 %>% forecast(h=20) %>% autoplot() + ggtitle("Dota2의 20시차 예측값")

TeamFortress_pred<-predict(auto_TeamFortress,n.ahead=20)
exp(TeamFortress_pred$pred)
auto_TeamFortress %>% forecast(h=20) %>% autoplot() + ggtitle("TeamFortress의 20시차 예측값")


