library(ggplot2)
library(quantmod)
library(zoo)
library(FinTS)
library(timeDate)
library(timeSeries)
library(fGarch) 
library(data.table)
library(MSGARCH)

data = read.csv('D:\\研一课\\风险管理\\马尔可夫机制转换模型\\returns.csv',sep = ',',fileEncoding="UCS-2LE")

#数据
GLD <- data$GLD
#S&P = data$S.P500
NASDAQ <- data$NASDAQ
A <- data$上证指数
SZ <- data$深圳成指
GEM <- data$创业板指数
HK <- data$恒生指数
America <- data$美国债券



#发现在5阶滞后下全都拒绝不存在 ARCH 效应的假设，
#所以收益率序列存在 ARCH 效应，可以进行 GARCH 模型的拟合。

###################################################################
#GLD 模型的波动率拟合与估计

GLD_m1<-garchFit(~1+garch(1,1),data=GLD,trace=F) #拟合GARCH（1,1）模型
GLD_m2<-garchFit(~1+garch(1,2),data=GLD,trace=F) #拟合GARCH（1,2）模型
GLD_m3<-garchFit(~1+garch(2,1),data=GLD,trace=F) #拟合GARCH（2,1）模型
summary(GLD_m1)  
summary(GLD_m2)
summary(GLD_m3)
#发现三个模型的AIC都差不多，所以选择最简单的GARCH（1,1）作为拟合模型

vol_garch <-fBasics::volatility(GLD_m1)   
vol_garch.ts <- ts(vol_garch, frequency=255, start=c(2012, 1))
plot(vol_garch.ts, xlab='年', ylab='波动率',main = 'GLD GARCH(1,1)模型波动率估计')

garch_pred = predict(GLD_m1, n.ahead = 250, plot=TRUE)
print(garch_pred$meanForecast)

GLD_sample_sigma = (GLD-mean(GLD))^2
mcmc_estimate_volitity = sum(sqrt(vol_mcmc) - sqrt(GLD_sample_sigma))^2/1257
mcmc_estimate_volitity




##################################

#markove switching

spec <- CreateSpec(variance.spec = list(model = c("sGARCH","gjrGARCH")),
                   distribution.spec = list(distribution = c("std","std")),
                   switch.spec = list(do.mix = TRUE))
set.seed(12345678)
fit_GLD <- FitML(spec = spec, data = GLD)

GLD_vol_ml <- Volatility(object = fit_GLD)
plot.ts(vol_ml)#由马尔可夫机制转换GARCH模型拟合的波动率
plot.ts(GLD_sample_sigma)#GLD样本2011年~2016年波动率

#预测未来2017年的GLD波动率
GLD_pred <- predict(object = fit_GLD, nahead = 250L, do.return.draw = F)
plot(pred, xlab='date', ylab='波动率',main = 'GLD 马尔可夫转换GARCH(1,1)模型波动率估计')
sum(pred$vol)
max(pred$vol)
min(pred$vol)
print('预测的2017年GLD波动率为：')
print(mean(pred$vol))


GLD_statis <- data.frame(GLD_fit = c(sum(GLD_vol_ml),mean(GLD_vol_ml),max(GLD_vol_ml),min(GLD_vol_ml),median(GLD_vol_ml)),
                        GLD_pred = c(sum(GLD_pred$vol),mean(GLD_pred$vol),max(GLD_pred$vol),min(pred$vol),median(GLD_pred$vol)), 
                         GLD_sample = c(sum(GLD_sample_sigma),mean(GLD_sample_sigma),max(GLD_sample_sigma),min(GLD_sample_sigma),median(GLD_sample_sigma)))

#############################################################################
#S&P500 模型的波动率拟合与估计

m_SP1<-garchFit(~1+garch(1,1),data=data$S.P500,trace=F) #拟合GARCH（1,1）模型
m_SP2<-garchFit(~1+garch(1,2),data=data$S.P500,trace=F) #拟合GARCH（1,2）模型
m_SP3<-garchFit(~1+garch(2,1),data=data$S.P500,trace=F) #拟合GARCH（2,1）模型
summary(m_SP1)  
summary(m_SP2)
summary(m_SP3)
##发现三个模型的AIC都差不多，所以选择最简单的GARCH（1,1）作为拟合模型
vol_garch <-fBasics::volatility(m_SP1)   
vol_garch.ts <- ts(vol_garch, frequency=255, start=c(2012, 1))
plot(vol_garch.ts, xlab='年', ylab='波动率',main = 'S&P500 GARCH(1,1)模型波动率估计')

#################################################################
#S&P500 markov switching garch model estimate


spec <- CreateSpec(variance.spec = list(model = c("sGARCH","gjrGARCH")),
                   distribution.spec = list(distribution = c("std","std")),
                   switch.spec = list(do.mix = TRUE))
set.seed(12345678)
fit_SP <- FitML(spec = spec, data = data$S.P500)

sp_vol_ml <- Volatility(object = fit_SP)
plot.ts(vol_ml)#由马尔可夫机制转换GARCH模型拟合的波动率
plot.ts(SP500_sample_sigma)#样本2011年~2016年波动率


SP_sample_sigma = (data$S.P500-mean(data$S.P500))^2
mcmc_estimate_volitity = sum(sqrt(sp_vol_ml) - sqrt(SP500_sample_sigma))^2/1257
mcmc_estimate_volitity




#预测未来2017年的S&P500波动率
SP_pred <- predict(object = fit_SP, nahead = 250L, do.return.draw = F)
plot(SP_pred, xlab='date', ylab='波动率',main = '马尔可夫转换GARCH(1,1)模型波动率估计')
sum(SP_pred$vol)
max(SP_pred$vol)
min(SP_pred$vol)
print('预测的2017年S&P500波动率为：')
print(mean(SP_pred$vol))

SP500_statis <- data.frame(SP_fit = c(sum(sp_vol_ml),mean(sp_vol_ml),max(sp_vol_ml),min(sp_vol_ml),median(sp_vol_ml)),
                        SP_pred = c(sum(SP_pred$vol),mean(SP_pred$vol),max(SP_pred$vol),min(SP_pred$vol),median(SP_pred$vol)), 
                         SP_sample = c(sum(SP_sample_sigma),mean(SP_sample_sigma),max(SP_sample_sigma),min(SP_sample_sigma),median(SP_sample_sigma)))

#####################################################
#NASDAQ估计波动率

# markov switching garch model estimate


spec <- CreateSpec(variance.spec = list(model = c("sGARCH","gjrGARCH")),
                   distribution.spec = list(distribution = c("std","std")),
                   switch.spec = list(do.mix = TRUE))
set.seed(12345678)
fit_NAS <- FitML(spec = spec, data = NASDAQ)

NAS_vol_ml <- Volatility(object = fit_NAS)
plot.ts(vol_ml)#由马尔可夫机制转换GARCH模型拟合的波动率

NASDAQ_sample_sigma = (data$NASDAQ-mean(data$NASDAQ))^2

plot.ts(NASDAQ_sample_sigma)#样本2011年~2016年波动率

#估计的模型预测波动率与样本波动率的MSE误差
mcmc_estimate_volitity = sum(sqrt(NASDAQ_vol_ml) - sqrt(NASDAQ_sample_sigma))^2/1257
mcmc_estimate_volitity



#预测未来2017年的NASDAQ波动率
NASDAQ_pred <- predict(object = fit_NAS, nahead = 250L, do.return.draw = F)
plot(NASDAQ_pred, xlab='date', ylab='波动率',main = '马尔可夫转换GARCH(1,1)模型波动率估计')
sum(NASDAQ_pred$vol)
max(NASDAQ_pred$vol)
min(NASDAQ_pred$vol)
print('预测的2017年NASDAQ波动率为：')
print(mean(NASDAQ_pred$vol))



NASDAQ_statis <- data.frame(NASDAQ_fit = c(sum(NAS_vol_ml),mean(NAS_vol_ml),max(NAS_vol_ml),min(NAS_vol_ml),median(NAS_vol_ml)),
                           NASDAQ_pred = c(sum(NASDAQ_pred$vol),mean(NASDAQ_pred$vol),max(NASDAQ_pred$vol),min(NASDAQ_pred$vol),median(NASDAQ_pred$vol)), 
                           NASDAQ_sample = c(sum(NASDAQ_sample_sigma),mean(NASDAQ_sample_sigma),max(NASDAQ_sample_sigma),min(NASDAQ_sample_sigma),median(NASDAQ_sample_sigma)))


#############################################################################
#A股 markov switching garch model estimate


spec <- CreateSpec(variance.spec = list(model = c("sGARCH","gjrGARCH")),
                   distribution.spec = list(distribution = c("std","std")),
                   switch.spec = list(do.mix = TRUE))
set.seed(12345678)
fit_A <- FitML(spec = spec, data = A)

A_vol_ml <- Volatility(object = fit_A)
plot.ts(vol_ml)#由马尔可夫机制转换GARCH模型拟合的波动率

A_sample_sigma = (A-mean(A))^2

plot.ts(A_sample_sigma)#样本2011年~2016年波动率

#估计的模型预测波动率与样本波动率的MSE误差
mcmc_estimate_volitity = sum(sqrt(A_vol_ml) - sqrt(A_sample_sigma))^2/1257
mcmc_estimate_volitity


#预测未来2017年的A股波动率
A_pred <- predict(object = fit_A, nahead = 250L, do.return.draw = F)
plot(A_pred, xlab='date', ylab='波动率',main = '马尔可夫转换GARCH(1,1)模型波动率估计')
sum(A_pred$vol)
max(A_pred$vol)
min(A_pred$vol)
print('预测的2017年A股波动率为：')
print(mean(A_pred$vol))




A_statis <- data.frame(A_fit = c(sum(A_vol_ml),mean(A_vol_ml),max(A_vol_ml),min(A_vol_ml),median(A_vol_ml)),
                      A_pred = c(sum(A_pred$vol),mean(A_pred$vol),max(A_pred$vol),min(A_pred$vol),median(A_pred$vol)), 
                      A_sample = c(sum(A_sample_sigma),mean(A_sample_sigma),max(A_sample_sigma),min(A_sample_sigma),median(A_sample_sigma)))


################################################################################
#深股 markov switching garch model estimate


spec <- CreateSpec(variance.spec = list(model = c("sGARCH","gjrGARCH")),
                   distribution.spec = list(distribution = c("std","std")),
                   switch.spec = list(do.mix = TRUE))
set.seed(12345678)
fit_SZ <- FitML(spec = spec, data = SZ)

sz_vol_ml <- Volatility(object = fit_SZ)
plot.ts(vol_ml)#由马尔可夫机制转换GARCH模型拟合的波动率

sz_sample_sigma = (SZ-mean(SZ))^2

plot.ts(sz_sample_sigma)#样本2011年~2016年波动率

#估计的模型预测波动率与样本波动率的MSE误差
mcmc_estimate_volitity = sum(sqrt(sz_vol_ml) - sqrt(sz_sample_sigma))^2/1257
mcmc_estimate_volitity


#预测未来2017年的深股波动率
sz_pred <- predict(object = fit_SZ, nahead = 250L, do.return.draw = F)
plot(sz_pred, xlab='date', ylab='波动率',main = '马尔可夫转换GARCH(1,1)模型波动率估计')
sum(sz_pred$vol)
max(sz_pred$vol)
min(sz_pred$vol)
print('预测的2017年深股波动率为：')
print(mean(sz_pred$vol))




sz_statis <- data.frame(SZ_fit = c(sum(sz_vol_ml),mean(sz_vol_ml),max(sz_vol_ml),min(sz_vol_ml),median(GLD_vol_ml)),
                        SZ_pred = c(sum(sz_pred$vol),mean(sz_pred$vol),max(sz_pred$vol),min(sz_pred$vol),median(sz_pred$vol)), 
                       SZ_sample = c(sum(sz_sample_sigma),mean(sz_sample_sigma),max(sz_sample_sigma),min(sz_sample_sigma),median(sz_sample_sigma)))


##############################################################
#创业板股 markov switching garch model estimate


spec <- CreateSpec(variance.spec = list(model = c("sGARCH","gjrGARCH")),
                   distribution.spec = list(distribution = c("std","std")),
                   switch.spec = list(do.mix = TRUE))
set.seed(12345678)
fit_GEM <- FitML(spec = spec, data = GEM)

GEM_vol_ml <- Volatility(object = fit_GEM)
plot.ts(vol_ml)#由马尔可夫机制转换GARCH模型拟合的波动率

GEM_sample_sigma = (GEM-mean(GEM))^2

plot.ts(GEM_sample_sigma)#样本2011年~2016年波动率

#估计的模型预测波动率与样本波动率的MSE误差
mcmc_estimate_volitity = sum(sqrt(GEM_vol_ml) - sqrt(GEM_sample_sigma))^2/1257
mcmc_estimate_volitity


#预测未来2017年的深股波动率
GEM_pred <- predict(object = fit_GEM, nahead = 250L, do.return.draw = F)
plot(GEM_pred, xlab='date', ylab='波动率',main = '马尔可夫转换GARCH(1,1)模型波动率估计')
sum(GEM_pred$vol)
max(GEM_pred$vol)
min(GEM_pred$vol)
print('预测的2017年深股波动率为：')
print(mean(GEM_pred$vol))





GEM_statis <- data.frame(GEM_fit = c(sum(GEM_vol_ml),mean(GEM_vol_ml),max(GEM_vol_ml),min(GEM_vol_ml),median(GEM_vol_ml)),
                        GEM_pred = c(sum(GEM_pred$vol),mean(GEM_pred$vol),max(GEM_pred$vol),min(GEM_pred$vol),median(GEM_pred$vol)), 
                       GEM_sample = c(sum(GEM_sample_sigma),mean(GEM_sample_sigma),max(GEM_sample_sigma),min(GEM_sample_sigma),median(GEM_sample_sigma)))


##################################################################
#恒生 markov switching garch model estimate


spec <- CreateSpec(variance.spec = list(model = c("sGARCH","gjrGARCH")),
                   distribution.spec = list(distribution = c("std","std")),
                   switch.spec = list(do.mix = TRUE))
set.seed(12345678)
fit_HK <- FitML(spec = spec, data = HK)

HK_vol_ml <- Volatility(object = fit_HK)
plot.ts(vol_ml)#由马尔可夫机制转换GARCH模型拟合的波动率

HK_sample_sigma = (HK-mean(HK))^2

plot.ts(HK_sample_sigma)#样本2011年~2016年波动率

#估计的模型预测波动率与样本波动率的MSE误差
mcmc_estimate_volitity = sum(sqrt(HK_vol_ml) - sqrt(HK_sample_sigma))^2/1257
mcmc_estimate_volitity


#预测未来2017年的港股波动率
HK_pred <- predict(object = fit_HK, nahead = 250L, do.return.draw = F)
plot(HK_pred, xlab='date', ylab='波动率',main = '马尔可夫转换GARCH(1,1)模型波动率估计')
sum(HK_pred$vol)
max(HK_pred$vol)
min(HK_pred$vol)
print('预测的2017年港股波动率为：')
print(mean(HK_pred$vol))




HK_statis <- data.frame(HK_fit = c(sum(HK_vol_ml),mean(HK_vol_ml),max(HK_vol_ml),min(HK_vol_ml),median(HK_vol_ml)),
                        HK_pred = c(sum(HK_pred$vol),mean(HK_pred$vol),max(HK_pred$vol),min(HK_pred$vol),median(HK_pred$vol)), 
                         HK_sample = c(sum(HK_sample_sigma),mean(HK_sample_sigma),max(HK_sample_sigma),min(HK_sample_sigma),median(HK_sample_sigma)))

#########################################################################


spec <- CreateSpec(variance.spec = list(model = c("sGARCH","gjrGARCH")),
                   distribution.spec = list(distribution = c("std","std")),
                   switch.spec = list(do.mix = TRUE))
set.seed(12345678)
fit_America <- FitML(spec = spec, data = America)

America_vol_ml <- Volatility(object = fit_America)
plot.ts(vol_ml)#由马尔可夫机制转换GARCH模型拟合的波动率

America_sample_sigma = (America-mean(America))^2

plot.ts(America_sample_sigma)#样本2011年~2016年波动率

#估计的模型预测波动率与样本波动率的MSE误差
mcmc_estimate_volitity = sum(sqrt(America_vol_ml) - sqrt(America_sample_sigma))^2/1257
mcmc_estimate_volitity


#预测未来2017年的美国债券波动率
America_pred <- predict(object = fit_America, nahead = 250L, do.return.draw = F)
plot(America_pred, xlab='date', ylab='波动率',main = '马尔可夫转换GARCH(1,1)模型波动率估计')
sum(America_pred$vol)
max(America_pred$vol)
min(America_pred$vol)
print('预测的2017年美国债券波动率为：')
print(mean(America_pred$vol))




America_statis <- data.frame(America_fit = c(sum(America_vol_ml),mean(America_vol_ml),max(America_vol_ml),min(America_vol_ml),median(America_vol_ml)),
                             America_pred = c(sum(America_pred$vol),mean(America_pred$vol),max(America_pred$vol),min(America_pred$vol),median(America_pred$vol)), 
                             America_sample = c(sum(America_sample_sigma),mean(America_sample_sigma),max(America_sample_sigma),min(America_sample_sigma),median(America_sample_sigma)))

output = cbind(GLD_statis,SP500_statis,NASDAQ_statis,A_statis,sz_statis,GEM_statis,HK_statis,America_statis)
index_name = c('sum','mean','max','min','median')

output2 = cbind(index_name,output)

write.csv(output2,'D:\\研一课\\风险管理\\马尔可夫机制转换模型\\pred_volatility.csv')#,index = )
