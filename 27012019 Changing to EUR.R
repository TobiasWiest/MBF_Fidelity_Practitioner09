library(tidyverse)
library(dplyr)
library(readxl)
library(reshape2)
library(texreg)
library(lme4)
library(broom)
library(zoo)

Full_Equity_Sample <- read_excel("Full Equity Sample.xlsx")


Full_Equity_Sample$FundID <- seq.int(nrow(Full_Equity_Sample))

Full_Equity_Static  <- Full_Equity_Sample %>% 
  select(-c(grep("Fund Size", names(Full_Equity_Sample)),
         grep("Equity Style", names(Full_Equity_Sample)),
         grep("Monthly Return", names(Full_Equity_Sample))))

FundSizeWide <- Full_Equity_Sample %>% 
  select(FundID,
         grep("Fund Size", names(Full_Equity_Sample)))

EquityStyleWide <- Full_Equity_Sample %>% 
  select(FundID,
         grep("Equity Style", names(Full_Equity_Sample)))

MonthlyReturnWide <- Full_Equity_Sample %>% 
  select(FundID,
         grep("Monthly Return", names(Full_Equity_Sample)))


FundSizeLong <- melt(FundSizeWide,
                  id.vars="FundID",
                  measure.vars=c(grep("Fund Size", names(FundSizeWide))),
                  variable.name="date",
                  value.name="FundSize")

EquityStyleLong <- melt(EquityStyleWide,
                     id.vars="FundID",
                     measure.vars=c(grep("Equity Style", names(EquityStyleWide))),
                     variable.name="date",
                     value.name="EquityStyle")

MonthlyReturnLong <- melt(MonthlyReturnWide,
                        id.vars="FundID",
                        measure.vars=c(grep("Monthly Return", names(MonthlyReturnWide))),
                        variable.name="date",
                        value.name="MonthlyReturn")

MonthlyReturnLong <- MonthlyReturnLong %>% 
                    mutate(date = gsub("[^[:digit:]-]", "",  date))

EquityStyleLong <- EquityStyleLong %>% 
                    mutate(date = gsub("[^[:digit:]-]", "",  date))

FundSizeLong <- FundSizeLong %>% 
                    mutate(date = gsub("[^[:digit:]-]", "",  date)) %>% 
                    mutate(date = substring(date, 2, 8))

Full_Equity_Dynamic <- EquityStyleLong %>% 
              left_join(FundSizeLong, by = c("FundID", "date")) %>% 
              left_join(MonthlyReturnLong, by = c("FundID", "date"))

Full_Equity_Panel <- Full_Equity_Dynamic %>% 
              left_join(Full_Equity_Static, by = "FundID") %>% 
              arrange(FundID, date) %>% 
                filter(!
                (is.na(FundSize) == 1 &
                is.na(EquityStyle) == 1 |
                is.na(MonthlyReturn) == 1 
                ))

             
Full_Equity_Panel <- Full_Equity_Panel %>%
  mutate(date1 = paste(as.character(date), "-28", sep = "")) %>% 
  mutate(date1 = as.Date(date1)) %>% 
  mutate(year = as.numeric(format(date1,'%Y'))) %>% 
  mutate(month = as.numeric(format(date1, '%m')))


China_Panel <- Full_Equity_Panel %>% 
    filter(`Investment Area`  == "China") %>% 
    arrange(year, month)

India_Panel <- Full_Equity_Panel %>% 
  filter(`Investment Area`  == "India") %>% 
  arrange(year, month)

AsiaPacific_Panel <- Full_Equity_Panel %>% 
  filter(`Investment Area`  == "Asia Pacific ex Japan" | `Investment Area`  == "Asia Pacific ex Japan ex Australia") %>% 
  arrange(year, month)

AsiaEmerg_Panel <- Full_Equity_Panel %>% 
  filter(`Investment Area`  == "Asia Emerging Mkts") %>% 
  arrange(year, month)

Mid_Panel <- Full_Equity_Panel[grepl("Mid", Full_Equity_Panel$EquityStyle),]
Small_Panel <- Full_Equity_Panel[grepl("Small", Full_Equity_Panel$EquityStyle),]


Small_Panel %>% 
  filter(month == 12) %>% 
  group_by(date1, `Investment Area`) %>% 
  count() %>% 
  ggplot(aes(x= date1, y= n, color= `Investment Area`)) + 
  geom_line(size=1) 

Mid_Panel %>% 
  filter(month == 12) %>% 
  group_by(date1, `Investment Area`) %>% 
  count() %>% 
  ggplot(aes(x= date1, y= n, color= `Investment Area`)) + 
  geom_line(size=1) 

China_Mid_Panel <- China_Panel[grepl("Mid", Full_Equity_Panel$EquityStyle),]
China_Small_Panel <- China_Panel[grepl("Small", Full_Equity_Panel$EquityStyle),]

India_Mid_Panel <- India_Panel[grepl("Mid", Full_Equity_Panel$EquityStyle),]
India_Small_Panel <- India_Panel[grepl("Small", Full_Equity_Panel$EquityStyle),]

AsiaEmerg_Mid_Panel <- AsiaEmerg_Panel[grepl("Mid", Full_Equity_Panel$EquityStyle),]
AsiaEmerg_Small_Panel <- AsiaEmerg_Panel[grepl("Small", Full_Equity_Panel$EquityStyle),]

AsiaPacific_Mid_Panel <- AsiaPacific_Panel[grepl("Mid", Full_Equity_Panel$EquityStyle),]
AsiaPacific_Small_Panel <- AsiaPacific_Panel[grepl("Small", Full_Equity_Panel$EquityStyle),]


# Factors Emerging Markets from Fama French Website
Emerging_5Factors <- read.csv("Emerging_5_Factors.csv") 
Emerging_Momentum <- read.csv("Emerging_MOM_Factor.csv") 
Emerging_Factors <- Emerging_5Factors %>% 
      left_join(Emerging_Momentum, by = "Date")

Emerging_Factors <- Emerging_Factors %>% 
  mutate(date1 = paste(substring(as.character(Date), 1, 4), substring(as.character(Date),5,6), "28", sep = "-")) %>% 
  mutate(date1 = as.Date(date1)) %>% 
  filter(date1 > "2010-01-01")

ReturnUSD <- read_excel("ReturnUSD.xlsx")

ReturnUSD <- ReturnUSD %>% 
  mutate(date1 = as.Date(date1))

Emerging_Factors <- Emerging_Factors %>% 
  left_join(ReturnUSD, by = "date1") 


Emerging_Factors <- Emerging_Factors %>% 
  mutate(Mkt.RF = ((((Mkt.RF/100)+1)/((ReturnUSD/100)+1))-1)*100) %>% 
  mutate(SMB = ((((SMB/100)+1)/((ReturnUSD/100)+1)-1))*100) %>% 
  mutate(HML = ((((HML/100)+1)/((ReturnUSD/100)+1)-1))*100) %>% 
  mutate(CMA = ((((CMA/100)+1)/((ReturnUSD/100)+1)-1))*100) %>% 
  mutate(RF = ((((RF/100)+1)/((ReturnUSD/100)+1)-1))*100) %>% 
  mutate(WML = ((((WML/100)+1)/((ReturnUSD/100)+1)-1))*100) 

# Benchmark Returns from Morningstar
Benchmarks <- read_excel("Benchmark Returns.xlsx")
Benchmarks <- Benchmarks %>% 
  mutate(date1 = as.Date(Date))

Benchmarks_Factors <- Emerging_Factors %>% 
    left_join(Benchmarks, by = "date1") 

##### Pooled OLS MultiFactor Regressions 

#### China 

China_Panel <- China_Panel %>% 
  left_join(Benchmarks_Factors, by = "date1") 


China_Panel <- China_Panel %>% 
  mutate(Funds.RF = MonthlyReturn - RF) %>% 
  mutate(MSCIChinaNRUSD.RF = MSCIChinaNRUSD - RF) %>% 
  mutate(CSI300NRUSD.RF = CSI300NRUSD - RF) %>% 
  mutate(CSIStateownedEnterprisesCompPRCNY.RF = CSIStateownedEnterprisesCompPRCNY - RF)

model_china_1factor_10y_FF <- lm(Funds.RF ~ Mkt.RF, data = China_Panel)
model_china_3factor_10y_FF <- lm(Funds.RF ~ Mkt.RF + SMB + HML, data = China_Panel)
model_china_4factor_10y_FF <- lm(Funds.RF ~ Mkt.RF + SMB + HML + WML, data = China_Panel)
model_china_5factor_10y_FF <- lm(Funds.RF ~ Mkt.RF + SMB + HML + RMW + CMA, data = China_Panel)

model_china_1factor_5y_FF <- lm(Funds.RF ~ Mkt.RF, data = subset(China_Panel, date1 > as.Date("2015-01-01")))
model_china_3factor_5y_FF <- lm(Funds.RF ~ Mkt.RF + SMB + HML, data = subset(China_Panel, date1 > as.Date("2015-01-01")))
model_china_4factor_5y_FF <- lm(Funds.RF ~ Mkt.RF + SMB + HML + WML, data = subset(China_Panel, date1 > as.Date("2015-01-01")))
model_china_5factor_5y_FF <- lm(Funds.RF ~ Mkt.RF + SMB + HML + RMW + CMA, data = subset(China_Panel, date1 > as.Date("2015-01-01")))

model_china_1factor_3y_FF <- lm(Funds.RF ~ Mkt.RF, data = subset(China_Panel, date1 > as.Date("2017-01-01")))
model_china_3factor_3y_FF <- lm(Funds.RF ~ Mkt.RF + SMB + HML, data = subset(China_Panel, date1 > as.Date("2017-01-01")))
model_china_4factor_3y_FF <- lm(Funds.RF ~ Mkt.RF + SMB + HML + WML, data = subset(China_Panel, date1 > as.Date("2017-01-01")))
model_china_5factor_3y_FF <- lm(Funds.RF ~ Mkt.RF + SMB + HML + RMW + CMA, data = subset(China_Panel, date1 > as.Date("2017-01-01")))

model_china_1factor_10y_CSI300 <- lm(Funds.RF ~ CSI300NRUSD.RF, data = China_Panel)
model_china_3factor_10y_CSI300 <- lm(Funds.RF ~ CSI300NRUSD.RF + SMB + HML, data = China_Panel)
model_china_4factor_10y_CSI300 <- lm(Funds.RF ~ CSI300NRUSD.RF + SMB + HML + WML, data = China_Panel)
model_china_5factor_10y_CSI300 <- lm(Funds.RF ~ CSI300NRUSD.RF + SMB + HML + RMW + CMA, data = China_Panel)

model_china_1factor_5y_CSI300 <- lm(Funds.RF ~ CSI300NRUSD.RF, data = subset(China_Panel, date1 > as.Date("2015-01-01")))
model_china_3factor_5y_CSI300 <- lm(Funds.RF ~ CSI300NRUSD.RF + SMB + HML, data = subset(China_Panel, date1 > as.Date("2015-01-01")))
model_china_4factor_5y_CSI300 <- lm(Funds.RF ~ CSI300NRUSD.RF + SMB + HML + WML, data = subset(China_Panel, date1 > as.Date("2015-01-01")))
model_china_5factor_5y_CSI300 <- lm(Funds.RF ~ CSI300NRUSD.RF + SMB + HML + RMW + CMA, data = subset(China_Panel, date1 > as.Date("2015-01-01")))

model_china_1factor_3y_CSI300 <- lm(Funds.RF ~ CSI300NRUSD.RF, data = subset(China_Panel, date1 > as.Date("2017-01-01")))
model_china_3factor_3y_CSI300 <- lm(Funds.RF ~ CSI300NRUSD.RF + SMB + HML, data = subset(China_Panel, date1 > as.Date("2017-01-01")))
model_china_4factor_3y_CSI300 <- lm(Funds.RF ~ CSI300NRUSD.RF + SMB + HML + WML, data = subset(China_Panel, date1 > as.Date("2017-01-01")))
model_china_5factor_3y_CSI300 <- lm(Funds.RF ~ CSI300NRUSD.RF + SMB + HML + RMW + CMA, data = subset(China_Panel, date1 > as.Date("2017-01-01")))

model_china_1factor_10y_MSCIChina <- lm(Funds.RF ~ MSCIChinaNRUSD.RF, data = China_Panel)
model_china_3factor_10y_MSCIChina <- lm(Funds.RF ~ MSCIChinaNRUSD.RF + SMB + HML, data = China_Panel)
model_china_4factor_10y_MSCIChina <- lm(Funds.RF ~ MSCIChinaNRUSD.RF + SMB + HML + WML, data = China_Panel)
model_china_5factor_10y_MSCIChina <- lm(Funds.RF ~ MSCIChinaNRUSD.RF + SMB + HML + RMW + CMA, data = China_Panel)

model_china_1factor_5y_MSCIChina <- lm(Funds.RF ~ MSCIChinaNRUSD.RF, data = subset(China_Panel, date1 > as.Date("2015-01-01")))
model_china_3factor_5y_MSCIChina <- lm(Funds.RF ~ MSCIChinaNRUSD.RF + SMB + HML, data = subset(China_Panel, date1 > as.Date("2015-01-01")))
model_china_4factor_5y_MSCIChina <- lm(Funds.RF ~ MSCIChinaNRUSD.RF + SMB + HML + WML, data = subset(China_Panel, date1 > as.Date("2015-01-01")))
model_china_5factor_5y_MSCIChina <- lm(Funds.RF ~ MSCIChinaNRUSD.RF + SMB + HML + RMW + CMA, data = subset(China_Panel, date1 > as.Date("2015-01-01")))

model_china_1factor_3y_MSCIChina <- lm(Funds.RF ~ MSCIChinaNRUSD.RF, data = subset(China_Panel, date1 > 2017-01-01))
model_china_3factor_3y_MSCIChina <- lm(Funds.RF ~ MSCIChinaNRUSD.RF + SMB + HML, data = subset(China_Panel, date1 > 2017-01-01))
model_china_4factor_3y_MSCIChina <- lm(Funds.RF ~ MSCIChinaNRUSD.RF + SMB + HML + WML, data = subset(China_Panel, date1 > 2017-01-01))
model_china_5factor_3y_MSCIChina <- lm(Funds.RF ~ MSCIChinaNRUSD.RF + SMB + HML + RMW + CMA, data = subset(China_Panel, date1 > 2017-01-01))

model_china_1factor_10y_CSISOE <- lm(Funds.RF ~ CSIStateownedEnterprisesCompPRCNY.RF, data = China_Panel)
model_china_3factor_10y_CSISOE <- lm(Funds.RF ~ CSIStateownedEnterprisesCompPRCNY.RF + SMB + HML, data = China_Panel)
model_china_4factor_10y_CSISOE <- lm(Funds.RF ~ CSIStateownedEnterprisesCompPRCNY.RF + SMB + HML + WML, data = China_Panel)
model_china_5factor_10y_CSISOE <- lm(Funds.RF ~ CSIStateownedEnterprisesCompPRCNY.RF + SMB + HML + RMW + CMA, data = China_Panel)

model_china_1factor_5y_CSISOE <- lm(Funds.RF ~ CSIStateownedEnterprisesCompPRCNY.RF, data = subset(China_Panel, date1 > 2015-01-01))
model_china_3factor_5y_CSISOE <- lm(Funds.RF ~ CSIStateownedEnterprisesCompPRCNY.RF + SMB + HML, data = subset(China_Panel, date1 > 2015-01-01))
model_china_4factor_5y_CSISOE <- lm(Funds.RF ~ CSIStateownedEnterprisesCompPRCNY.RF + SMB + HML + WML, data = subset(China_Panel, date1 > 2015-01-01))
model_china_5factor_5y_CSISOE <- lm(Funds.RF ~ CSIStateownedEnterprisesCompPRCNY.RF + SMB + HML + RMW + CMA, data = subset(China_Panel, date1 > 2015-01-01))

model_china_1factor_3y_CSISOE <- lm(Funds.RF ~ CSIStateownedEnterprisesCompPRCNY.RF, data = subset(China_Panel, date1 > as.Date("2017-01-01")))
model_china_3factor_3y_CSISOE <- lm(Funds.RF ~ CSIStateownedEnterprisesCompPRCNY.RF + SMB + HML, data = subset(China_Panel, date1 > as.Date("2017-01-01")))
model_china_4factor_3y_CSISOE <- lm(Funds.RF ~ CSIStateownedEnterprisesCompPRCNY.RF + SMB + HML + WML, data = subset(China_Panel, date1 > as.Date("2017-01-01")))
model_china_5factor_3y_CSISOE <- lm(Funds.RF ~ CSIStateownedEnterprisesCompPRCNY.RF + SMB + HML + RMW + CMA, data = subset(China_Panel, date1 > as.Date("2017-01-01")))


#### Asia Pacific ex Japan Factor Models

AsiaPacific_Panel <- AsiaPacific_Panel %>% 
  left_join(Benchmarks_Factors, by = "date1") 


AsiaPacific_Panel <- AsiaPacific_Panel %>% 
  mutate(Funds.RF = MonthlyReturn - RF) %>% 
  mutate(MSCIACAsiaPacExJPNNRUSD.RF = MSCIACAsiaPacExJPNNRUSD - RF) %>% 
  mutate(CSI300NRUSD.RF = CSI300NRUSD - RF) %>% 
  mutate(CSIStateownedEnterprisesCompPRCNY.RF = CSIStateownedEnterprisesCompPRCNY - RF)

model_asiapacexjpn_1factor_10y_FF <- lm(Funds.RF ~ Mkt.RF, data = AsiaPacific_Panel)
model_asiapacexjpn_3factor_10y_FF <- lm(Funds.RF ~ Mkt.RF + SMB + HML, data = AsiaPacific_Panel)
model_asiapacexjpn_4factor_10y_FF <- lm(Funds.RF ~ Mkt.RF + SMB + HML + WML, data = AsiaPacific_Panel)
model_asiapacexjpn_5factor_10y_FF <- lm(Funds.RF ~ Mkt.RF + SMB + HML + RMW + CMA, data = AsiaPacific_Panel)

model_asiapacexjpn_1factor_5y_FF <- lm(Funds.RF ~ Mkt.RF, data = subset(AsiaPacific_Panel, date1 > as.Date("2015-01-01")))
model_asiapacexjpn_3factor_5y_FF <- lm(Funds.RF ~ Mkt.RF + SMB + HML, data = subset(AsiaPacific_Panel, date1 > as.Date("2015-01-01")))
model_asiapacexjpn_4factor_5y_FF <- lm(Funds.RF ~ Mkt.RF + SMB + HML + WML, data = subset(AsiaPacific_Panel, date1 > as.Date("2015-01-01")))
model_asiapacexjpn_5factor_5y_FF <- lm(Funds.RF ~ Mkt.RF + SMB + HML + RMW + CMA, data =subset(AsiaPacific_Panel, date1 > as.Date("2015-01-01")))

model_asiapacexjpn_1factor_3y_FF <- lm(Funds.RF ~ Mkt.RF, data = subset(AsiaPacific_Panel, date1 > as.Date("2017-01-01")))
model_asiapacexjpn_3factor_3y_FF <- lm(Funds.RF ~ Mkt.RF + SMB + HML, data = subset(AsiaPacific_Panel, date1 > as.Date("2017-01-01")))
model_asiapacexjpn_4factor_3y_FF <- lm(Funds.RF ~ Mkt.RF + SMB + HML + WML, data = subset(AsiaPacific_Panel, date1 > as.Date("2017-01-01")))
model_asiapacexjpn_5factor_3y_FF <- lm(Funds.RF ~ Mkt.RF + SMB + HML + RMW + CMA, data = subset(AsiaPacific_Panel, date1 > as.Date("2017-01-01")))

model_asiapacexjpn_1factor_10y_CSI300 <- lm(Funds.RF ~ CSI300NRUSD.RF, data = AsiaPacific_Panel)
model_asiapacexjpn_3factor_10y_CSI300 <- lm(Funds.RF ~ CSI300NRUSD.RF + SMB + HML, data = AsiaPacific_Panel)
model_asiapacexjpn_4factor_10y_CSI300 <- lm(Funds.RF ~ CSI300NRUSD.RF + SMB + HML + WML, data = AsiaPacific_Panel)
model_asiapacexjpn_5factor_10y_CSI300 <- lm(Funds.RF ~ CSI300NRUSD.RF + SMB + HML + RMW + CMA, data = AsiaPacific_Panel)

model_asiapacexjpn_1factor_5y_CSI300 <- lm(Funds.RF ~ CSI300NRUSD.RF, data = subset(AsiaPacific_Panel, date1 > as.Date("2015-01-01")))
model_asiapacexjpn_3factor_5y_CSI300 <- lm(Funds.RF ~ CSI300NRUSD.RF + SMB + HML, data = subset(AsiaPacific_Panel, date1 > as.Date("2015-01-01")))
model_asiapacexjpn_4factor_5y_CSI300 <- lm(Funds.RF ~ CSI300NRUSD.RF + SMB + HML + WML, data = subset(AsiaPacific_Panel, date1 > as.Date("2015-01-01")))
model_asiapacexjpn_5factor_5y_CSI300 <- lm(Funds.RF ~ CSI300NRUSD.RF + SMB + HML + RMW + CMA, data = subset(AsiaPacific_Panel, date1 > as.Date("2015-01-01")))

model_asiapacexjpn_1factor_3y_CSI300 <- lm(Funds.RF ~ CSI300NRUSD.RF, data = subset(AsiaPacific_Panel, date1 > as.Date("2017-01-01")))
model_asiapacexjpn_3factor_3y_CSI300 <- lm(Funds.RF ~ CSI300NRUSD.RF + SMB + HML, data = subset(AsiaPacific_Panel, date1 > as.Date("2017-01-01")))
model_asiapacexjpn_4factor_3y_CSI300 <- lm(Funds.RF ~ CSI300NRUSD.RF + SMB + HML + WML, data = subset(AsiaPacific_Panel, date1 > as.Date("2017-01-01")))
model_asiapacexjpn_5factor_3y_CSI300 <- lm(Funds.RF ~ CSI300NRUSD.RF + SMB + HML + RMW + CMA, data = subset(AsiaPacific_Panel, date1 > as.Date("2017-01-01")))

model_asiapacexjpn_1factor_10y_MSCIAsiaPacExJpn <- lm(Funds.RF ~ MSCIACAsiaPacExJPNNRUSD.RF, data = AsiaPacific_Panel)
model_asiapacexjpn_3factor_10y_MSCIAsiaPacExJpn <- lm(Funds.RF ~ MSCIACAsiaPacExJPNNRUSD.RF + SMB + HML, data = AsiaPacific_Panel)
model_asiapacexjpn_4factor_10y_MSCIAsiaPacExJpn <- lm(Funds.RF ~ MSCIACAsiaPacExJPNNRUSD.RF + SMB + HML + WML, data = AsiaPacific_Panel)
model_asiapacexjpn_5factor_10y_MSCIAsiaPacExJpn <- lm(Funds.RF ~ MSCIACAsiaPacExJPNNRUSD.RF + SMB + HML + RMW + CMA, data = AsiaPacific_Panel)

model_asiapacexjpn_1factor_5y_MSCIAsiaPacExJpn <- lm(Funds.RF ~ MSCIACAsiaPacExJPNNRUSD.RF, data = subset(AsiaPacific_Panel, date1 > as.Date("2015-01-01")))
model_asiapacexjpn_3factor_5y_MSCIAsiaPacExJpn <- lm(Funds.RF ~ MSCIACAsiaPacExJPNNRUSD.RF + SMB + HML, data = subset(AsiaPacific_Panel, date1 > as.Date("2015-01-01")))
model_asiapacexjpn_4factor_5y_MSCIAsiaPacExJpn <- lm(Funds.RF ~ MSCIACAsiaPacExJPNNRUSD.RF + SMB + HML + WML, data = subset(AsiaPacific_Panel, date1 > as.Date("2015-01-01")))
model_asiapacexjpn_5factor_5y_MSCIAsiaPacExJpn <- lm(Funds.RF ~ MSCIACAsiaPacExJPNNRUSD.RF + SMB + HML + RMW + CMA, data = subset(AsiaPacific_Panel, date1 > as.Date("2015-01-01")))

model_asiapacexjpn_1factor_3y_MSCIAsiaPacExJpn <- lm(Funds.RF ~ MSCIACAsiaPacExJPNNRUSD.RF, data = subset(AsiaPacific_Panel, date1 > as.Date("2017-01-01")))
model_asiapacexjpn_3factor_3y_MSCIAsiaPacExJpn <- lm(Funds.RF ~ MSCIACAsiaPacExJPNNRUSD.RF + SMB + HML, data = subset(AsiaPacific_Panel, date1 > as.Date("2017-01-01")))
model_asiapacexjpn_4factor_3y_MSCIAsiaPacExJpn <- lm(Funds.RF ~ MSCIACAsiaPacExJPNNRUSD.RF + SMB + HML + WML, data = subset(AsiaPacific_Panel, date1 > as.Date("2017-01-01")))
model_asiapacexjpn_5factor_3y_MSCIAsiaPacExJpn <- lm(Funds.RF ~ MSCIACAsiaPacExJPNNRUSD.RF + SMB + HML + RMW + CMA, data = subset(AsiaPacific_Panel, date1 > as.Date("2017-01-01")))

model_asiapacexjpn_1factor_10y_CSISOE <- lm(Funds.RF ~ CSIStateownedEnterprisesCompPRCNY.RF, data = AsiaPacific_Panel)
model_asiapacexjpn_3factor_10y_CSISOE <- lm(Funds.RF ~ CSIStateownedEnterprisesCompPRCNY.RF + SMB + HML, data = AsiaPacific_Panel)
model_asiapacexjpn_4factor_10y_CSISOE <- lm(Funds.RF ~ CSIStateownedEnterprisesCompPRCNY.RF + SMB + HML + WML, data = AsiaPacific_Panel)
model_asiapacexjpn_5factor_10y_CSISOE <- lm(Funds.RF ~ CSIStateownedEnterprisesCompPRCNY.RF + SMB + HML + RMW + CMA, data = AsiaPacific_Panel)

model_asiapacexjpn_1factor_5y_CSISOE <- lm(Funds.RF ~ CSIStateownedEnterprisesCompPRCNY.RF, data = subset(AsiaPacific_Panel, date1 > as.Date("2015-01-01")))
model_asiapacexjpn_3factor_5y_CSISOE <- lm(Funds.RF ~ CSIStateownedEnterprisesCompPRCNY.RF + SMB + HML, data = subset(AsiaPacific_Panel, date1 > as.Date("2015-01-01")))
model_asiapacexjpn_4factor_5y_CSISOE <- lm(Funds.RF ~ CSIStateownedEnterprisesCompPRCNY.RF + SMB + HML + WML, data = subset(AsiaPacific_Panel, date1 > as.Date("2015-01-01")))
model_asiapacexjpn_5factor_5y_CSISOE <- lm(Funds.RF ~ CSIStateownedEnterprisesCompPRCNY.RF + SMB + HML + RMW + CMA, data = subset(AsiaPacific_Panel, date1 > as.Date("2015-01-01")))

model_asiapacexjpn_1factor_3y_CSISOE <- lm(Funds.RF ~ CSIStateownedEnterprisesCompPRCNY.RF, data = subset(AsiaPacific_Panel, date1 > as.Date("2017-01-01")))
model_asiapacexjpn_3factor_3y_CSISOE <- lm(Funds.RF ~ CSIStateownedEnterprisesCompPRCNY.RF + SMB + HML, data = subset(AsiaPacific_Panel, date1 > as.Date("2017-01-01")))
model_asiapacexjpn_4factor_3y_CSISOE <- lm(Funds.RF ~ CSIStateownedEnterprisesCompPRCNY.RF + SMB + HML + WML, data = subset(AsiaPacific_Panel, date1 > as.Date("2017-01-01")))
model_asiapacexjpn_5factor_3y_CSISOE <- lm(Funds.RF ~ CSIStateownedEnterprisesCompPRCNY.RF + SMB + HML + RMW + CMA, data = subset(AsiaPacific_Panel, date1 > as.Date("2017-01-01")))




#### India

India_Panel <- India_Panel %>% 
  left_join(Benchmarks_Factors, by = "date1") 

India_Panel <- India_Panel %>% 
  mutate(Funds.RF = MonthlyReturn - RF) %>% 
  mutate(IISLNifty50TRINR.RF = IISLNifty50TRINR - RF) %>% 
  mutate(MSCIIndiaNRUSD.RF = MSCIIndiaNRUSD - RF) %>% 
  mutate(SPBSE500IndiaTRINR.RF = SPBSE500IndiaTRINR...40 - RF)

model_india_1factor_10y_FF <- lm(Funds.RF ~ Mkt.RF, data = India_Panel)
model_india_3factor_10y_FF <- lm(Funds.RF ~ Mkt.RF + SMB + HML, data = India_Panel)
model_india_4factor_10y_FF <- lm(Funds.RF ~ Mkt.RF + SMB + HML + WML, data = India_Panel)
model_india_5factor_10y_FF <- lm(Funds.RF ~ Mkt.RF + SMB + HML + RMW + CMA, data = India_Panel)

model_india_1factor_5y_FF <- lm(Funds.RF ~ Mkt.RF, data = subset(India_Panel, date1 > as.Date("2015-01-01")))
model_india_3factor_5y_FF <- lm(Funds.RF ~ Mkt.RF + SMB + HML, data = subset(India_Panel, date1 > as.Date("2015-01-01")))
model_india_4factor_5y_FF <- lm(Funds.RF ~ Mkt.RF + SMB + HML + WML, data = subset(India_Panel, date1 > as.Date("2015-01-01")))
model_india_5factor_5y_FF <- lm(Funds.RF ~ Mkt.RF + SMB + HML + RMW + CMA, data = subset(India_Panel, date1 > as.Date("2015-01-01")))

model_india_1factor_3y_FF <- lm(Funds.RF ~ Mkt.RF, data = subset(India_Panel, date1 > as.Date("2017-01-01")))
model_india_3factor_3y_FF <- lm(Funds.RF ~ Mkt.RF + SMB + HML, data = subset(India_Panel, date1 > as.Date("2017-01-01")))
model_india_4factor_3y_FF <- lm(Funds.RF ~ Mkt.RF + SMB + HML + WML, data = subset(India_Panel, date1 > as.Date("2017-01-01")))
model_india_5factor_3y_FF <- lm(Funds.RF ~ Mkt.RF + SMB + HML + RMW + CMA, data = subset(India_Panel, date1 > as.Date("2017-01-01")))

model_india_1factor_10y_IISLNifty50 <- lm(Funds.RF ~ IISLNifty50TRINR.RF, data = India_Panel)
model_india_3factor_10y_IISLNifty50 <- lm(Funds.RF ~ IISLNifty50TRINR.RF + SMB + HML, data = India_Panel)
model_india_4factor_10y_IISLNifty50 <- lm(Funds.RF ~ IISLNifty50TRINR.RF + SMB + HML + WML, data = India_Panel)
model_india_5factor_10y_IISLNifty50 <- lm(Funds.RF ~ IISLNifty50TRINR.RF + SMB + HML + RMW + CMA, data = India_Panel)

model_india_1factor_5y_IISLNifty50 <- lm(Funds.RF ~ IISLNifty50TRINR.RF, data = subset(India_Panel, date1 > as.Date("2015-01-01")))
model_india_3factor_5y_IISLNifty50 <- lm(Funds.RF ~ IISLNifty50TRINR.RF + SMB + HML, data = subset(India_Panel, date1 > as.Date("2015-01-01")))
model_india_4factor_5y_IISLNifty50 <- lm(Funds.RF ~ IISLNifty50TRINR.RF + SMB + HML + WML, data = subset(India_Panel, date1 > as.Date("2015-01-01")))
model_india_5factor_5y_IISLNifty50 <- lm(Funds.RF ~ IISLNifty50TRINR.RF + SMB + HML + RMW + CMA, data = subset(India_Panel, date1 > as.Date("2015-01-01")))

model_india_1factor_3y_IISLNifty50 <- lm(Funds.RF ~ IISLNifty50TRINR.RF, data = subset(India_Panel, date1 > as.Date("2017-01-01")))
model_india_3factor_3y_IISLNifty50 <- lm(Funds.RF ~ IISLNifty50TRINR.RF + SMB + HML, data = subset(India_Panel, date1 > as.Date("2017-01-01")))
model_india_4factor_3y_IISLNifty50 <- lm(Funds.RF ~ IISLNifty50TRINR.RF + SMB + HML + WML, data = subset(India_Panel, date1 > as.Date("2017-01-01")))
model_india_5factor_3y_IISLNifty50 <- lm(Funds.RF ~ IISLNifty50TRINR.RF + SMB + HML + RMW + CMA, data = subset(India_Panel, date1 > as.Date("2017-01-01")))

model_india_1factor_10y_MSCIIndia <- lm(Funds.RF ~ MSCIIndiaNRUSD.RF, data = India_Panel)
model_india_3factor_10y_MSCIIndia <- lm(Funds.RF ~ MSCIIndiaNRUSD.RF + SMB + HML, data = India_Panel)
model_india_4factor_10y_MSCIIndia <- lm(Funds.RF ~ MSCIIndiaNRUSD.RF + SMB + HML + WML, data = India_Panel)
model_india_5factor_10y_MSCIIndia <- lm(Funds.RF ~ MSCIIndiaNRUSD.RF + SMB + HML + RMW + CMA, data = India_Panel)

model_india_1factor_5y_MSCIIndia <- lm(Funds.RF ~ MSCIIndiaNRUSD.RF, data = subset(India_Panel, date1 > as.Date("2015-01-01")))
model_india_3factor_5y_MSCIIndia <- lm(Funds.RF ~ MSCIIndiaNRUSD.RF + SMB + HML, data = subset(India_Panel, date1 > as.Date("2015-01-01")))
model_india_4factor_5y_MSCIIndia <- lm(Funds.RF ~ MSCIIndiaNRUSD.RF + SMB + HML + WML, data = subset(India_Panel, date1 > as.Date("2015-01-01")))
model_india_5factor_5y_MSCIIndia <- lm(Funds.RF ~ MSCIIndiaNRUSD.RF + SMB + HML + RMW + CMA, data = subset(India_Panel, date1 > as.Date("2015-01-01")))

model_india_1factor_3y_MSCIIndia <- lm(Funds.RF ~ MSCIIndiaNRUSD.RF, data = subset(India_Panel, date1 > as.Date("2017-01-01")))
model_india_3factor_3y_MSCIIndia <- lm(Funds.RF ~ MSCIIndiaNRUSD.RF + SMB + HML, data = subset(India_Panel, date1 > as.Date("2017-01-01")))
model_india_4factor_3y_MSCIIndia <- lm(Funds.RF ~ MSCIIndiaNRUSD.RF + SMB + HML + WML, data = subset(India_Panel, date1 > as.Date("2017-01-01")))
model_india_5factor_3y_MSCIIndia <- lm(Funds.RF ~ MSCIIndiaNRUSD.RF + SMB + HML + RMW + CMA, data = subset(India_Panel, date1 > as.Date("2017-01-01")))

model_india_1factor_10y_SPBSE500India <- lm(Funds.RF ~ SPBSE500IndiaTRINR.RF, data = India_Panel)
model_india_3factor_10y_SPBSE500India <- lm(Funds.RF ~ SPBSE500IndiaTRINR.RF + SMB + HML, data = India_Panel)
model_india_4factor_10y_SPBSE500India <- lm(Funds.RF ~ SPBSE500IndiaTRINR.RF + SMB + HML + WML, data = India_Panel)
model_india_5factor_10y_SPBSE500India <- lm(Funds.RF ~ SPBSE500IndiaTRINR.RF + SMB + HML + RMW + CMA, data = India_Panel)

model_india_1factor_5y_SPBSE500India <- lm(Funds.RF ~ SPBSE500IndiaTRINR.RF, data = subset(India_Panel, date1 > as.Date("2015-01-01")))
model_india_3factor_5y_SPBSE500India <- lm(Funds.RF ~ SPBSE500IndiaTRINR.RF + SMB + HML, data = subset(India_Panel, date1 > as.Date("2015-01-01")))
model_india_4factor_5y_SPBSE500India <- lm(Funds.RF ~ SPBSE500IndiaTRINR.RF + SMB + HML + WML, data = subset(India_Panel, date1 > as.Date("2015-01-01")))
model_india_5factor_5y_SPBSE500India <- lm(Funds.RF ~ SPBSE500IndiaTRINR.RF + SMB + HML + RMW + CMA, data = subset(India_Panel, date1 > as.Date("2015-01-01")))

model_india_1factor_3y_SPBSE500India <- lm(Funds.RF ~ SPBSE500IndiaTRINR.RF, data = subset(India_Panel, date1 > as.Date("2017-01-01")))
model_india_3factor_3y_SPBSE500India <- lm(Funds.RF ~ SPBSE500IndiaTRINR.RF + SMB + HML, data = subset(India_Panel, date1 > as.Date("2017-01-01")))
model_india_4factor_3y_SPBSE500India <- lm(Funds.RF ~ SPBSE500IndiaTRINR.RF + SMB + HML + WML, data = subset(India_Panel, date1 > as.Date("2017-01-01")))
model_india_5factor_3y_SPBSE500India <- lm(Funds.RF ~ SPBSE500IndiaTRINR.RF + SMB + HML + RMW + CMA, data = subset(India_Panel, date1 > as.Date("2017-01-01")))


#### Asia Emerging Markets

AsiaEmerg_Panel <- AsiaEmerg_Panel %>% 
  left_join(Benchmarks_Factors, by = "date1") 

AsiaEmerg_Panel <- AsiaEmerg_Panel %>% 
  mutate(Funds.RF = MonthlyReturn - RF) %>% 
  mutate(MSCIEMAsia1040NRUSD.RF = MSCIEMAsia1040NRUSD - RF) %>% 
  mutate(MSCIEMAsiaNRUSD.RF = MSCIEMAsiaNRUSD - RF) %>% 
  mutate(CSIStateownedEnterprisesCompPRCNY.RF = CSIStateownedEnterprisesCompPRCNY - RF)
  
model_asiaemerg_1factor_10y_FF <- lm(Funds.RF ~ Mkt.RF, data = AsiaEmerg_Panel)
model_asiaemerg_3factor_10y_FF <- lm(Funds.RF ~ Mkt.RF + SMB + HML, data = AsiaEmerg_Panel)
model_asiaemerg_4factor_10y_FF <- lm(Funds.RF ~ Mkt.RF + SMB + HML + WML, data = AsiaEmerg_Panel)
model_asiaemerg_5factor_10y_FF <- lm(Funds.RF ~ Mkt.RF + SMB + HML + RMW + CMA, data = AsiaEmerg_Panel)

model_asiaemerg_1factor_5y_FF <- lm(Funds.RF ~ Mkt.RF, data = subset(AsiaEmerg_Panel, date1 > as.Date("2015-01-01")))
model_asiaemerg_3factor_5y_FF <- lm(Funds.RF ~ Mkt.RF + SMB + HML, data = subset(AsiaEmerg_Panel, date1 > as.Date("2015-01-01")))
model_asiaemerg_4factor_5y_FF <- lm(Funds.RF ~ Mkt.RF + SMB + HML + WML, data = subset(AsiaEmerg_Panel, date1 > as.Date("2015-01-01")))
model_asiaemerg_5factor_5y_FF <- lm(Funds.RF ~ Mkt.RF + SMB + HML + RMW + CMA, data = subset(AsiaEmerg_Panel, date1 > as.Date("2015-01-01")))

model_asiaemerg_1factor_3y_FF <- lm(Funds.RF ~ Mkt.RF, data = subset(AsiaEmerg_Panel, date1 > as.Date("2017-01-01")))
model_asiaemerg_3factor_3y_FF <- lm(Funds.RF ~ Mkt.RF + SMB + HML, data = subset(AsiaEmerg_Panel, date1 > as.Date("2017-01-01")))
model_asiaemerg_4factor_3y_FF <- lm(Funds.RF ~ Mkt.RF + SMB + HML + WML, data = subset(AsiaEmerg_Panel, date1 > as.Date("2017-01-01")))
model_asiaemerg_5factor_3y_FF <- lm(Funds.RF ~ Mkt.RF + SMB + HML + RMW + CMA, data = subset(AsiaEmerg_Panel, date1 > as.Date("2017-01-01")))

model_asiaemerg_1factor_10y_MSCIEMAsia1040 <- lm(Funds.RF ~ MSCIEMAsia1040NRUSD.RF, data = AsiaEmerg_Panel)
model_asiaemerg_3factor_10y_MSCIEMAsia1040 <- lm(Funds.RF ~ MSCIEMAsia1040NRUSD.RF + SMB + HML, data = AsiaEmerg_Panel)
model_asiaemerg_4factor_10y_MSCIEMAsia1040 <- lm(Funds.RF ~ MSCIEMAsia1040NRUSD.RF + SMB + HML + WML, data = AsiaEmerg_Panel)
model_asiaemerg_5factor_10y_MSCIEMAsia1040 <- lm(Funds.RF ~ MSCIEMAsia1040NRUSD.RF + SMB + HML + RMW + CMA, data = AsiaEmerg_Panel)

model_asiaemerg_1factor_5y_MSCIEMAsia1040 <- lm(Funds.RF ~ MSCIEMAsia1040NRUSD.RF, data = subset(AsiaEmerg_Panel, date1 > as.Date("2015-01-01")))
model_asiaemerg_3factor_5y_MSCIEMAsia1040 <- lm(Funds.RF ~ MSCIEMAsia1040NRUSD.RF + SMB + HML, data = subset(AsiaEmerg_Panel, date1 > as.Date("2015-01-01")))
model_asiaemerg_4factor_5y_MSCIEMAsia1040 <- lm(Funds.RF ~ MSCIEMAsia1040NRUSD.RF + SMB + HML + WML, data = subset(AsiaEmerg_Panel, date1 > as.Date("2015-01-01")))
model_asiaemerg_5factor_5y_MSCIEMAsia1040 <- lm(Funds.RF ~ MSCIEMAsia1040NRUSD.RF + SMB + HML + RMW + CMA, data = subset(AsiaEmerg_Panel, date1 > as.Date("2015-01-01")))

model_asiaemerg_1factor_3y_MSCIEMAsia1040 <- lm(Funds.RF ~ MSCIEMAsia1040NRUSD.RF, data = subset(AsiaEmerg_Panel, date1 > as.Date("2017-01-01")))
model_asiaemerg_3factor_3y_MSCIEMAsia1040 <- lm(Funds.RF ~ MSCIEMAsia1040NRUSD.RF + SMB + HML, data = subset(AsiaEmerg_Panel, date1 > as.Date("2017-01-01")))
model_asiaemerg_4factor_3y_MSCIEMAsia1040 <- lm(Funds.RF ~ MSCIEMAsia1040NRUSD.RF + SMB + HML + WML, data = subset(AsiaEmerg_Panel, date1 > as.Date("2017-01-01")))
model_asiaemerg_5factor_3y_MSCIEMAsia1040 <- lm(Funds.RF ~ MSCIEMAsia1040NRUSD.RF + SMB + HML + RMW + CMA, data = subset(AsiaEmerg_Panel, date1 > as.Date("2017-01-01")))

model_asiaemerg_1factor_10y_MSCIEMAsia <- lm(Funds.RF ~ MSCIEMAsiaNRUSD.RF, data = AsiaEmerg_Panel)
model_asiaemerg_3factor_10y_MSCIEMAsia <- lm(Funds.RF ~ MSCIEMAsiaNRUSD.RF + SMB + HML, data = AsiaEmerg_Panel)
model_asiaemerg_4factor_10y_MSCIEMAsia <- lm(Funds.RF ~ MSCIEMAsiaNRUSD.RF + SMB + HML + WML, data = AsiaEmerg_Panel)
model_asiaemerg_5factor_10y_MSCIEMAsia <- lm(Funds.RF ~ MSCIEMAsiaNRUSD.RF + SMB + HML + RMW + CMA, data = AsiaEmerg_Panel)

model_asiaemerg_1factor_5y_MSCIEMAsia <- lm(Funds.RF ~ MSCIEMAsiaNRUSD.RF, data = subset(AsiaEmerg_Panel, date1 > as.Date("2015-01-01")))
model_asiaemerg_3factor_5y_MSCIEMAsia <- lm(Funds.RF ~ MSCIEMAsiaNRUSD.RF + SMB + HML, data = subset(AsiaEmerg_Panel, date1 > as.Date("2015-01-01")))
model_asiaemerg_4factor_5y_MSCIEMAsia <- lm(Funds.RF ~ MSCIEMAsiaNRUSD.RF + SMB + HML + WML, data = subset(AsiaEmerg_Panel, date1 > as.Date("2015-01-01")))
model_asiaemerg_5factor_5y_MSCIEMAsia <- lm(Funds.RF ~ MSCIEMAsiaNRUSD.RF + SMB + HML + RMW + CMA, data = subset(AsiaEmerg_Panel, date1 > as.Date("2015-01-01")))

model_asiaemerg_1factor_3y_MSCIEMAsia <- lm(Funds.RF ~ MSCIEMAsiaNRUSD.RF, data = subset(AsiaEmerg_Panel, date1 > as.Date("2017-01-01")))
model_asiaemerg_3factor_3y_MSCIEMAsia <- lm(Funds.RF ~ MSCIEMAsiaNRUSD.RF + SMB + HML, data = subset(AsiaEmerg_Panel, date1 > as.Date("2017-01-01")))
model_asiaemerg_4factor_3y_MSCIEMAsia <- lm(Funds.RF ~ MSCIEMAsiaNRUSD.RF + SMB + HML + WML, data = subset(AsiaEmerg_Panel, date1 > as.Date("2017-01-01")))
model_asiaemerg_5factor_3y_MSCIEMAsia <- lm(Funds.RF ~ MSCIEMAsiaNRUSD.RF + SMB + HML + RMW + CMA, data = subset(AsiaEmerg_Panel, date1 > as.Date("2017-01-01")))

model_asiaemerg_1factor_10y_CSIStateownedEnterprisesComp <- lm(Funds.RF ~ CSIStateownedEnterprisesCompPRCNY.RF, data = AsiaEmerg_Panel)
model_asiaemerg_3factor_10y_CSIStateownedEnterprisesComp <- lm(Funds.RF ~ CSIStateownedEnterprisesCompPRCNY.RF + SMB + HML, data = AsiaEmerg_Panel)
model_asiaemerg_4factor_10y_CSIStateownedEnterprisesComp <- lm(Funds.RF ~ CSIStateownedEnterprisesCompPRCNY.RF + SMB + HML + WML, data = AsiaEmerg_Panel)
model_asiaemerg_5factor_10y_CSIStateownedEnterprisesComp <- lm(Funds.RF ~ CSIStateownedEnterprisesCompPRCNY.RF + SMB + HML + RMW + CMA, data = AsiaEmerg_Panel)

model_asiaemerg_1factor_5y_CSIStateownedEnterprisesComp <- lm(Funds.RF ~ CSIStateownedEnterprisesCompPRCNY.RF, data = subset(AsiaEmerg_Panel, date1 > as.Date("2015-01-01")))
model_asiaemerg_3factor_5y_CSIStateownedEnterprisesComp <- lm(Funds.RF ~ CSIStateownedEnterprisesCompPRCNY.RF + SMB + HML, data = subset(AsiaEmerg_Panel, date1 > as.Date("2015-01-01")))
model_asiaemerg_4factor_5y_CSIStateownedEnterprisesComp <- lm(Funds.RF ~ CSIStateownedEnterprisesCompPRCNY.RF + SMB + HML + WML, data = subset(AsiaEmerg_Panel, date1 > as.Date("2015-01-01")))
model_asiaemerg_5factor_5y_CSIStateownedEnterprisesComp <- lm(Funds.RF ~ CSIStateownedEnterprisesCompPRCNY.RF + SMB + HML + RMW + CMA, data = subset(AsiaEmerg_Panel, date1 > as.Date("2015-01-01")))

model_asiaemerg_1factor_3y_CSIStateownedEnterprisesComp <- lm(Funds.RF ~ CSIStateownedEnterprisesCompPRCNY.RF, data = subset(AsiaEmerg_Panel, date1 > as.Date("2017-01-01")))
model_asiaemerg_3factor_3y_CSIStateownedEnterprisesComp <- lm(Funds.RF ~ CSIStateownedEnterprisesCompPRCNY.RF + SMB + HML, data =subset(AsiaEmerg_Panel, date1 > as.Date("2017-01-01")))
model_asiaemerg_4factor_3y_CSIStateownedEnterprisesComp <- lm(Funds.RF ~ CSIStateownedEnterprisesCompPRCNY.RF + SMB + HML + WML, data = subset(AsiaEmerg_Panel, date1 > as.Date("2017-01-01")))
model_asiaemerg_5factor_3y_CSIStateownedEnterprisesComp <- lm(Funds.RF ~ CSIStateownedEnterprisesCompPRCNY.RF + SMB + HML + RMW + CMA, data = subset(AsiaEmerg_Panel, date1 > as.Date("2017-01-01")))


##### Regressions for Cap-Style Categorization

#' India Mid Cap

India_Mid_Panel <- India_Mid_Panel %>% 
  left_join(Benchmarks_Factors, by = "date1") 

India_Mid_Panel <- India_Mid_Panel %>% 
  mutate(Funds.RF = MonthlyReturn - RF) %>% 
  mutate(MSCIIndiaMidNRUSD.RF = MSCIIndiaMidNRUSD - RF) %>% 
  mutate(IISLNiftyMidcap150TRINR.RF = IISLNiftyMidcap150TRINR - RF) %>% 
  mutate(SPBSE500IndiaTRINR.RF = SPBSE500IndiaTRINR...40 - RF)


model_india_mid_1factor_10y_FF <- lm(Funds.RF ~ Mkt.RF, data = India_Mid_Panel)
model_india_mid_3factor_10y_FF <- lm(Funds.RF ~ Mkt.RF + SMB + HML, data = India_Mid_Panel)
model_india_mid_4factor_10y_FF <- lm(Funds.RF ~ Mkt.RF + SMB + HML + WML, data = India_Mid_Panel)
model_india_mid_5factor_10y_FF <- lm(Funds.RF ~ Mkt.RF + SMB + HML + RMW + CMA, data = India_Mid_Panel)

model_india_mid_1factor_10y_MSCIIndiaMid <- lm(Funds.RF ~ MSCIIndiaMidNRUSD.RF, data = India_Mid_Panel)
model_india_mid_3factor_10y_MSCIIndiaMid <- lm(Funds.RF ~ MSCIIndiaMidNRUSD.RF + SMB + HML, data = India_Mid_Panel)
model_india_mid_4factor_10y_MSCIIndiaMid <- lm(Funds.RF ~ MSCIIndiaMidNRUSD.RF + SMB + HML + WML, data = India_Mid_Panel)
model_india_mid_5factor_10y_MSCIIndiaMid <- lm(Funds.RF ~ MSCIIndiaMidNRUSD.RF + SMB + HML + RMW + CMA, data = India_Mid_Panel)

model_india_mid_1factor_10y_IISLNiftyMidcap150 <- lm(Funds.RF ~ IISLNiftyMidcap150TRINR.RF, data = India_Mid_Panel)
model_india_mid_3factor_10y_IISLNiftyMidcap150 <- lm(Funds.RF ~ IISLNiftyMidcap150TRINR.RF + SMB + HML, data = India_Mid_Panel)
model_india_mid_4factor_10y_IISLNiftyMidcap150 <- lm(Funds.RF ~ IISLNiftyMidcap150TRINR.RF + SMB + HML + WML, data = India_Mid_Panel)
model_india_mid_5factor_10y_IISLNiftyMidcap150 <- lm(Funds.RF ~ IISLNiftyMidcap150TRINR.RF + SMB + HML + RMW + CMA, data = India_Mid_Panel)

model_india_mid_1factor_10y_SPBSE500India <- lm(Funds.RF ~ SPBSE500IndiaTRINR.RF, data = India_Mid_Panel)
model_india_mid_3factor_10y_SPBSE500India <- lm(Funds.RF ~ SPBSE500IndiaTRINR.RF + SMB + HML, data = India_Mid_Panel)
model_india_mid_4factor_10y_SPBSE500India <- lm(Funds.RF ~ SPBSE500IndiaTRINR.RF + SMB + HML + WML, data = India_Mid_Panel)
model_india_mid_5factor_10y_SPBSE500India <- lm(Funds.RF ~ SPBSE500IndiaTRINR.RF + SMB + HML + RMW + CMA, data = India_Mid_Panel)


#' India Small Cap

India_Small_Panel <- India_Small_Panel %>% 
  left_join(Benchmarks_Factors, by = "date1") 

India_Small_Panel <- India_Small_Panel %>% 
  mutate(Funds.RF = MonthlyReturn - RF) %>% 
  mutate(MSCIIndiaSmallNRUSD.RF = MSCIIndiaSmallNRUSD - RF) %>% 
  mutate(IISLNiftySmallcap250TRINR.RF = IISLNiftySmallcap250TRINR...39 - RF) %>% 
  mutate(SPBSE500IndiaTRINR.RF = SPBSE500IndiaTRINR...40 - RF)

model_india_small_1factor_10y_FF <- lm(Funds.RF ~ Mkt.RF, data = India_Small_Panel)
model_india_small_3factor_10y_FF <- lm(Funds.RF ~ Mkt.RF + SMB + HML, data = India_Small_Panel)
model_india_small_4factor_10y_FF <- lm(Funds.RF ~ Mkt.RF + SMB + HML + WML, data = India_Small_Panel)
model_india_small_5factor_10y_FF <- lm(Funds.RF ~ Mkt.RF + SMB + HML + RMW + CMA, data = India_Small_Panel)

model_india_small_1factor_10y_MSCIIndiaSmall <- lm(Funds.RF ~ MSCIIndiaSmallNRUSD.RF, data = India_Small_Panel)
model_india_small_3factor_10y_MSCIIndiaSmall <- lm(Funds.RF ~ MSCIIndiaSmallNRUSD.RF + SMB + HML, data = India_Small_Panel)
model_india_small_4factor_10y_MSCIIndiaSmall <- lm(Funds.RF ~ MSCIIndiaSmallNRUSD.RF + SMB + HML + WML, data = India_Small_Panel)
model_india_small_5factor_10y_MSCIIndiaSmall <- lm(Funds.RF ~ MSCIIndiaSmallNRUSD.RF + SMB + HML + RMW + CMA, data = India_Small_Panel)

model_india_small_1factor_10y_IISLNiftySmallcap250 <- lm(Funds.RF ~ IISLNiftySmallcap250TRINR.RF, data = India_Small_Panel)
model_india_small_3factor_10y_IISLNiftySmallcap250 <- lm(Funds.RF ~ IISLNiftySmallcap250TRINR.RF + SMB + HML, data = India_Small_Panel)
model_india_small_4factor_10y_IISLNiftySmallcap250 <- lm(Funds.RF ~ IISLNiftySmallcap250TRINR.RF + SMB + HML + WML, data = India_Small_Panel)
model_india_small_5factor_10y_IISLNiftySmallcap250 <- lm(Funds.RF ~ IISLNiftySmallcap250TRINR.RF + SMB + HML + RMW + CMA, data = India_Small_Panel)

model_india_small_1factor_10y_SPBSE500India <- lm(Funds.RF ~ SPBSE500IndiaTRINR.RF, data = India_Small_Panel)
model_india_small_3factor_10y_SPBSE500India <- lm(Funds.RF ~ SPBSE500IndiaTRINR.RF + SMB + HML, data = India_Small_Panel)
model_india_small_4factor_10y_SPBSE500India <- lm(Funds.RF ~ SPBSE500IndiaTRINR.RF + SMB + HML + WML, data = India_Small_Panel)
model_india_small_5factor_10y_SPBSE500India <- lm(Funds.RF ~ SPBSE500IndiaTRINR.RF + SMB + HML + RMW + CMA, data = India_Small_Panel)



#' China Mid Cap

China_Mid_Panel <- China_Mid_Panel %>% 
  left_join(Benchmarks_Factors, by = "date1") 

China_Mid_Panel <- China_Mid_Panel %>% 
  mutate(Funds.RF = MonthlyReturn - RF) %>% 
  mutate(CSI200MidCapPRCNY.RF = CSI200MidCapPRCNY - RF) %>% 
  mutate(MSCIChinaMidGRUSD.RF = MSCIChinaMidGRUSD - RF) %>% 
  mutate(CSIStateownedEnterprisesCompPRCNY.RF = CSIStateownedEnterprisesCompPRCNY - RF)

model_china_mid_1factor_10y_FF <- lm(Funds.RF ~ Mkt.RF, data = China_Mid_Panel)
model_china_mid_3factor_10y_FF <- lm(Funds.RF ~ Mkt.RF + SMB + HML, data = China_Mid_Panel)
model_china_mid_4factor_10y_FF <- lm(Funds.RF ~ Mkt.RF + SMB + HML + WML, data = China_Mid_Panel)
model_china_mid_5factor_10y_FF <- lm(Funds.RF ~ Mkt.RF + SMB + HML + RMW + CMA, data = China_Mid_Panel)

model_china_mid_1factor_10y_CSI200 <- lm(Funds.RF ~ CSI200MidCapPRCNY.RF, data = China_Mid_Panel)
model_china_mid_3factor_10y_CSI200 <- lm(Funds.RF ~ CSI200MidCapPRCNY.RF + SMB + HML, data = China_Mid_Panel)
model_china_mid_4factor_10y_CSI200 <- lm(Funds.RF ~ CSI200MidCapPRCNY.RF + SMB + HML + WML, data = China_Mid_Panel)
model_china_mid_5factor_10y_CSI200 <- lm(Funds.RF ~ CSI200MidCapPRCNY.RF + SMB + HML + RMW + CMA, data = China_Mid_Panel)

model_china_mid_1factor_10y_MSCIChinaMid <- lm(Funds.RF ~ MSCIChinaMidGRUSD.RF, data = China_Mid_Panel)
model_china_mid_3factor_10y_MSCIChinaMid <- lm(Funds.RF ~ MSCIChinaMidGRUSD.RF + SMB + HML, data = China_Mid_Panel)
model_china_mid_4factor_10y_MSCIChinaMid <- lm(Funds.RF ~ MSCIChinaMidGRUSD.RF + SMB + HML + WML, data = China_Mid_Panel)
model_china_mid_5factor_10y_MSCIChinaMid <- lm(Funds.RF ~ MSCIChinaMidGRUSD.RF + SMB + HML + RMW + CMA, data = China_Mid_Panel)

model_china_mid_1factor_10y_CSIStateownedEnterprises <- lm(Funds.RF ~ CSIStateownedEnterprisesCompPRCNY.RF, data = China_Mid_Panel)
model_china_mid_3factor_10y_CSIStateownedEnterprises <- lm(Funds.RF ~ CSIStateownedEnterprisesCompPRCNY.RF + SMB + HML, data = China_Mid_Panel)
model_china_mid_4factor_10y_CSIStateownedEnterprises <- lm(Funds.RF ~ CSIStateownedEnterprisesCompPRCNY.RF + SMB + HML + WML, data = China_Mid_Panel)
model_china_mid_5factor_10y_CSIStateownedEnterprises <- lm(Funds.RF ~ CSIStateownedEnterprisesCompPRCNY.RF + SMB + HML + RMW + CMA, data = China_Mid_Panel)



#' Asia Pacific Mid Cap

AsiaPacific_Mid_Panel <- AsiaPacific_Mid_Panel %>% 
  left_join(Benchmarks_Factors, by = "date1") 

AsiaPacific_Mid_Panel <- AsiaPacific_Mid_Panel %>% 
  mutate(Funds.RF = MonthlyReturn - RF) %>% 
  mutate(MSCIACAsiaPacificExJapanMidNRUSD.RF = MSCIACAsiaPacificExJapanMidNRUSD - RF) %>% 
  mutate(MSCIACAsiaPacExJPNNRUSD.RF = MSCIACAsiaPacExJPNNRUSD - RF)


model_asiapacexjpn_mid_1factor_10y_FF <- lm(Funds.RF ~ Mkt.RF, data = AsiaPacific_Mid_Panel)
model_asiapacexjpn_mid_3factor_10y_FF <- lm(Funds.RF ~ Mkt.RF + SMB + HML, data = AsiaPacific_Mid_Panel)
model_asiapacexjpn_mid_4factor_10y_FF <- lm(Funds.RF ~ Mkt.RF + SMB + HML + WML, data = AsiaPacific_Mid_Panel)
model_asiapacexjpn_mid_5factor_10y_FF <- lm(Funds.RF ~ Mkt.RF + SMB + HML + RMW + CMA, data = AsiaPacific_Mid_Panel)

model_asiapacexjpn_mid_1factor_10y_MSCIACAsiaPacificExJapanMid <- lm(Funds.RF ~ MSCIACAsiaPacificExJapanMidNRUSD.RF, data = AsiaPacific_Mid_Panel)
model_asiapacexjpn_mid_3factor_10y_MSCIACAsiaPacificExJapanMid <- lm(Funds.RF ~ MSCIACAsiaPacificExJapanMidNRUSD.RF + SMB + HML, data = AsiaPacific_Mid_Panel)
model_asiapacexjpn_mid_4factor_10y_MSCIACAsiaPacificExJapanMid <- lm(Funds.RF ~ MSCIACAsiaPacificExJapanMidNRUSD.RF + SMB + HML + WML, data = AsiaPacific_Mid_Panel)
model_asiapacexjpn_mid_5factor_10y_MSCIACAsiaPacificExJapanMid <- lm(Funds.RF ~ MSCIACAsiaPacificExJapanMidNRUSD.RF + SMB + HML + RMW + CMA, data = AsiaPacific_Mid_Panel)

model_asiapacexjpn_mid_1factor_10y_MSCIACAsiaPacExJPN <- lm(Funds.RF ~ MSCIACAsiaPacExJPNNRUSD.RF, data = AsiaPacific_Mid_Panel)
model_asiapacexjpn_mid_3factor_10y_MSCIACAsiaPacExJPN <- lm(Funds.RF ~ MSCIACAsiaPacExJPNNRUSD.RF + SMB + HML, data = AsiaPacific_Mid_Panel)
model_asiapacexjpn_mid_4factor_10y_MSCIACAsiaPacExJPN <- lm(Funds.RF ~ MSCIACAsiaPacExJPNNRUSD.RF + SMB + HML + WML, data = AsiaPacific_Mid_Panel)
model_asiapacexjpn_mid_5factor_10y_MSCIACAsiaPacExJPN <- lm(Funds.RF ~ MSCIACAsiaPacExJPNNRUSD.RF + SMB + HML + RMW + CMA, data = AsiaPacific_Mid_Panel)


##### One Regression per fund

#Only keep funds with at least 24 observations
China_Panel_Ind <- China_Panel[as.numeric(ave(China_Panel$FundID, China_Panel$FundID, FUN=length)) >= 24, ]
AsiaPacific_Panel_Ind <- AsiaPacific_Panel[as.numeric(ave(AsiaPacific_Panel$FundID, AsiaPacific_Panel$FundID, FUN=length)) >= 24, ]
India_Panel_Ind <- India_Panel[as.numeric(ave(India_Panel$FundID, India_Panel$FundID, FUN=length)) >= 24, ]
AsiaEmerg_Panel_Ind <- AsiaEmerg_Panel[as.numeric(ave(AsiaEmerg_Panel$FundID, AsiaEmerg_Panel$FundID, FUN=length)) >= 24, ]

China_Panel_Ind_1factor_10y_FF <- China_Panel_Ind %>% 
  group_by(FundID) %>%
  do(formula_1factor_FF = lm(Funds.RF ~ Mkt.RF , data = .))

fund_alphas_china_1factor_10y_FF <- tidy(China_Panel_Ind_1factor_10y_FF, formula_1factor_FF) %>% 
      filter(term == "(Intercept)")

fund_alphas_china_1factor_10y_FF %>% 
  ggplot(aes(estimate)) +
      geom_freqpoly(bins = 50)

sum(fund_alphas_china_1factor_10y_FF$estimate > 0 & fund_alphas_china_1factor_10y_FF$p.value < 0.05)
sum(fund_alphas_china_1factor_10y_FF$estimate < 0 & fund_alphas_china_1factor_10y_FF$p.value < 0.05)
sum(fund_alphas_china_1factor_10y_FF$p.value >= 0.05)

China_Panel_Ind_4factor_10y_FF <- China_Panel_Ind %>% 
  group_by(FundID) %>%
  do(formula_4factor_FF = lm(Funds.RF ~ Mkt.RF + SMB + HML + WML , data = .))

fund_alphas_china_4factor_10y_FF <- tidy(China_Panel_Ind_4factor_10y_FF, formula_4factor_FF) %>% 
  filter(term == "(Intercept)")

fund_alphas_china_4factor_10y_FF %>% 
  ggplot(aes(estimate)) +
  geom_freqpoly(bins = 50)


sum(fund_alphas_china_4factor_10y_FF$estimate > 0 & fund_alphas_china_4factor_10y_FF$p.value < 0.05)
sum(fund_alphas_china_4factor_10y_FF$estimate < 0 & fund_alphas_china_4factor_10y_FF$p.value < 0.05)
sum(fund_alphas_china_4factor_10y_FF$p.value >= 0.05)

#### Checking Relationship Between Fund Characteristics and Fund Alphas

fund_alphas_china_4factor_10y_FF$alphadecile <-
  ntile(fund_alphas_china_4factor_10y_FF$estimate, 10)

China_Panel_Ind <- China_Panel_Ind  %>% 
  left_join(fund_alphas_china_4factor_10y_FF, by = "FundID")


China_Panel_Ind <- China_Panel_Ind %>% 
  mutate(Age = as.yearmon(strptime("31.12.2019", format = "%d.%m.%Y"))-
                 as.yearmon(strptime(`Inception Date`, format = "%Y-%m-%d")))

China_Panel_Ind <- China_Panel_Ind %>% 
  group_by(alphadecile) %>% 
  mutate(AverageFundAge = mean(Age, na.rm = TRUE)) %>% 
  mutate(AverageFundSize = mean(FundSize, na.rm = TRUE)) %>% 
  mutate(AverageExpenseRatio = mean(`Annual Report Net Expense Ratio Year2019`, na.rm = TRUE))

China_Panel_Ind <- China_Panel_Ind %>% 
  mutate(LNSize = log(FundSize))

China_Panel_Ind %>% 
  group_by(alphadecile) %>% 
  ggplot(aes(x = alphadecile, y = AverageFundSize)) +
  geom_line()

China_Panel_Ind %>% 
  group_by(alphadecile) %>% 
  ggplot(aes(x = alphadecile, y = AverageFundAge)) +
  geom_line()

China_Panel_Ind %>% 
  group_by(alphadecile) %>% 
  ggplot(aes(x = alphadecile, y = AverageExpenseRatio)) +
  geom_line()

model_alphas_china_age <- lm(estimate ~ Age, data = China_Panel_Ind)
model_alphas_china_size <- lm(estimate ~ LNSize, data = China_Panel_Ind)
model_alphas_china_expenseratio <- lm(estimate ~ `Annual Report Net Expense Ratio Year2019`, data = China_Panel_Ind)
model_alphas_china_all <- lm(estimate ~ Age + LNSize + `Annual Report Net Expense Ratio Year2019`, data = China_Panel_Ind)



#### Descriptive Statistics

### Number of funds

Full_Equity_Panel_PerYear <- Full_Equity_Panel %>%
    filter(month == 12) %>% 
    group_by(date1, `Investment Area`) 

Full_Equity_Panel_PerYear %>% 
  count() %>% 
  ggplot(aes(x= date1, y= n, color= `Investment Area`)) + 
  geom_line(size=1) 

Full_Equity_Panel_PerYear %>% 
  summarize(sumAmount = sum(FundSize, na.rm = TRUE)) %>% 
  mutate(`AuM in BN EUR` = sumAmount / 1000000000 ) %>% 
  ggplot(aes(x= date1, y=`AuM in BN EUR`, color= `Investment Area`)) + 
  geom_line(size=1) 


#### Appendix

# 
# Full_Equity_Panel <- Full_Equity_Panel %>% 
#   mutate(Age = as.yearmon(strptime("31.12.2019", format = "%d.%m.%Y"))-
#            as.yearmon(strptime(`Inception Date`, format = "%Y-%m-%d"))) 
# 
# Descriptiv <- Full_Equity_Panel %>% 
#   group_by(`Investment Area`, year) %>% 
#   summarise(MeanAge = mean(Age, na.rm = TRUE),
#             MaxAge = max(Age, na.rm = TRUE),
#             MinAge = min(Age, na.rm = TRUE),
#             SDAge = sd(Age, na.rm = TRUE),
#             MeanReturn = mean(MonthlyReturn, na.rm = TRUE),
#             MaxReturn = max(MonthlyReturn, na.rm = TRUE),
#             MinReturn = min(MonthlyReturn, na.rm = TRUE),
#             SDReturn = sd(MonthlyReturn, na.rm = TRUE),
#             MeanSize = mean(FundSize, na.rm = TRUE),
#             MaxSize = max(FundSize, na.rm = TRUE),
#             MinSize = min(FundSize, na.rm = TRUE),
#             SDSize = sd(FundSize, na.rm = TRUE)) 
# 
# dcast(Descriptiv, Area + Time + Benchmark ~ `Investment Area`, value.var = "estimate")
# 
# ### Extract ISIN List of best performing Chinese Funds
# 
# ISINGOODALPHAS <- China_Panel_Ind %>% 
#   filter(alphadecile == 10) %>% 
#   distinct(ISIN, keep_all = FALSE) %>% 
#   select(ISIN)
# 
# 
# 
# #### Table with all Alphas
# list <- ls(pattern = "model")
# list2 <- mget(list)[-1]
# alphalist <- lapply(list2, tidy) %>% 
#   bind_rows(.id = "column_labels") %>% 
#   filter(term == "(Intercept)") 
#   
# 
# alphalist <- alphalist %>% 
#   separate(column_labels, c("Model", "Area", "Factor", "Time", "Benchmark"), "_")

# alphalist_wide <- dcast(alphalist, Area + Time + Benchmark ~ Factor, value.var = "estimate") %>%
#   arrange(Area, Benchmark, Time)

Factor_Chart <- Emerging_Factors %>%
  mutate(HML = cumprod(1+(HML/100))-1) %>%
  mutate(SMB = cumprod(1+(SMB/100))-1) %>%
  mutate(RMW = cumprod(1+(RMW/100))-1) %>%
  mutate(CMA = cumprod(1+(RMW/100))-1) %>%
  mutate(WML = cumprod(1+(WML/100))-1)

Factor_Chart <- melt(Factor_Chart, id.vars = "date1", measure.vars = c("HML", "SMB", "RMW", "CMA", "WML"), na.rm = FALSE, value.name = "value")
Factor_Chart %>%
  ggplot(aes(x = date1, y = value, color = variable)) +
  geom_line(size=1)

Benchmark_Chart <- Benchmarks_Factors %>%
  filter(date1 > as.Date("2010-02-28")) %>% 
  mutate(ChinaSOE = cumprod(1+(CSIStateownedEnterprisesCompPRCNY/100))-1) %>%
  mutate(China = cumprod(1+(CSI300NRUSD/100))-1) %>%
  mutate(EM = cumprod(1+(MSCIEMAsiaNRUSD/100))-1) %>%
  mutate(India = cumprod(1+(IISLNifty50TRINR/100))-1) %>%
  mutate(AsiaPacExJPN = cumprod(1+(MSCIACAsiaPacExJPNNRUSD/100))-1)

Benchmark_Chart <- melt(Benchmark_Chart, id.vars = "date1", measure.vars = c("ChinaSOE", "China", "EM", "India", "AsiaPacExJPN"), na.rm = FALSE, value.name = "value")
Benchmark_Chart %>%
  ggplot(aes(x = date1, y = value, color = variable)) +
  geom_line(size=1)
#### Extract Top 10% and Low 10% of Chinese Funds versus CSI300 over the last three

China_Panel_Ind_3y <- subset(China_Panel, date1 > as.Date("2017-01-01"))
China_Panel_Ind_3y <- China_Panel_Ind_3y[as.numeric(ave(China_Panel_Ind_3y$FundID, China_Panel_Ind_3y$FundID, FUN=length)) >= 24, ] 

China_Panel_Ind_4factor_3y_CSI300 <- China_Panel_Ind_3y %>% 
  group_by(FundID) %>%
  do(formula_4factor_CSI300 = lm(Funds.RF ~ CSI300NRUSD.RF + SMB + HML + WML , data = .))

fund_alphas_china_4factor_3y_CSI300 <- tidy(China_Panel_Ind_4factor_3y_CSI300, formula_4factor_CSI300) %>% 
  filter(term == "(Intercept)") %>% 
  distinct(ISIN, .keep_all = TRUE)

fund_alphas_china_4factor_3y_CSI300$alphadecile <-
  ntile(fund_alphas_china_4factor_3y_CSI300$estimate, 10)

fund_alphas_china_4factor_3y_CSI300 %>% 
  group_by(alphadecile) %>% 
  count()

China_Panel_Ind_3y <- China_Panel_Ind_3y  %>% 
  left_join(fund_alphas_china_4factor_3y_CSI300, by = "FundID")

ISINGOODALPHAS_3y <- China_Panel_Ind_3y %>%
  filter(alphadecile == 10) %>%
  distinct(FundID, .keep_all = TRUE) %>% 
  select(ISIN, estimate)

ISINBADALPHAS_3y <- China_Panel_Ind_3y %>%
  filter(alphadecile == 1) %>%
  distinct(FundID, .keep_all = TRUE) %>% 
  select(ISIN, estimate)



##### Holding-Level-Analysis

HOLDINGS <- read_excel("HOLDINGS.xlsx")
INSTOWNER <- read_excel("INSTOWNER.xlsm")
SECTORSOE <- read_excel("SectorSOE.xlsm")
CSI300 <- read_excel("CSI300.xlsx")
ANALYST <- read_excel("ANALYST.xlsm")

ANALYST <- melt(ANALYST, 
     id.vars = "date1", 
     measure.vars= c(2:1207),
     variable.name = "RIC", value.name  = "NumberAnalyst") %>% 
     filter(NumberAnalyst != "#N/A Invalid Security") %>% 
     filter(NumberAnalyst != "<NA>") %>% 
     filter(NumberAnalyst != "#N/A N/A") %>% 
     mutate(Quarter = as.yearqtr(date1, format = "%m-%d-%Y")) %>% 
     arrange(RIC, Quarter)

INSTOWNER <-  melt(INSTOWNER, 
              id.vars = "date1", 
              measure.vars= c(2:1207),
              variable.name = "RIC", value.name  = "INSTOWN")%>% 
              filter(INSTOWN != "#N/A Invalid Security") %>% 
              filter(INSTOWN != "<NA>") %>% 
              filter(INSTOWN != "#N/A N/A") %>% 
              mutate(Quarter = as.yearqtr(date1, format = "%m-%d-%Y"))


HOLDING_FULL <- INSTOWNER %>% 
  left_join(ANALYST, by = c("RIC", "Quarter")) 

Test <- HOLDING_FULL %>% 
  group_by(Quarter, RIC) %>% 
  count() %>% 
  filter(n > 1)


HOLDINGS <- HOLDINGS %>%   
  mutate(Quarter = as.yearqtr(Date, format = "%m-%d-%Y")) %>% 
  mutate(date1 = as.Date(Date, format = "%m-%d-%Y")) %>% 
  arrange(ISIN, Quarter, RIC) %>% 
  filter(RIC != "NA")


HOLDINGS <- HOLDINGS %>% 
  inner_join(HOLDING_FULL, by = c("RIC", "Quarter")) %>% 
  inner_join(SECTORSOE, by = "RIC")

HOLDINGSTUDY <- China_Panel_Ind_3y %>%
  filter(ISIN %in% HOLDINGS$ISIN) %>% 
  select(ISIN, EquityStyle, FundSize, year, MonthlyReturn, date1, CSI300NRUSD, RF, SMB, HML, WML, estimate, alphadecile) %>% 
  mutate(Quarter = as.yearqtr(date1) - 0.25)

HOLDINGSTUDYFULL <- HOLDINGSTUDY %>% 
  left_join(HOLDINGS, by = c("ISIN", "Quarter")) %>% 
  group_by(ISIN, date1.x.x) %>% 
  mutate(ScaledWeights = Weight/sum(Weight)) 

HOLDINGSTUDYFULL <- HOLDINGSTUDYFULL %>% 
  mutate(SOEDummy = if_else(SOE == "Sovereign" | SOE == "Regional", 1, 0))

HOLDINGSTUDYFULL <- HOLDINGSTUDYFULL %>% 
  mutate(ANALYSTCONTR = ScaledWeights * as.numeric(NumberAnalyst)) %>% 
  mutate(INSTICONTR = ScaledWeights * as.numeric(INSTOWN))

HOLDINGSSTUDYFULL <- HOLDINGSTUDYFULL %>% 
  group_by(ISIN, date1.x.x) %>% 
  mutate(SOEWeight= sum(SOEDummy*ScaledWeights, na.rm = TRUE)) %>% 
  mutate(ANALYSTFUND = sum(ANALYSTCONTR, na.rm = TRUE)) %>% 
  mutate(INSTIFUND = sum(INSTICONTR, na.rm = TRUE)) %>% 
  mutate(TOP = if_else(alphadecile == 10, 1, 0)) %>% 
  ungroup()

HOLDINGS_REGRESSION <- HOLDINGSSTUDYFULL %>% 
  distinct(ISIN, date1.x.x, .keep_all = TRUE) 

HOLDINGS_REGRESSION <- HOLDINGS_REGRESSION %>% 
  group_by(ISIN, year) %>%  
  mutate(SOEWeight = mean(SOEWeight, na.rm = TRUE)) %>% 
  mutate(ANALYSTFUND = mean(ANALYSTFUND, na.rm = TRUE)) %>% 
  mutate(INSTIFUND = mean(INSTIFUND, na.rm = TRUE))
  
model_probit_SOE <- glm(TOP ~ SOEWeight,family = binomial(link = "probit"), data = HOLDINGS_REGRESSION)
model_probit_ANALYST <- glm(TOP ~ ANALYSTFUND, family = binomial(link = "probit"), data = HOLDINGS_REGRESSION)
model_probit_INSTI <- glm(TOP ~ INSTIFUND, family = binomial(link = "probit"), data = HOLDINGS_REGRESSION)
model_probit_full <-  glm(TOP ~ SOEWeight + ANALYSTFUND + INSTIFUND, family = binomial(link = "probit"), data = HOLDINGS_REGRESSION)

model_ols_SOE <- lm(estimate ~ SOEWeight, data = HOLDINGS_REGRESSION)
model_ols_ANALYST <- lm(estimate ~ ANALYSTFUND, data = HOLDINGS_REGRESSION)
model_ols_INSTI <- lm(estimate ~ INSTIFUND, data = HOLDINGS_REGRESSION)
model_ols_full <- lm(estimate ~ SOEWeight + ANALYSTFUND + INSTIFUND, data = HOLDINGS_REGRESSION)



HOLDINGS_REGRESSION %>% 
  ggplot(aes(x = SOEWeight, y = estimate)) +
  geom_point(aes(x = SOEWeight, y = estimate)) +
  geom_smooth(method = "lm")

HOLDINGS_REGRESSION %>% 
  ggplot(aes(x = ANALYSTFUND, y = estimate)) +
  geom_point(aes(x = ANALYSTFUND, y = estimate)) +
  geom_smooth(method = "lm")

HOLDINGS_REGRESSION %>% 
  ggplot(aes(x = INSTIFUND, y = estimate)) +
  geom_point(aes(x = INSTIFUND, y = estimate)) +
  geom_smooth(method = "lm")

library(AER)
coeftest(model_probit_SOE, vcov. = vcovHC, type = "HC1")
coeftest(model_probit_ANALYST,  vcov. = vcovHC, type = "HC1")
coeftest(model_probit_INSTI, vcov. = vcovHC, type = "HC1")
coeftest(model_probit_full, vcov. = vcovHC, type = "HC1")


##### China All Caps

#+ results='asis'
htmlreg(
  list(
    model_china_1factor_10y_FF,
    model_china_3factor_10y_FF,
    model_china_4factor_10y_FF,
    model_china_5factor_10y_FF 
  ),
  include.ci = FALSE, 
  caption = "Chinese Funds versus Fama French EM Benchmark 2010-2019",
  doctype = FALSE,
  caption.above = TRUE)

htmlreg(
  list(
    model_china_1factor_5y_FF,
    model_china_3factor_5y_FF,
    model_china_4factor_5y_FF,
    model_china_5factor_5y_FF 
  ),
  include.ci = FALSE, 
  caption = "Chinese Funds versus Fama French EM Benchmark 2015-2019",
  doctype = FALSE,
  caption.above = TRUE)

htmlreg(
  list(
    model_china_1factor_3y_FF,
    model_china_3factor_3y_FF,
    model_china_4factor_3y_FF,
    model_china_5factor_3y_FF 
  ),
  include.ci = FALSE, 
  caption = "Chinese Funds versus Fama French EM Benchmark 2017-2019",
  doctype = FALSE,
  caption.above = TRUE)

#+ results='asis'
htmlreg(
  list(
    model_china_1factor_10y_CSI300,
    model_china_3factor_10y_CSI300,
    model_china_4factor_10y_CSI300,
    model_china_5factor_10y_CSI300 
  ),
  include.ci = FALSE, 
  caption = "Chinese Funds versus CSI 300, 2010-2019",
  doctype = FALSE,
  caption.above = TRUE)

#+ results='asis'
htmlreg(
  list(
    model_china_1factor_5y_CSI300,
    model_china_3factor_5y_CSI300,
    model_china_4factor_5y_CSI300,
    model_china_5factor_5y_CSI300 
  ),
  include.ci = FALSE, 
  caption = "Chinese Funds versus CSI 300, 2015-2019",
  doctype = FALSE,
  caption.above = TRUE)

#+ results='asis'
htmlreg(
  list(
    model_china_1factor_3y_CSI300,
    model_china_3factor_3y_CSI300,
    model_china_4factor_3y_CSI300,
    model_china_5factor_3y_CSI300 
  ),
  include.ci = FALSE, 
  caption = "Chinese Funds versus CSI 300, 2017-2019",
  doctype = FALSE,
  caption.above = TRUE)

#+ results='asis'
htmlreg(
  list(
    model_china_1factor_10y_MSCIChina,
    model_china_3factor_10y_MSCIChina,
    model_china_4factor_10y_MSCIChina,
    model_china_5factor_10y_MSCIChina 
  ),
  include.ci = FALSE, 
  caption = "Chinese Funds versus MSCI China, 2010-2019",
  doctype = FALSE,
  caption.above = TRUE)

#+ results='asis'
htmlreg(
  list(
    model_china_1factor_5y_MSCIChina,
    model_china_3factor_5y_MSCIChina,
    model_china_4factor_5y_MSCIChina,
    model_china_5factor_5y_MSCIChina 
  ),
  include.ci = FALSE, 
  caption = "Chinese Funds versus MSCI China, 2015-2019",
  doctype = FALSE,
  caption.above = TRUE)

#+ results='asis'
htmlreg(
  list(
    model_china_1factor_3y_MSCIChina,
    model_china_3factor_3y_MSCIChina,
    model_china_4factor_3y_MSCIChina,
    model_china_5factor_3y_MSCIChina 
  ),
  include.ci = FALSE, 
  caption = "Chinese Funds versus MSCI China, 2017-2019",
  doctype = FALSE,
  caption.above = TRUE)

#+ results='asis'
htmlreg(
  list(
    model_china_1factor_10y_CSISOE,
    model_china_3factor_10y_CSISOE,
    model_china_4factor_10y_CSISOE,
    model_china_5factor_10y_CSISOE 
  ),
  include.ci = FALSE, 
  caption = "Chinese Funds versus CSI Stateowned Enterprises Comp, 2010-2019",
  doctype = FALSE,
  caption.above = TRUE)

#+ results='asis'
htmlreg(
  list(
    model_china_1factor_10y_CSISOE,
    model_china_3factor_10y_CSISOE,
    model_china_4factor_10y_CSISOE,
    model_china_5factor_10y_CSISOE 
  ),
  include.ci = FALSE, 
  caption = "Chinese Funds versus CSI Stateowned Enterprises Comp, 2015-2019",
  doctype = FALSE,
  caption.above = TRUE)

#+ results='asis'
htmlreg(
  list(
    model_china_1factor_3y_CSISOE,
    model_china_3factor_3y_CSISOE,
    model_china_4factor_3y_CSISOE,
    model_china_5factor_3y_CSISOE 
  ),
  include.ci = FALSE, 
  caption = "Chinese Funds versus CSI Stateowned Enterprises Comp, 2017-2019",
  doctype = FALSE,
  caption.above = TRUE)

##### Asia Pacific ex Japan All Caps

#+ results='asis'
htmlreg(
  list(
    model_asiapacexjpn_1factor_10y_FF,
    model_asiapacexjpn_3factor_10y_FF,
    model_asiapacexjpn_4factor_10y_FF,
    model_asiapacexjpn_5factor_10y_FF 
  ),
  include.ci = FALSE, 
  caption = "Asian Pacfic ex Japan versus Fama French EM Benchmark, 2010-2019",
  doctype = FALSE,
  caption.above = TRUE)

htmlreg(
  list(
    model_asiapacexjpn_1factor_5y_FF,
    model_asiapacexjpn_3factor_5y_FF,
    model_asiapacexjpn_4factor_5y_FF,
    model_asiapacexjpn_5factor_5y_FF 
  ),
  include.ci = FALSE, 
  caption = "Asian Pacfic ex Japan versus Fama French EM Benchmark, 2015-2019",
  doctype = FALSE,
  caption.above = TRUE)

htmlreg(
  list(
    model_asiapacexjpn_1factor_3y_FF,
    model_asiapacexjpn_3factor_3y_FF,
    model_asiapacexjpn_4factor_3y_FF,
    model_asiapacexjpn_5factor_3y_FF 
  ),
  include.ci = FALSE, 
  caption = "Asian Pacfic ex Japan versus Fama French EM Benchmark, 2017-2019",
  doctype = FALSE,
  caption.above = TRUE)

#+ results='asis'
htmlreg(
  list(
    model_asiapacexjpn_1factor_10y_CSI300,
    model_asiapacexjpn_3factor_10y_CSI300,
    model_asiapacexjpn_4factor_10y_CSI300,
    model_asiapacexjpn_5factor_10y_CSI300 
  ),
  include.ci = FALSE, 
  caption = "Asian Pacfic ex Japan versus CSI 300, 2010-2019",
  doctype = FALSE,
  caption.above = TRUE)

htmlreg(
  list(
    model_asiapacexjpn_1factor_5y_CSI300,
    model_asiapacexjpn_3factor_5y_CSI300,
    model_asiapacexjpn_4factor_5y_CSI300,
    model_asiapacexjpn_5factor_5y_CSI300 
  ),
  include.ci = FALSE, 
  caption = "Asian Pacfic ex Japan versus CSI 300, 2015-2019",
  doctype = FALSE,
  caption.above = TRUE)

htmlreg(
  list(
    model_asiapacexjpn_1factor_3y_CSI300,
    model_asiapacexjpn_3factor_3y_CSI300,
    model_asiapacexjpn_4factor_3y_CSI300,
    model_asiapacexjpn_5factor_3y_CSI300 
  ),
  include.ci = FALSE, 
  caption = "Asian Pacfic ex Japan versus CSI 300, 2017-2019",
  doctype = FALSE,
  caption.above = TRUE)

#+ results='asis'
htmlreg(
  list(
    model_asiapacexjpn_1factor_10y_MSCIAsiaPacExJpn,
    model_asiapacexjpn_3factor_10y_MSCIAsiaPacExJpn,
    model_asiapacexjpn_4factor_10y_MSCIAsiaPacExJpn,
    model_asiapacexjpn_5factor_10y_MSCIAsiaPacExJpn 
  ),
  include.ci = FALSE, 
  caption = "Asian Pacfic ex Japan versus MSCI Asia Pacific ex Japan, 2010-2019",
  doctype = FALSE,
  caption.above = TRUE)

htmlreg(
  list(
    model_asiapacexjpn_1factor_5y_MSCIAsiaPacExJpn,
    model_asiapacexjpn_3factor_5y_MSCIAsiaPacExJpn,
    model_asiapacexjpn_4factor_5y_MSCIAsiaPacExJpn,
    model_asiapacexjpn_5factor_5y_MSCIAsiaPacExJpn 
  ),
  include.ci = FALSE, 
  caption = "Asian Pacfic ex Japan versus MSCI Asia Pacific ex Japan, 2015-2019",
  doctype = FALSE,
  caption.above = TRUE)

htmlreg(
  list(
    model_asiapacexjpn_1factor_3y_MSCIAsiaPacExJpn,
    model_asiapacexjpn_3factor_3y_MSCIAsiaPacExJpn,
    model_asiapacexjpn_4factor_3y_MSCIAsiaPacExJpn,
    model_asiapacexjpn_5factor_3y_MSCIAsiaPacExJpn 
  ),
  include.ci = FALSE, 
  caption = "Asian Pacfic ex Japan versus MSCI Asia Pacific ex Japan, 2017-2019",
  doctype = FALSE,
  caption.above = TRUE)

#+ results='asis'
htmlreg(
  list(
    model_asiapacexjpn_1factor_10y_CSISOE,
    model_asiapacexjpn_3factor_10y_CSISOE,
    model_asiapacexjpn_4factor_10y_CSISOE,
    model_asiapacexjpn_5factor_10y_CSISOE 
  ),
  include.ci = FALSE, 
  caption = "Asian Pacfic ex Japan Funds versus CSI Stateowned Enterprises Comp, 2010-2019",
  doctype = FALSE,
  caption.above = TRUE)

htmlreg(
  list(
    model_asiapacexjpn_1factor_5y_CSISOE,
    model_asiapacexjpn_3factor_5y_CSISOE,
    model_asiapacexjpn_4factor_5y_CSISOE,
    model_asiapacexjpn_5factor_5y_CSISOE 
  ),
  include.ci = FALSE, 
  caption = "Asian Pacfic ex Japan Funds versus CSI Stateowned Enterprises Comp, 2015-2019",
  doctype = FALSE,
  caption.above = TRUE)

htmlreg(
  list(
    model_asiapacexjpn_1factor_3y_CSISOE,
    model_asiapacexjpn_3factor_3y_CSISOE,
    model_asiapacexjpn_4factor_3y_CSISOE,
    model_asiapacexjpn_5factor_3y_CSISOE 
  ),
  include.ci = FALSE, 
  caption = "Asian Pacfic ex Japan Funds versus CSI Stateowned Enterprises Comp, 2017-2019",
  doctype = FALSE,
  caption.above = TRUE)


#### India All Caps

#+ results='asis'
htmlreg(
  list(
    model_india_1factor_10y_FF,
    model_india_3factor_10y_FF,
    model_india_4factor_10y_FF,
    model_india_5factor_10y_FF
  ),
  include.ci = FALSE, 
  caption = "India versus Fama French EM Benchmark, 2010-2019",
  doctype = FALSE,
  caption.above = TRUE)

#+ results='asis'
htmlreg(
  list(
    model_india_1factor_5y_FF,
    model_india_3factor_5y_FF,
    model_india_4factor_5y_FF,
    model_india_5factor_5y_FF
  ),
  include.ci = FALSE, 
  caption = "India versus Fama French EM Benchmark, 2015-2019",
  doctype = FALSE,
  caption.above = TRUE)

#+ results='asis'
htmlreg(
  list(
    model_india_1factor_3y_FF,
    model_india_3factor_3y_FF,
    model_india_4factor_3y_FF,
    model_india_5factor_3y_FF
  ),
  include.ci = FALSE, 
  caption = "India versus Fama French EM Benchmark, 2017-2019",
  doctype = FALSE,
  caption.above = TRUE)

#+ results='asis'
htmlreg(
  list(
    model_india_1factor_10y_IISLNifty50,
    model_india_3factor_10y_IISLNifty50,
    model_india_4factor_10y_IISLNifty50,
    model_india_5factor_10y_IISLNifty50
  ),
  include.ci = FALSE, 
  caption = "India versus Nifty 50, 2010-2019",
  doctype = FALSE,
  caption.above = TRUE)

#+ results='asis'
htmlreg(
  list(
    model_india_1factor_5y_IISLNifty50,
    model_india_3factor_5y_IISLNifty50,
    model_india_4factor_5y_IISLNifty50,
    model_india_5factor_5y_IISLNifty50
  ),
  include.ci = FALSE, 
  caption = "India versus Nifty 50, 2015-2019",
  doctype = FALSE,
  caption.above = TRUE)

#+ results='asis'
htmlreg(
  list(
    model_india_1factor_3y_IISLNifty50,
    model_india_3factor_3y_IISLNifty50,
    model_india_4factor_3y_IISLNifty50,
    model_india_5factor_3y_IISLNifty50
  ),
  include.ci = FALSE, 
  caption = "India versus Nifty 50, 2017-2019",
  doctype = FALSE,
  caption.above = TRUE)

#+ results='asis'
htmlreg(
  list(
    model_india_1factor_10y_MSCIIndia,
    model_india_3factor_10y_MSCIIndia,
    model_india_4factor_10y_MSCIIndia,
    model_india_5factor_10y_MSCIIndia
  ),
  include.ci = FALSE, 
  caption = "India versus MSCI India, 2010-2019",
  doctype = FALSE,
  caption.above = TRUE)

#+ results='asis'
htmlreg(
  list(
    model_india_1factor_5y_MSCIIndia,
    model_india_3factor_5y_MSCIIndia,
    model_india_4factor_5y_MSCIIndia,
    model_india_5factor_5y_MSCIIndia
  ),
  include.ci = FALSE, 
  caption = "India versus MSCI India, 2015-2019",
  doctype = FALSE,
  caption.above = TRUE)

#+ results='asis'
htmlreg(
  list(
    model_india_1factor_3y_MSCIIndia,
    model_india_3factor_3y_MSCIIndia,
    model_india_4factor_3y_MSCIIndia,
    model_india_5factor_3y_MSCIIndia
  ),
  include.ci = FALSE, 
  caption = "India versus MSCI India, 2017-2019",
  doctype = FALSE,
  caption.above = TRUE)

#+ results='asis'
htmlreg(
  list(
    model_india_1factor_10y_SPBSE500India,
    model_india_3factor_10y_SPBSE500India,
    model_india_4factor_10y_SPBSE500India,
    model_india_5factor_10y_SPBSE500India
  ),
  include.ci = FALSE, 
  caption = "India versus S&P BSE 500 India, 2010-2019",
  doctype = FALSE,
  caption.above = TRUE)

#+ results='asis'
htmlreg(
  list(
    model_india_1factor_5y_SPBSE500India,
    model_india_3factor_5y_SPBSE500India,
    model_india_4factor_5y_SPBSE500India,
    model_india_5factor_5y_SPBSE500India
  ),
  include.ci = FALSE, 
  caption = "India versus S&P BSE 500 India, 2015-2019",
  doctype = FALSE,
  caption.above = TRUE)

#+ results='asis'
htmlreg(
  list(
    model_india_1factor_3y_SPBSE500India,
    model_india_3factor_3y_SPBSE500India,
    model_india_4factor_3y_SPBSE500India,
    model_india_5factor_3y_SPBSE500India
  ),
  include.ci = FALSE, 
  caption = "India versus S&P BSE 500 India, 2017-2019",
  doctype = FALSE,
  caption.above = TRUE)



#### Asia EM All Caps

#+ results='asis'
htmlreg(
  list(
    model_asiaemerg_1factor_10y_FF,
    model_asiaemerg_3factor_10y_FF,
    model_asiaemerg_4factor_10y_FF,
    model_asiaemerg_5factor_10y_FF
  ),
  include.ci = FALSE, 
  caption = "Emerging Markets in Asia versus Fama French EM Benchmark, 2010-2019",
  doctype = FALSE,
  caption.above = TRUE)

#+ results='asis'
htmlreg(
  list(
    model_asiaemerg_1factor_5y_FF,
    model_asiaemerg_3factor_5y_FF,
    model_asiaemerg_4factor_5y_FF,
    model_asiaemerg_5factor_5y_FF
  ),
  include.ci = FALSE, 
  caption = "Emerging Markets in Asia versus Fama French EM Benchmark, 2015-2019",
  doctype = FALSE,
  caption.above = TRUE)

#+ results='asis'
htmlreg(
  list(
    model_asiaemerg_1factor_3y_FF,
    model_asiaemerg_3factor_3y_FF,
    model_asiaemerg_4factor_3y_FF,
    model_asiaemerg_5factor_3y_FF
  ),
  include.ci = FALSE, 
  caption = "Emerging Markets in Asia versus Fama French EM Benchmark, 2017-2019",
  doctype = FALSE,
  caption.above = TRUE)

#+ results='asis'
htmlreg(
  list(
    model_asiaemerg_1factor_10y_MSCIEMAsia1040,
    model_asiaemerg_3factor_10y_MSCIEMAsia1040,
    model_asiaemerg_4factor_10y_MSCIEMAsia1040,
    model_asiaemerg_5factor_10y_MSCIEMAsia1040
  ),
  include.ci = FALSE, 
  caption = "Emerging Markets in Asia versus MSCI Asia EM 1040, 2010-2019",
  doctype = FALSE,
  caption.above = TRUE)

#+ results='asis'
htmlreg(
  list(
    model_asiaemerg_1factor_5y_MSCIEMAsia1040,
    model_asiaemerg_3factor_5y_MSCIEMAsia1040,
    model_asiaemerg_4factor_5y_MSCIEMAsia1040,
    model_asiaemerg_5factor_5y_MSCIEMAsia1040
  ),
  include.ci = FALSE, 
  caption = "Emerging Markets in Asia versus MSCI Asia EM 1040, 2015-2019",
  doctype = FALSE,
  caption.above = TRUE)

#+ results='asis'
htmlreg(
  list(
    model_asiaemerg_1factor_3y_MSCIEMAsia1040,
    model_asiaemerg_3factor_3y_MSCIEMAsia1040,
    model_asiaemerg_4factor_3y_MSCIEMAsia1040,
    model_asiaemerg_5factor_3y_MSCIEMAsia1040
  ),
  include.ci = FALSE, 
  caption = "Emerging Markets in Asia versus MSCI Asia EM 1040, 2017-2019",
  doctype = FALSE,
  caption.above = TRUE)

#+ results='asis'
htmlreg(
  list(
    model_asiaemerg_1factor_10y_MSCIEMAsia,
    model_asiaemerg_3factor_10y_MSCIEMAsia,
    model_asiaemerg_4factor_10y_MSCIEMAsia,
    model_asiaemerg_5factor_10y_MSCIEMAsia
  ),
  include.ci = FALSE, 
  caption = "Emerging Markets in Asia versus MSCI Asia EM, 2010-2019",
  doctype = FALSE,
  caption.above = TRUE)

#+ results='asis'
htmlreg(
  list(
    model_asiaemerg_1factor_5y_MSCIEMAsia,
    model_asiaemerg_3factor_5y_MSCIEMAsia,
    model_asiaemerg_4factor_5y_MSCIEMAsia,
    model_asiaemerg_5factor_5y_MSCIEMAsia
  ),
  include.ci = FALSE, 
  caption = "Emerging Markets in Asia versus MSCI Asia EM, 2015-2019",
  doctype = FALSE,
  caption.above = TRUE)

#+ results='asis'
htmlreg(
  list(
    model_asiaemerg_1factor_3y_MSCIEMAsia,
    model_asiaemerg_3factor_3y_MSCIEMAsia,
    model_asiaemerg_4factor_3y_MSCIEMAsia,
    model_asiaemerg_5factor_3y_MSCIEMAsia
  ),
  include.ci = FALSE, 
  caption = "Emerging Markets in Asia versus MSCI Asia EM, 2017-2019",
  doctype = FALSE,
  caption.above = TRUE)

#+ results='asis'
htmlreg(
  list(
    model_asiaemerg_1factor_10y_CSIStateownedEnterprisesComp,
    model_asiaemerg_3factor_10y_CSIStateownedEnterprisesComp,
    model_asiaemerg_4factor_10y_CSIStateownedEnterprisesComp,
    model_asiaemerg_5factor_10y_CSIStateownedEnterprisesComp
  ),
  include.ci = FALSE, 
  caption = "Emerging Markets in Asia versus CSI Stateowned Enterprises Comp, 2010-2019",
  doctype = FALSE,
  caption.above = TRUE)

#+ results='asis'
htmlreg(
  list(
    model_asiaemerg_1factor_5y_CSIStateownedEnterprisesComp,
    model_asiaemerg_3factor_5y_CSIStateownedEnterprisesComp,
    model_asiaemerg_4factor_5y_CSIStateownedEnterprisesComp,
    model_asiaemerg_5factor_5y_CSIStateownedEnterprisesComp
  ),
  include.ci = FALSE, 
  caption = "Emerging Markets in Asia versus CSI Stateowned Enterprises Comp, 2015-2019",
  doctype = FALSE,
  caption.above = TRUE)

#+ results='asis'
htmlreg(
  list(
    model_asiaemerg_1factor_3y_CSIStateownedEnterprisesComp,
    model_asiaemerg_3factor_3y_CSIStateownedEnterprisesComp,
    model_asiaemerg_4factor_3y_CSIStateownedEnterprisesComp,
    model_asiaemerg_5factor_3y_CSIStateownedEnterprisesComp
  ),
  include.ci = FALSE, 
  caption = "Emerging Markets in Asia versus CSI Stateowned Enterprises Comp, 2017-2019",
  doctype = FALSE,
  caption.above = TRUE)


#### India Mid Cap

#+ results='asis'
htmlreg(
  list(
    model_india_mid_1factor_10y_FF,
    model_india_mid_3factor_10y_FF,
    model_india_mid_4factor_10y_FF,
    model_india_mid_5factor_10y_FF
  ),
  include.ci = FALSE, 
  caption = "Indian Mid Cap Funds versus Fama French Benchmark, 2010-2019",
  doctype = FALSE,
  caption.above = TRUE)

#+ results='asis'
htmlreg(
  list(
    model_india_mid_1factor_10y_MSCIIndiaMid,
    model_india_mid_3factor_10y_MSCIIndiaMid,
    model_india_mid_4factor_10y_MSCIIndiaMid,
    model_india_mid_5factor_10y_MSCIIndiaMid
  ),
  include.ci = FALSE, 
  caption = "Indian Mid Cap Funds versus MSCI India Mid Cap, 2010-2019",
  doctype = FALSE,
  caption.above = TRUE)

#+ results='asis'
htmlreg(
  list(
    model_india_mid_1factor_10y_IISLNiftyMidcap150,
    model_india_mid_3factor_10y_IISLNiftyMidcap150,
    model_india_mid_4factor_10y_IISLNiftyMidcap150,
    model_india_mid_5factor_10y_IISLNiftyMidcap150
  ),
  include.ci = FALSE, 
  caption = "Indian Mid Cap Funds versus Nifty Mid Cap 150, 2010-2019",
  doctype = FALSE,
  caption.above = TRUE)

#+ results='asis'
htmlreg(
  list(
    model_india_mid_1factor_10y_SPBSE500India,
    model_india_mid_3factor_10y_SPBSE500India,
    model_india_mid_4factor_10y_SPBSE500India,
    model_india_mid_5factor_10y_SPBSE500India
  ),
  include.ci = FALSE, 
  caption = "Indian Mid Cap Funds versus S&P BSE 500 India, 2010-2019",
  doctype = FALSE,
  caption.above = TRUE)

#### India Small Caps

#+ results='asis'
htmlreg(
  list(
    model_india_small_1factor_10y_FF,
    model_india_small_3factor_10y_FF,
    model_india_small_4factor_10y_FF,
    model_india_small_5factor_10y_FF
  ),
  include.ci = FALSE, 
  caption = "Indian Small Cap Funds versus Fama French Benchmark, 2010-2019",
  doctype = FALSE,
  caption.above = TRUE)

#+ results='asis'
htmlreg(
  list(
    model_india_small_1factor_10y_MSCIIndiaSmall,
    model_india_small_3factor_10y_MSCIIndiaSmall,
    model_india_small_4factor_10y_MSCIIndiaSmall,
    model_india_small_5factor_10y_MSCIIndiaSmall
  ),
  include.ci = FALSE, 
  caption = "Indian Small Cap Funds versus MSCI India Small Cap, 2010-2019",
  doctype = FALSE,
  caption.above = TRUE)

#+ results='asis'
htmlreg(
  list(
    model_india_small_1factor_10y_IISLNiftySmallcap250,
    model_india_small_3factor_10y_IISLNiftySmallcap250,
    model_india_small_4factor_10y_IISLNiftySmallcap250,
    model_india_small_5factor_10y_IISLNiftySmallcap250
  ),
  include.ci = FALSE, 
  caption = "Indian Small Cap Funds versus Nifty Small Cap 250, 2010-2019",
  doctype = FALSE,
  caption.above = TRUE)

#+ results='asis'
htmlreg(
  list(
    model_india_small_1factor_10y_SPBSE500India,
    model_india_small_3factor_10y_SPBSE500India,
    model_india_small_4factor_10y_SPBSE500India,
    model_india_small_5factor_10y_SPBSE500India
  ),
  include.ci = FALSE, 
  caption = "Indian Small Cap Funds versus S&P BSE 500 India, 2010-2019",
  doctype = FALSE,
  caption.above = TRUE)

#### China Mid Cap

#+ results='asis'
htmlreg(
  list(
    model_china_mid_1factor_10y_FF,
    model_china_mid_3factor_10y_FF,
    model_china_mid_4factor_10y_FF,
    model_china_mid_5factor_10y_FF
  ),
  include.ci = FALSE, 
  caption = "Chinese Mid Cap Funds versus Fama French Benchmark, 2010-2019",
  doctype = FALSE,
  caption.above = TRUE)

#+ results='asis'
htmlreg(
  list(
    model_china_mid_1factor_10y_CSI200,
    model_china_mid_3factor_10y_CSI200,
    model_china_mid_4factor_10y_CSI200,
    model_china_mid_5factor_10y_CSI200
  ),
  include.ci = FALSE, 
  caption = "Chinese Mid Cap Funds versus CSI 200 Mid Cap, 2010-2019",
  doctype = FALSE,
  caption.above = TRUE)

#+ results='asis'
htmlreg(
  list(
    model_china_mid_1factor_10y_MSCIChinaMid,
    model_china_mid_3factor_10y_MSCIChinaMid,
    model_china_mid_4factor_10y_MSCIChinaMid,
    model_china_mid_5factor_10y_MSCIChinaMid
  ),
  include.ci = FALSE, 
  caption = "Chinese Mid Cap Funds versus MSCI China Mid Cap, 2010-2019",
  doctype = FALSE,
  caption.above = TRUE)

#+ results='asis'
htmlreg(
  list(
    model_china_mid_1factor_10y_CSIStateownedEnterprises,
    model_china_mid_3factor_10y_CSIStateownedEnterprises,
    model_china_mid_4factor_10y_CSIStateownedEnterprises,
    model_china_mid_5factor_10y_CSIStateownedEnterprises
  ),
  include.ci = FALSE, 
  caption = "Chinese Mid Cap Funds versus CSI Stateowned Enterprises, 2010-2019",
  doctype = FALSE,
  caption.above = TRUE)


#### Asia Pac ex Japan Mid Cap

#+ results='asis'
htmlreg(
  list(
    model_asiapacexjpn_mid_1factor_10y_FF,
    model_asiapacexjpn_mid_3factor_10y_FF,
    model_asiapacexjpn_mid_4factor_10y_FF,
    model_asiapacexjpn_mid_5factor_10y_FF
  ),
  include.ci = FALSE, 
  caption = "AsiaPac ex Japan Funds versus Fama French Benchmark, 2010-2019",
  doctype = FALSE,
  caption.above = TRUE)

#+ results='asis'
htmlreg(
  list(
    model_asiapacexjpn_mid_1factor_10y_MSCIACAsiaPacificExJapanMid,
    model_asiapacexjpn_mid_3factor_10y_MSCIACAsiaPacificExJapanMid,
    model_asiapacexjpn_mid_4factor_10y_MSCIACAsiaPacificExJapanMid,
    model_asiapacexjpn_mid_5factor_10y_MSCIACAsiaPacificExJapanMid
  ),
  include.ci = FALSE, 
  caption = "AsiaPac ex Japan Funds versus MSCI AC AsiaPac ex Japan Mid Cap, 2010-2019",
  doctype = FALSE,
  caption.above = TRUE)

#+ results='asis'
htmlreg(
  list(
    model_asiapacexjpn_mid_1factor_10y_MSCIACAsiaPacExJPN,
    model_asiapacexjpn_mid_3factor_10y_MSCIACAsiaPacExJPN,
    model_asiapacexjpn_mid_4factor_10y_MSCIACAsiaPacExJPN,
    model_asiapacexjpn_mid_5factor_10y_MSCIACAsiaPacExJPN
  ),
  include.ci = FALSE, 
  caption = "AsiaPac ex Japan Funds versus MSCI AC AsiaPac ex Japan, 2010-2019",
  doctype = FALSE,
  caption.above = TRUE)

#### Alpas on Stock Characteristics

#+ results='asis'
htmlreg(
  list(
    model_alphas_china_age,
    model_alphas_china_size,
    model_alphas_china_expenseratio,
    model_alphas_china_all
  ),
  include.ci = FALSE, 
  caption = "Influences of Chinese Fund Characteristics on their Alphas",
  doctype = FALSE,
  caption.above = TRUE)

#### Alphas on Stock Characteristics

model_ols_full_test <- lm(estimate ~ SOEWeight + ANALYSTFUND + INSTIFUND, data = HOLDINGS_REGRESSION)

#+ results='asis'
htmlreg(
  list(
    model_ols_SOE ,
    model_ols_ANALYST ,
    model_ols_INSTI ,
    model_ols_full
  ),
  include.ci = FALSE, 
  caption = "Influences of Chinese Fund Holdings on their Alphas",
  doctype = FALSE,
  caption.above = TRUE)
