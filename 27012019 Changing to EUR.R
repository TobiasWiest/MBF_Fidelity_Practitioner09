library(tidyr)
library(dplyr)
library(readxl)


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

library(reshape2)

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

Large_Panel <- Full_Equity_Panel[grepl("Large", Full_Equity_Panel$EquityStyle),]
Mid_Panel <- Full_Equity_Panel[grepl("Mid", Full_Equity_Panel$EquityStyle),]
Small_Panel <- Full_Equity_Panel[grepl("Small", Full_Equity_Panel$EquityStyle),]

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

library(texreg)
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

library(texreg)
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

##### One Regression per fund

library(lme4)


#Only keep funds with at lest 24 observations
China_Panel_Ind <- China_Panel[as.numeric(ave(China_Panel$FundID, China_Panel$FundID, FUN=length)) >= 24, ]
AsiaPacific_Panel_Ind <- AsiaPacific_Panel[as.numeric(ave(AsiaPacific_Panel$FundID, AsiaPacific_Panel$FundID, FUN=length)) >= 24, ]
India_Panel_Ind <- India_Panel[as.numeric(ave(India_Panel$FundID, India_Panel$FundID, FUN=length)) >= 24, ]
AsiaEmerg_Panel_Ind <- AsiaEmerg_Panel[as.numeric(ave(AsiaEmerg_Panel$FundID, AsiaEmerg_Panel$FundID, FUN=length)) >= 24, ]



library(broom)
library(tidyverse)


China_Panel_Ind_10y_FF <- China_Panel_Ind %>% 
  group_by(FundID) %>%
  do(formula_FF = lm(Funds.RF ~ Mkt.RF , data = .))

fund_alphas_china_10y_FF <- tidy(China_Panel_Ind_10y_FF, formula_FF) %>% 
      filter(term == "(Intercept)")

fund_alphas_china_10y_FF %>% 
  ggplot(aes(estimate)) +
      geom_freqpoly(bins = 50)

sum(fund_alphas_china_10y_FF$estimate > 0 & fund_alphas_china_10y_FF$p.value < 0.05)
sum(fund_alphas_china_10y_FF$estimate < 0 & fund_alphas_china_10y_FF$p.value < 0.05)
sum(fund_alphas_china_10y_FF$p.value >= 0.05)

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


    
