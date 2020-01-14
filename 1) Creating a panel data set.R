library(tidyr)
library(dplyr)
library(readxl)
library(tidyverse)
Full_Equity_Sample <- read_excel("Full Equity Sample.xlsx")
colnames(Full_Equity_Sample)

Full_Equity_Sample <- Full_Equity_Sample %>% 
  mutate(FundID = row_number())

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
                is.na(MonthlyReturn) == 1 &
                is.na(EquityStyle) == 1 ))

             
Full_Equity_Panel <- Full_Equity_Panel %>%
  mutate(date1 = paste(as.character(date), "-28", sep = "")) %>% 
  mutate(date1 = as.Date(date1)) %>% 
  mutate(year = as.numeric(format(date1,'%Y'))) %>% 
  mutate(month = as.numeric(format(date1, '%m')))

# Full_Equity_Panel %>% 
#   ggplot(aes(x = date1)) +
#   geom_bar()

# Full_Equity_Panel %>% Full_Equity_Panel %>% 
#   group_by(FundID, year) %>% 
#     mutate(FundSize = ifelse(is.na(FundSize) == 0, FundSize, mean(FundSize, na.rm = TRUE))) %>% 
#       ungroup()
# 
# Full_Equity_Panel %>%    
#     group_by(year) %>% 
#           summarize(TotalAUM = sum(FundSize, na.rm = TRUE)) %>%
#             ungroup() %>% 
#               ggplot(aes(x = year, y = TotalAUM)) +
#                geom_line()

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
  mutate(date1 = as.Date(date1))


# Benchmark Returns from Morningstar
Benchmarks <- read_excel("Benchmark Returns.xlsx")
Benchmarks <- Benchmarks %>% 
  mutate(date1 = as.Date(Date))

Benchmarks_Factors <- Emerging_Factors %>% 
    left_join(Benchmarks, by = "date1") %>% 
    filter(date1 > "2010-01-01")


#### China Factor Models

China_Panel <- China_Panel %>% 
  left_join(Benchmarks_Factors, by = "date1") 

China_Panel <- rename(China_Panel, CSIStateownedEnterprisesCompPRCNY = `CSIState-ownedEnterprisesCompPRCNY`)
  
China_Panel <- China_Panel %>% 
  mutate(Funds.RF = MonthlyReturn - RF) %>% 
  mutate(MSCIChinaNRUSD.RF = MSCIChinaNRUSD - RF) %>% 
  mutate(CSI300NRUSD.RF = CSI300NRUSD - RF) %>% 
  mutate(CSIStateownedEnterprisesCompPRCNY.RF = CSIStateownedEnterprisesCompPRCNY - RF)

model_china_1factor_10y_FF <- lm(Funds.RF ~ Mkt.RF, data = China_Panel)
model_china_3factor_10y_FF <- lm(Funds.RF ~ Mkt.RF + SMB + HML, data = China_Panel)
model_china_4factor_10y_FF <- lm(Funds.RF ~ Mkt.RF + SMB + HML + WML, data = China_Panel)
model_china_5factor_10y_FF <- lm(Funds.RF ~ Mkt.RF + SMB + HML + RMW + CMA, data = China_Panel)

model_china_1factor_10y_CSI300 <- lm(Funds.RF ~ CSI300NRUSD.RF, data = China_Panel)
model_china_3factor_10y_CSI300 <- lm(Funds.RF ~ CSI300NRUSD.RF + SMB + HML, data = China_Panel)
model_china_4factor_10y_CSI300 <- lm(Funds.RF ~ CSI300NRUSD.RF + SMB + HML + WML, data = China_Panel)
model_china_5factor_10y_CSI300 <- lm(Funds.RF ~ CSI300NRUSD.RF + SMB + HML + RMW + CMA, data = China_Panel)

model_china_1factor_10y_MSCIChina <- lm(Funds.RF ~ MSCIChinaNRUSD.RF, data = China_Panel)
model_china_3factor_10y_MSCIChina <- lm(Funds.RF ~ MSCIChinaNRUSD.RF + SMB + HML, data = China_Panel)
model_china_4factor_10y_MSCIChina <- lm(Funds.RF ~ MSCIChinaNRUSD.RF + SMB + HML + WML, data = China_Panel)
model_china_5factor_10y_MSCIChina <- lm(Funds.RF ~ MSCIChinaNRUSD.RF + SMB + HML + RMW + CMA, data = China_Panel)

model_china_1factor_10y_CSISOE <- lm(Funds.RF ~ CSIStateownedEnterprisesCompPRCNY.RF, data = China_Panel)
model_china_3factor_10y_CSISOE <- lm(Funds.RF ~ CSIStateownedEnterprisesCompPRCNY.RF + SMB + HML, data = China_Panel)
model_china_4factor_10y_CSISOE <- lm(Funds.RF ~ CSIStateownedEnterprisesCompPRCNY.RF + SMB + HML + WML, data = China_Panel)
model_china_5factor_10y_CSISOE <- lm(Funds.RF ~ CSIStateownedEnterprisesCompPRCNY.RF + SMB + HML + RMW + CMA, data = China_Panel)

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
  caption = "Chinese Funds versus Fama French EM Benchmark",
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
  caption = "Chinese Funds versus CSI 300",
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
  caption = "Chinese Funds versus MSCI China",
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
  caption = "Chinese Funds versus CSI Stateowned Enterprises Comp",
  doctype = FALSE,
  caption.above = TRUE)


#### Asia Pacific ex Japan Factor Models

AsiaPacific_Panel <- AsiaPacific_Panel %>% 
  left_join(Benchmarks_Factors, by = "date1") 

AsiaPacific_Panel <- rename(AsiaPacific_Panel, CSIStateownedEnterprisesCompPRCNY = `CSIState-ownedEnterprisesCompPRCNY`)


AsiaPacific_Panel <- AsiaPacific_Panel %>% 
  mutate(Funds.RF = MonthlyReturn - RF) %>% 
  mutate(MSCIACAsiaPacExJPNNRUSD.RF = MSCIACAsiaPacExJPNNRUSD - RF) %>% 
  mutate(CSI300NRUSD.RF = CSI300NRUSD - RF) %>% 
  mutate(CSIStateownedEnterprisesCompPRCNY.RF = CSIStateownedEnterprisesCompPRCNY - RF)

model_asiapacexjpn_1factor_10y_FF <- lm(Funds.RF ~ Mkt.RF, data = AsiaPacific_Panel)
model_asiapacexjpn_3factor_10y_FF <- lm(Funds.RF ~ Mkt.RF + SMB + HML, data = AsiaPacific_Panel)
model_asiapacexjpn_4factor_10y_FF <- lm(Funds.RF ~ Mkt.RF + SMB + HML + WML, data = AsiaPacific_Panel)
model_asiapacexjpn_5factor_10y_FF <- lm(Funds.RF ~ Mkt.RF + SMB + HML + RMW + CMA, data = AsiaPacific_Panel)

model_asiapacexjpn_1factor_10y_CSI300 <- lm(Funds.RF ~ CSI300NRUSD.RF, data = AsiaPacific_Panel)
model_asiapacexjpn_3factor_10y_CSI300 <- lm(Funds.RF ~ CSI300NRUSD.RF + SMB + HML, data = AsiaPacific_Panel)
model_asiapacexjpn_4factor_10y_CSI300 <- lm(Funds.RF ~ CSI300NRUSD.RF + SMB + HML + WML, data = AsiaPacific_Panel)
model_asiapacexjpn_5factor_10y_CSI300 <- lm(Funds.RF ~ CSI300NRUSD.RF + SMB + HML + RMW + CMA, data = AsiaPacific_Panel)

model_asiapacexjpn_1factor_10y_MSCIAsiaPacExJpn <- lm(Funds.RF ~ MSCIACAsiaPacExJPNNRUSD.RF, data = AsiaPacific_Panel)
model_asiapacexjpn_3factor_10y_MSCIAsiaPacExJpn <- lm(Funds.RF ~ MSCIACAsiaPacExJPNNRUSD.RF + SMB + HML, data = AsiaPacific_Panel)
model_asiapacexjpn_4factor_10y_MSCIAsiaPacExJpn <- lm(Funds.RF ~ MSCIACAsiaPacExJPNNRUSD.RF + SMB + HML + WML, data = AsiaPacific_Panel)
model_asiapacexjpn_5factor_10y_MSCIAsiaPacExJpn <- lm(Funds.RF ~ MSCIACAsiaPacExJPNNRUSD.RF + SMB + HML + RMW + CMA, data = AsiaPacific_Panel)

model_asiapacexjpn_1factor_10y_CSISOE <- lm(Funds.RF ~ CSIStateownedEnterprisesCompPRCNY.RF, data = AsiaPacific_Panel)
model_asiapacexjpn_3factor_10y_CSISOE <- lm(Funds.RF ~ CSIStateownedEnterprisesCompPRCNY.RF + SMB + HML, data = AsiaPacific_Panel)
model_asiapacexjpn_4factor_10y_CSISOE <- lm(Funds.RF ~ CSIStateownedEnterprisesCompPRCNY.RF + SMB + HML + WML, data = AsiaPacific_Panel)
model_asiapacexjpn_5factor_10y_CSISOE <- lm(Funds.RF ~ CSIStateownedEnterprisesCompPRCNY.RF + SMB + HML + RMW + CMA, data = AsiaPacific_Panel)

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
  caption = "Asian Pacfic ex Japan versus Fama French EM Benchmark",
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
  caption = "Asian Pacfic ex Japan versus CSI 300",
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
  caption = "Asian Pacfic ex Japan versus MSCI Asia Pacific ex Japan",
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
  caption = "Asian Pacfic ex Japan Funds versus CSI Stateowned Enterprises Comp",
  doctype = FALSE,
  caption.above = TRUE)

#### India

India_Panel <- India_Panel %>% 
  left_join(Benchmarks_Factors, by = "date1") 

