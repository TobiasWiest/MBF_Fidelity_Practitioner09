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


Emerging_5Factors <- read.csv("Emerging_5_Factors.csv") 
Emerging_Momentum <- read.csv("Emerging_MOM_Factor.csv") 

Emerging_Factors <- Emerging_5Factors %>% 
      left_join(Emerging_Momentum, by = "Date")

Emerging_Factors <- Emerging_Factors %>% 
  mutate(date1 = paste(substring(as.character(Date), 1, 4), substring(as.character(Date),5,6), "28", sep = "-")) %>% 
  mutate(date1 = as.Date(date1))

China_Panel <- China_Panel %>% 
  left_join(Emerging_Factors, by = "date1") %>% 
  mutate(R.RF = MonthlyReturn - RF)

model_china_1factor <- lm(R.RF ~ Mkt.RF, data = China_Panel)
model_china_3factor <- lm(R.RF ~ Mkt.RF + SMB + HML, data = China_Panel)
model_china_4factor <- lm(R.RF ~ Mkt.RF + SMB + HML + WML, data = China_Panel)
model_china_5factor <- lm(R.RF ~ Mkt.RF + SMB + HML + RMW + CMA, data = China_Panel)

library(texreg)
#+ results='asis'
htmlreg(
  list(
    model_china_1factor,
    model_china_3factor,
    model_china_4factor,
    model_china_5factor 
  ),
  include.ci = FALSE, 
  caption = "China Factor Models",
  doctype = FALSE)
