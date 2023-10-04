#setwd("C:/Users/rneb3bv/Desktop/Inflation_Trade_Thesis")
library(readxl)
library(stargazer)



######## EXPORT DATA US NATIONAL DATA

trade<-read_excel("trade_data.xlsx")

###### FORMAT DATE VARIABLES

trade$Date_m <- format(as.Date(trade$Date_m), "%d/%m/%Y")
trade$Date_y <- as.character(trade$Date_y)
trade$Date_y <- format(as.Date(trade$Date_y, format = "%Y"), "%Y")

##### EXPORT US TRADE FLOWS - H2 
panel <- read_excel("trade_country.xlsx")
panel <- read_excel("trade_country.xlsx", col_types = c("date","date", "text", "text","text", "text", "text", "text", "numeric"))


# FORMAT PANEL DATA FOR US TRADE FLOWS

panel$Date_m <- format(as.Date(panel$Date_m), "%d/%m/%Y")
panel$Date_y <- as.character(panel$Date_y)
panel$Date_y <- format(as.Date(panel$Date_y, format = "%Y"), "%Y")

##### MERGE US NATIOANAL DATA WITH US TRADE FLOWS DATA

merged_dataset <- merge( panel,trade, by = c("Date_m","Date_y"))
merged_dataset<-na.omit(merged_dataset)

##### CRETAE NEW DATASETS FOR EXPORTS AND IMPORTS
exports<-merged_dataset[merged_dataset$FlowDesc=="Export",]
imports<-merged_dataset[merged_dataset$FlowDesc=="Import",]


##### CREATE A DUMMY VARIABLE CALLED TARGET FOR SPECIFIC TWO PERIODS 

exports$target[exports$Date_m %in% c("15/10/2012","15/11/2012","15/12/2012","15/01/2013","15/02/2013","15/03/2013","15/04/2013","15/05/2013","15/06/2013","15/07/2013","15/08/2013","15/09/2013","15/10/2013","15/11/2013","15/12/2013","15/01/2014","15/02/2014","15/03/2014","15/04/2014","15/05/2014","15/06/2014","15/07/2014","15/08/2014","15/09/2014","15/10/2014","15/11/2014","15/12/2014","15/01/2015","15/02/2015","15/03/2015","15/04/2015","15/05/2015","15/06/2015","15/07/2015","15/08/2015","15/09/2015","15/10/2015","15/11/2015")]<-1
exports$target[exports$Date_m %in% c("15/11/2019","15/12/2019","15/01/2020","15/02/2020","15/03/2020","15/04/2020","15/05/2020","15/06/2020","15/07/2020","15/08/2020","15/09/2020","15/10/2020","15/11/2020","15/12/2020","15/01/2021","15/02/2021","15/03/2021","15/04/2021","15/05/2021","15/06/2021","15/07/2021","15/08/2021","15/09/2021","15/10/2021","15/11/2021","15/12/2021","15/01/2022","15/02/2022","15/03/2022","15/04/2022","15/05/2022","15/06/2022","15/07/2022","15/08/2022","15/09/2022","15/10/2022","15/11/2022","15/12/2022")]<-0

imports$target[imports$Date_m %in% c("15/10/2012","15/11/2012","15/12/2012","15/01/2013","15/02/2013","15/03/2013","15/04/2013","15/05/2013","15/06/2013","15/07/2013","15/08/2013","15/09/2013","15/10/2013","15/11/2013","15/12/2013","15/01/2014","15/02/2014","15/03/2014","15/04/2014","15/05/2014","15/06/2014","15/07/2014","15/08/2014","15/09/2014","15/10/2014","15/11/2014","15/12/2014","15/01/2015","15/02/2015","15/03/2015","15/04/2015","15/05/2015","15/06/2015","15/07/2015","15/08/2015","15/09/2015","15/10/2015","15/11/2015")]<-1
imports$target[imports$Date_m %in% c("15/11/2019","15/12/2019","15/01/2020","15/02/2020","15/03/2020","15/04/2020","15/05/2020","15/06/2020","15/07/2020","15/08/2020","15/09/2020","15/10/2020","15/11/2020","15/12/2020","15/01/2021","15/02/2021","15/03/2021","15/04/2021","15/05/2021","15/06/2021","15/07/2021","15/08/2021","15/09/2021","15/10/2021","15/11/2021","15/12/2021","15/01/2022","15/02/2022","15/03/2022","15/04/2022","15/05/2022","15/06/2022","15/07/2022","15/08/2022","15/09/2022","15/10/2022","15/11/2022","15/12/2022")]<-0


exports <- pdata.frame(exports, index = c("PartnerDesc","Date_m","Date_y"))
imports <- pdata.frame(imports, index = c("PartnerDesc","Date_m","Date_y"))

#### FORMAT DATES AND SORT
exports$Date_m <- as.Date(exports$Date_m, format = "%d/%m/%Y")
exports$Date_y <- as.character(exports$Date_y)
exports$Date_y <- format(as.Date(exports$Date_y, format = "%Y"), "%Y")
exports <- exports%>%
  arrange(Date_m)


imports$Date_m <- as.Date(imports$Date_m, format = "%d/%m/%Y")
imports$Date_y <- as.character(imports$Date_y)
imports$Date_y <- format(as.Date(imports$Date_y, format = "%Y"), "%Y")
imports <- imports%>%
  arrange(Date_m)

#### DROP NAs
imports<-na.omit(imports)
exports<-na.omit(exports)


################# TOP 10 TRADING PARTNERS exclude CHINA ################# 


##### FILTER DATA WITH TOP 10

top10<-c("Canada","Mexico","Japan","United Kingdom","Germany","Netherlands","South Korea","Brazil","India","Vietnam")
top10_exports<-exports[exports$PartnerDesc %in% top10,]
top10_imports<-imports[imports$PartnerDesc %in% top10,]


#### DIFFERENCE 
top10_exports$lagged_exp <- ave(top10_exports$PrimaryValue ,top10_exports$PartnerDesc , FUN = function(x) c(NA, head(x,-1)))
top10_exports$delta_PrimaryValue <- top10_exports$PrimaryValue - top10_exports$lagged_exp 

top10_imports$lagged_exp <- ave(top10_imports$PrimaryValue ,top10_imports$PartnerDesc , FUN = function(x) c(NA, head(x,-1)))
top10_imports$delta_PrimaryValue <- top10_imports$PrimaryValue - top10_imports$lagged_exp 

top10_imports$lagged_surp <- ave(top10_imports$surprise_rate ,top10_imports$PartnerDesc , FUN = function(x) c(NA, head(x,-1)))
top10_imports$delta_surp <- top10_imports$surprise_rate - top10_imports$lagged_surp 

top10_exports$lagged_surp <- ave(top10_exports$surprise_rate ,top10_exports$PartnerDesc , FUN = function(x) c(NA, head(x,-1)))
top10_exports$delta_surp <- top10_exports$surprise_rate - top10_exports$lagged_surp 

##### PANEL REGRESSIONS WITH DIFFERENT SPECIFICATIONS 


m1<- plm(log(PrimaryValue) ~  log(inflation) + diff_gdp +delta_diff_oil + target*log(inflation)  + target*diff_reer  , data = top10_exports, model = "within")
summary(m1)

m2 <- plm(delta_PrÝmaryValue ~ delta_inflation + delta_gdp + delta_reer + delta_oil + target * delta_inflation + delta_inflation * delta_reer + target * delta_reer, data = top10_imports, model = "within", effect = "twoways")
summary(m2)

m3<- plm(delta_PrimaryValue ~ delta_inflation + delta_gdp + delta_reer + delta_oil + target * delta_inflation + delta_inflation * delta_reer + target * delta_reer, data = top10_exports, model = "within", effect = "twoways")
summary(m3)

m4 <- plm(delta_PrimaryValue ~ delta_inflation + delta_gdp + delta_reer + delta_oil + target * delta_inflation + delta_inflation * delta_reer + target * delta_reer +delta_inflation*delta_gdp , data = top10_imports, model = "within", effect = "twoways")
summary(m4)


######  drop yearly fe
top10_exports_m<- top10_exports[,c(1,3:42)]
top10_imports_m<- top10_imports[,c(1,3:42)]


m5<- plm(delta_PrimaryValue ~ delta_inflation + delta_gdp + delta_reer + delta_oil + target * delta_inflation  + delta_inflation * delta_reer + target *delta_reer, data = top10_exports_m, model = "within", effect = "time")
summary(m5)

m6<- plm(delta_PrimaryValue ~ delta_inflation + delta_gdp + delta_reer + delta_oil + target * delta_inflation  + delta_inflation * delta_reer + target *delta_reer, data = top10_imports_m, model = "within", effect = "twoway")
summary(m6)

stargazer(m5,m6, type = "html"  ,digits.extra = 5,  column.labels=c("deltaExport","deltaImport") ,title="Regression Results",add.lines=list(c('Country FE', 'Yes','Yes'),c('Monthly Time FE', 'Yes','Yes'),c('Yearly Time FE', 'No','No')), digits=1,out="Panel_final_last2.html")
########### delta_suprise 

m7<- plm(delta_PrimaryValue ~ delta_surp + delta_gdp + delta_reer + delta_oil + target * delta_surp  + delta_surp * delta_reer + target *delta_reer, data = top10_exports, model = "within", effect = "twoways")
summary(m7)

m8<- plm(delta_PrimaryValue ~ delta_surp + delta_gdp + delta_reer + delta_oil + target * delta_surp  + delta_surp * delta_reer + target *delta_reer, data = top10_imports, model = "within", effect = "twoways")
summary(m8)

stargazer(m7,m8, type = "html"  ,digits.extra = 5,  column.labels=c("deltaExport","deltaImport") ,title="Regression Results",add.lines=list(c('Country FE', 'Yes','Yes'),c('Monthly Time FE', 'Yes','Yes'),c('Yearly Time FE', 'Yes','Yes')), digits=1,out="Panel_final_suprise.html")


