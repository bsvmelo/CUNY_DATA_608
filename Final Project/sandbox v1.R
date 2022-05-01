library(tidyverse)
library(tidyquant)
library(timetk)


mf.raw<-read.csv("C:/Users/atm20/OneDrive/Documents/CUNY_DATA_608/Final Project/MutualFunds.csv", sep = ",", header = TRUE, na.strings = c("N/A", ""))
#etf.raw<-read.csv("C:/Users/atm20/OneDrive/Documents/CUNY_DATA_608/Final Project/ETFs.csv", sep = ",", header = TRUE, na.strings = c("N/A", ""))
mf.strat<-read.csv("C:/Users/atm20/OneDrive/Documents/CUNY_DATA_608/Final Project/fund_strategy.csv", sep = ",", header = TRUE, na.strings = c("N/A", ""))


#ETF <- data.frame(na.omit(etf.raw))
#MUTUAL <- data.frame(na.omit(mf.raw))
#etf.raw$fund_type <- "ETF"
#mf.raw$fund_type <- "MF"
#common_col_names <- intersect(names(etf.raw), names(mf.raw))
#df<-full_join(etf.raw,mf.raw, by=common_col_names)

df<-mf.raw
df<-left_join(mf.raw,mf.strat)
#df<-df %>% arrange(desc(fund_long_name))

#mf.raw %>% group_by(fund_long_name) %>% summarize(nn=n()) %>% arrange(desc(nn)) 


#Removing duplicates
df<-distinct(df,fund_long_name, fund_category, .keep_all= TRUE)
df<-distinct(df,investment_strategy,asset_stocks,top10_holdings, .keep_all= TRUE)
df<-df %>% select(fund_strategy, everything())
#df %>% group_by(fund_long_name) %>% summarize(nn=n()) %>% arrange(desc(nn)) 
#remove(etf.raw,mf.raw)


#Vectors:
#Fund Category == All
fund.cat<-unique(df$fund_category)
#Fund Category == Target Date Funds
fund.cat.target<- subset(df, grepl("Target-Date 2", fund_category))
fund.cat.target<-unique(fund.cat$fund_category)
#Fund Category == Allocation
fund.cat.alloc<- subset(df, grepl("Allocation--", fund_category))
fund.cat.alloc<-unique(fund.cat.alloc$fund_category)
#Sectors, Rating, Asset allocation
nms <- names(df)
sec.eq<-colnames(df[grepl("sector_", nms)])
sec.eq<-str_sub(sec.eq,13,)
sec.eq<-str_to_title(str_replace(sec.eq,"_"," "))

sec.alloc<-colnames(df[grepl("asset_", nms)])
sec.alloc<-str_sub(sec.alloc,7,)
sec.alloc<-str_to_title(sec.alloc)

sec.bonds<-colnames(df[grepl("fund_bonds_", nms)])
#sec.bonds<-sec.bonds[2:9]
sec.bonds<-str_sub(sec.bonds,12,)
sec.bonds<-str_to_title(str_replace(sec.bonds,"_"," "))

#Returns
qtr.ret.q1<-colnames(df[grepl("_q1", nms)])
qtr.ret.q2<- colnames(df[grepl("_q2", nms)])
qtr.ret.q3<- colnames(df[grepl("_q3", nms)])
qtr.ret.q4<- colnames(df[grepl("_q4", nms)])
qtr.ret<- c(qtr.ret.q1,qtr.ret.q2,qtr.ret.q3,qtr.ret.q4)
#Portfolio set up
#Fund categories definition
cat.pf.eq <-c("International Equity" , "U.S. Equity" , "Sector Equity")
cat.pf.fi <-c("Taxable Bond" , "Municipal Bond")
cat.pf.alt<-c("Alternative")

cat.pf<-c(cat.pf.eq, cat.pf.fi,cat.pf.alt)


#Pf creation

#aggressive -> Standard Deviation > 75 percentile
a<-df %>% group_by(fund_category) %>% filter(fund_stdev_3years >= quantile(fund_stdev_3years,0.75, na.rm=TRUE)) %>% ungroup() %>% 
  filter(fund_strategy %in% cat.pf) %>% group_by(fund_strategy) %>% top_n(3,fund_return_3years) %>% mutate(Pf_category = 'Aggressive', Pf_alloc = 1/18) %>% ungroup()
#conservative -> Standard Deviation < 25 percentile
b<-df %>% group_by(fund_category) %>% filter(fund_stdev_3years <= quantile(fund_stdev_3years,0.25, na.rm=TRUE)) %>% ungroup() %>% 
  filter(fund_strategy %in% cat.pf) %>% group_by(fund_strategy) %>% top_n(3,fund_return_3years) %>% mutate(Pf_category = 'Conservative', Pf_alloc = 1/18) %>% ungroup()
#moderate -> Standard Deviation btw 25 & 75 percentile
c<-df %>% group_by(fund_category) %>% filter(fund_stdev_3years > quantile(fund_stdev_3years,0.25, na.rm=TRUE) & fund_stdev_3years < quantile(fund_stdev_3years,0.75, na.rm=TRUE)) %>% ungroup() %>% 
  filter(fund_strategy %in% cat.pf) %>% group_by(fund_strategy) %>% top_n(3,fund_return_3years) %>% mutate(Pf_category = 'Moderate', Pf_alloc = 1/18) %>% ungroup()
#final pfs
dt<-rbind(a,b,c)


#Summary statistics
#replace NA by 0
dt <- dt %>%mutate_if(is.numeric, ~replace_na(., 0))
# Fund weighting
alloc<-1/18
# basic return and risk
dt %>% group_by(Pf_category)%>%summarise(Fund_Return_3_yrs=sum(Pf_alloc*fund_return_3years), Risk_3_yrs=sum(Pf_alloc*fund_stdev_3years), Sharpe_3_yrs=sum(Pf_alloc*fund_sharpe_ratio_3years), Fund_Return_5_yrs=sum(Pf_alloc*fund_return_5years), Risk_5_yrs=sum(Pf_alloc*fund_stdev_5years), Sharpe_5_yrs=sum(Pf_alloc*fund_sharpe_ratio_5years))%>%
  arrange(desc(Fund_Return_3_yrs))
#Return streams

#Pf_Category
#dt%>%group_by(Pf_category)%>%select(ends_with("_q1"))%>%summarize(count=n())
dt[paste0(qtr.ret.q1, "_cont")] <- map2(dt %>% select(ends_with("_q1")), alloc,  ~ .x *.y)
dt[paste0(qtr.ret.q2, "_cont")] <- map2(dt %>% select(ends_with("_q2")), alloc,  ~ .x *.y)
dt[paste0(qtr.ret.q3, "_cont")] <- map2(dt %>% select(ends_with("_q3")), alloc,  ~ .x *.y)
dt[paste0(qtr.ret.q4, "_cont")] <- map2(dt %>% select(ends_with("_q4")), alloc,  ~ .x *.y)

ret.stream<-dt%>%group_by(Pf_category) %>% select(contains('_cont')) %>% summarize(across(everything(), list(sum)))
ret.stream<-as.data.frame(ret.stream)
ret.stream.pivot<-pivot_longer(ret.stream,2:88,names_to = "temp",values_to="Return")
#Date conversion
tt<-unlist(ret.stream.pivot[2])
dd<-if_else(str_sub(tt,18,19) %in% 'q1',"3/31/",if_else(str_sub(tt,18,19) %in% 'q2',"6/30/",if_else(str_sub(tt,18,19) %in% 'q3',"9/30/",if_else(str_sub(tt,18,19) %in% 'q4',"12/31/",""))))
yy<-str_sub(tt,13,16)
datt<-paste(dd,yy,sep="")
ret.stream.pivot[,4]=datt
colnames(ret.stream.pivot)[4]<-"Date"
ret.stream.pivot$Date<-as.Date(ret.stream.pivot$Date, "%m/%d/%Y")


#Fund_Strategy
#dt%>%group_by(Pf_category)%>%select(ends_with("_q1"))%>%summarize(count=n())

ret.stream.strat<-dt%>%group_by(Pf_category,fund_strategy) %>% select(contains('_cont')) %>% summarize(across(everything(), list(sum)))
ret.stream.strat<-as.data.frame(ret.stream.strat)
ret.stream.strat.pivot<-pivot_longer(ret.stream.strat,3:89,names_to = "temp",values_to="Return")
#Date conversion
ttt<-unlist(ret.stream.strat.pivot[3])
ddd<-if_else(str_sub(ttt,18,19) %in% 'q1',"3/31/",if_else(str_sub(ttt,18,19) %in% 'q2',"6/30/",if_else(str_sub(ttt,18,19) %in% 'q3',"9/30/",if_else(str_sub(ttt,18,19) %in% 'q4',"12/31/",""))))
yyy<-str_sub(ttt,13,16)
dattt<-paste(ddd,yyy,sep="")
ret.stream.strat.pivot[,5]=dattt
colnames(ret.stream.strat.pivot)[5]<-"Date"
ret.stream.strat.pivot$Date<-as.Date(ret.stream.strat.pivot$Date, "%m/%d/%Y")


#charts approved

# Facet grid by Fund strategy with VaRs
ret.stream.strat.pivot %>% 
  ggplot(aes(x = Return, fill = Pf_category)) + 
  geom_density(aes(fill = Pf_category, colour = Pf_category)) +
  #geom_histogram(alpha = 0.25, binwidth = .01) + #geom_vline(aes(xintercept = quantile(Return,0.05)), data=ret.stream.strat.pivot, colour = "darkgrey", alpha = 0.5) + 
  #annotate(geom = "curve", x = quantile(ret.stream.strat.pivot$Return,0.05)+0.01, y = 70, xend = quantile(ret.stream.strat.pivot$Return,0.05), yend = 60,curvature = .3, arrow = arrow(length = unit(2, "mm"))) +
  #annotate(geom = "text", x = quantile(ret.stream.strat.pivot$Return,0.05)+0.03, y = 73, label = quantile(ret.stream.strat.pivot$Return,0.05), hjust = "left") +
  stat_summary( geom = "vline", orientation = "y", aes(y = 1, xintercept = after_stat(x)), fun = function(x) {quantile(x, probs = c(0.025, 0.975))}) +
  facet_grid(fct_relevel(Pf_category,'Aggressive','Moderate','Conservative') ~ fct_relevel(fund_strategy,'U.S. Equity','Sector Equity','International Equity','Taxable Bond', 'Municipal Bond', 'Alternative')) +
  theme(legend.position = "none")

ret.stream.strat.pivot %>% 
  ggplot(aes(x = Return, fill = Pf_category)) + 
  geom_density(aes(fill = Pf_category, colour = Pf_category)) +
  #geom_histogram(alpha = 0.25, binwidth = .01) + #geom_vline(aes(xintercept = quantile(Return,0.05)), data=ret.stream.strat.pivot, colour = "darkgrey", alpha = 0.5) + 
  #annotate(geom = "curve", x = quantile(ret.stream.strat.pivot$Return,0.05)+0.01, y = 70, xend = quantile(ret.stream.strat.pivot$Return,0.05), yend = 60,curvature = .3, arrow = arrow(length = unit(2, "mm"))) +
  #annotate(geom = "text", x = quantile(ret.stream.strat.pivot$Return,0.05)+0.03, y = 73, label = quantile(ret.stream.strat.pivot$Return,0.05), hjust = "left") +
  stat_summary( geom = "vline", orientation = "y", aes(y = 1, xintercept = after_stat(x)), fun = function(x) {quantile(x, probs = c(0.025, 0.975))}) +
  facet_grid(fct_relevel(Pf_category,'Aggressive','Moderate','Conservative') ~ .) + theme(legend.position = "none")






#histogram
theme_update(plot.title = element_text(hjust = 0.5))

ret.stream.pivot %>% 
  ggplot(aes(x = Return, fill = Pf_category)) + 
  geom_histogram(alpha = 0.25, binwidth = .01)

ret.stream.strat.pivot %>% 
  ggplot(aes(x = Return, fill = Pf_category)) + 
  geom_density(aes(fill = Pf_category, colour = Pf_category)) +
  #geom_histogram(alpha = 0.25, binwidth = .01) + #geom_vline(aes(xintercept = quantile(Return,0.05)), data=ret.stream.strat.pivot, colour = "darkgrey", alpha = 0.5) + 
  #annotate(geom = "curve", x = quantile(ret.stream.strat.pivot$Return,0.05)+0.01, y = 70, xend = quantile(ret.stream.strat.pivot$Return,0.05), yend = 60,curvature = .3, arrow = arrow(length = unit(2, "mm"))) +
  #annotate(geom = "text", x = quantile(ret.stream.strat.pivot$Return,0.05)+0.03, y = 73, label = quantile(ret.stream.strat.pivot$Return,0.05), hjust = "left") +
  stat_summary( geom = "vline", orientation = "y", aes(y = 1, xintercept = after_stat(x)), fun = function(x) {quantile(x, probs = c(0.025, 0.975))}) +
  facet_grid(fct_relevel(Pf_category,'Aggressive','Moderate','Conservative') ~ fct_relevel(fund_strategy,'U.S. Equity','Sector Equity','International Equity','Taxable Bond', 'Municipal Bond', 'Alternative')) 

+   ggtitle("Monthly Returns Since 2005")

# ggplot(df.example, aes(x = value)) +
#   facet_grid(type ~ model) +
#   geom_density(aes(fill = model, colour = model)) +
#   stat_summary(
#     geom = "vline",
#     orientation = "y",
#     # y is a required aesthetic, so use a dummy value
#     aes(y = 1, xintercept = after_stat(x)),
#     fun = function(x) {
#       quantile(x, probs = c(0.025, 0.975))
#     }
#   )



#cols = factor(ret.stream.strat.pivot$fund_strategy, levels=unique(ret.stream.strat.pivot$fund_strategy)) , rows=vars(Pf_category), as.table=FALSE
#forcats::fct_relevel(fund_strategy,'U.S.Equity','Sector Equity','International Equity','Taxable Bond', 'Municipal Bond', 'Alternative')

ret.cons<-
ret.mod
ret.agg



#sector exposure
#sum and product across
dt[paste0(sec.eq, "*")] <- map2(dt %>% select(contains("fund_sector")), alloc,  ~ .x *.y)  
#dt[paste0("sector", 1:11)] <- map2(dt %>% select(contains("fund_sector")), alloc,  ~ .x *.y)  
df.sec.exp<-dt%>%group_by(Pf_category) %>% select(contains('*')) %>% summarize(across(everything(), list(sum)))
df.sec.exp<-as.data.frame(df.sec.exp)
colnames(df.sec.exp)[2:12]<-sec.eq
df.sec.exp.pivot<-pivot_longer(df.sec.exp,2:12,names_to = "Sector",values_to="Exposure")

#bonds exposure
dt[paste0(sec.bonds, "**")] <- map2(dt %>% select(contains("fund_bonds_")), alloc,  ~ .x *.y)
df.bond.exp<-dt%>%group_by(Pf_category) %>% select(contains('**')) %>% summarize(across(everything(), list(sum)))
df.bond.exp<-as.data.frame(df.bond.exp)
colnames(df.bond.exp)[2:10]<-sec.bonds
df.bond.exp <-df.bond.exp[-2]
df.bond.exp.pivot<-pivot_longer(df.bond.exp,2:9,names_to = "Bond_Rating",values_to="Exposure")


#asset exposure
dt[paste0(sec.alloc, "***")] <- map2(dt %>% select(contains("asset_")), alloc,  ~ .x *.y)
df.asset.exp<-dt%>%group_by(Pf_category) %>% select(contains('***')) %>% summarize(across(everything(), list(sum)))
df.asset.exp<-as.data.frame(df.asset.exp)
colnames(df.asset.exp)[2:7]<-sec.alloc
df.asset.exp.pivot<-pivot_longer(df.asset.exp,2:7,names_to = "Asset_Class",values_to="Exposure")






#dt %>% select(contains('fund_sector'),Pf_category, fund_category, fund_symbol) %>% group_by(Pf_category) %>% mutate(mult = 1:8 *alloc)

#dt %>% select(Pf_category, contains('fund_sector')) %>% group_by(Pf_category)%>%rowwise() %>% sum()

#dt %>% group_by(Pf_category)%>%summarise()%>%
#arrange(desc())

#x<-dt %>% mutate(y=across(contains('fund_sector'))*1/18)

#x<-dt %>% mutate(across(where(contains('fund_sector')), prod, na.rm = TRUE))

mtcars %>% 
  select(hp, qsec, disp) %>% 
  rowwise() %>%
  mutate(mult = prod(c_across()))


x%>% group_by(Pf_category) %>% select(sum)
#, .keep = "used"


#ungroup() filter() summarise(final= count=n())
  
  
  
pf.fi <- df %>% filter(fund_category %in% cat.pf.fi) %>% select(fund_symbol, fund_short_name,fund_return_3years, fund_stdev_3years, fund_category, fund_strategy, fund_sharpe_ratio_3years)  
pf.alt <- df %>% filter(fund_category %in% cat.pf.alt) %>% select(fund_symbol, fund_short_name,fund_return_3years, fund_stdev_3years, fund_category, fund_strategy, fund_sharpe_ratio_3years)  
  
df %>% filter(fund_category == cat.pf) %>% group_by(fund_symbol) %>% summarize(group1_sd=quantile(fund_stdev_3years,0.75, na.rm=TRUE)) #%>% ungroup() %>% select(fund_return_3years)
df %>% group_by(fund_category) %>% summarize(group3=quantile(fund_stdev_3years,0.75, na.rm=TRUE), count=n()) %>% arrange(desc(group3))
df %>% group_by(fund_category) %>% summarize(group2=(quantile(fund_stdev_3years,0.75, na.rm=TRUE)-quantile(fund_stdev_3years,0.25, na.rm=TRUE)), count=n()) %>% arrange(desc(group2))

#excludes Allocation and Retirement
df %>% group_by( (fund_strategy)) %>% filter(!is.na(fund_strategy) & !(fund_strategy %in% c("Allocation","Retirement" ))) %>% summarize(group1=quantile(fund_stdev_3years,0.75, na.rm=TRUE), count=n()) %>% arrange(desc(group1))

df %>% group_by((fund_strategy)) %>% filter(!is.na(fund_strategy) & !(fund_strategy %in% c("Allocation","Retirement" ))) %>% summarize(group2=quantile(fund_stdev_3years,0.25, na.rm=TRUE)-quantile(fund_stdev_3years,0.75, na.rm=TRUE), count=n()) %>% arrange(desc(group2))

df %>% group_by((fund_strategy)) %>% filter(!is.na(fund_strategy) & !(fund_strategy %in% c("Allocation","Retirement" ))) %>% summarize(group3=quantile(fund_stdev_3years,0.25, na.rm=TRUE), count=n()) %>% arrange(desc(group3))







#data_frame_mod <- df[df$fund_category %in% "Target",]

# dataframe[dataframe$variable %in% dataframe2$variable2]



df %>% group_by(fund_category) %>% summarize(group1=max(fund_stdev_3years,na.rm=TRUE), count=n() ) %>% ggplot(aes(x=group1, y=count)) + geom_histogram(stat="identity") #%>% arrange(desc(group1))



df %>% group_by(fund_category) %>% summarize(group1=quantile(fund_stdev_3years,0.25, na.rm=TRUE), group3=quantile(fund_stdev_3years,0.75, na.rm=TRUE), count=n()) %>% arrange((group1))


df %>% filter(fund_category=="Allocation--15% to 30% Equity") %>% summarize(count=n_distinct(fund_stdev_3years)) %>% arrange(desc(count))

quantile(df$fund_stdev_3years, probs =seq(0,1,0.1), na.rm=TRUE)