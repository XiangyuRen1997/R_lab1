library(tidyverse) 
library(huxtable) 
library(dplyr)
library(tidyr)
library(writexl)
library(ggplot2) 
library(data.table)


setwd("/Users/xiangyuren/Documents/applied micro")
getwd()
#part 2
df_roses<-read.csv("rose_data.csv",stringsAsFactors = FALSE)
view(df_roses)


write.table(count(df_roses),file="table1.xlsx", row.names = "original data", col.names="Numbers of observations for each dataset" )

#a measure the number of roses registered and patented
df_annual <- df_roses %>% group_by(regyear) %>%
  summarise(roses_regist = n(), roses_patent = sum(!is.na(patentnumber))) %>%
  filter(regyear>1900) # report N

View(df_annual)
summary(df_annual)

#b match: filter years and registration years before patented years
df_roses1<-subset(df_roses, regyear>1900)
view(df_roses1) #filter years>1900 report N
write.table(count(df_roses1),file="table1.xlsx", row.names="data restricted years", col.names=FALSE, append= TRUE)
df_rose<-subset(df_roses1, regyear<patentyear|is.na(patentyear))
view(df_rose)#filter registration year<patent years report N
write.table(count(df_rose),file="table1.xlsx", row.names="data restricted years and registed years< patented years", col.names=FALSE,  append= TRUE)

df_rose_adj<-df_rose %>% group_by(regyear) %>%
  summarize(roses_regist = n(), 
            roses_patent = sum(!is.na(patentnumber)& (regyear < patentyear)) ) %>%
            filter(regyear>=1900)
view(df_rose_adj) #group by registration year for patents and registration only
df_annual_adj <- df_rose %>% group_by(regyear,color,class) %>%
  summarise(roses_regist = n(), 
            roses_patent = sum(!is.na(patentnumber)& (regyear < patentyear))) %>%
            filter(regyear>=1900)
View(df_annual_adj) #group by registration year and keep color and color classification for further uses
print(nrow(df_roses))

#c


df_annual_by_color <- df_annual_adj %>% group_by(regyear) %>%
  summarise(roses_regist = n(), 
            roses_white = sum(color == "w"),
            roses_yellow = sum(color %in% c("ly", "my", "dy", "yb")),
            roses_red = sum(color %in% c("mr", "dr", "rb")),
            roses_orange = sum(color %in%  c("ob","op","or")),
            roses_pink = sum(color %in% c("mp","lp","dp")),
            roses_patent) %>%
      filter(regyear>1900)

View(df_annual_by_color)
df_color_b1930<-subset(df_annual_by_color, regyear<1930) #subset of rose by color before 1930
write.table(sum(df_color_b1930$roses_white),file="table2.tex",row.names="number of registed white roses before 1930", col.names=FALSE)
write.table(sum(df_color_b1930$roses_yellow),file="table2.tex",row.names="number of registed yellow roses before 1930", col.names=FALSE, append=TRUE)
write.table(sum(df_color_b1930$roses_orange),file="table2.tex",row.names="number of registed orange roses before 1930", col.names=FALSE, append=TRUE)
write.table(sum(df_color_b1930$roses_red),file="table2.tex",row.names="number of registed red roses before 1930", col.names=FALSE, append=TRUE)
write.table(sum(df_color_b1930$roses_pink),file="table2.tex",row.names="number of registed pink roses before 1930", col.names=FALSE, append=TRUE)
df_color_a1930<-subset(df_annual_by_color, regyear>=1930)
write.table(sum(df_color_a1930$roses_white),file="table2.tex",row.names="number of registed white roses after 1930", col.names=FALSE,append=TRUE)
write.table(sum(df_color_a1930$roses_yellow),file="table2.tex",row.names="number of registed yellow roses after 1930", col.names=FALSE, append=TRUE)
write.table(sum(df_color_a1930$roses_orange),file="table2.tex",row.names="number of registed orange roses after 1930", col.names=FALSE, append=TRUE)
write.table(sum(df_color_a1930$roses_red),file="table2.tex",row.names="number of registed red roses after 1930", col.names=FALSE, append=TRUE)
write.table(sum(df_color_a1930$roses_pink),file="table2.tex",row.names="number of registed pink roses after 1930", col.names=FALSE, append=TRUE)




df_annual_by_class <- df_rose %>% group_by(regyear) %>%
  summarise(roses_regist = n(), 
            roses_Flouribonda=sum(class=="F"),
            roses_Htea=sum(class=="HT"),
            roses_Shrub=sum(class=="S"),
            roses_Miniature=sum(class=="Min"),
            roses_patent = sum(!is.na(patentnumber))) %>%
  filter(regyear>1900)
View(df_annual_by_class)
df_class_b1930<-subset(df_annual_by_class,regyear<1930)
write.table(sum(df_class_b1930$roses_Flouribonda),file="table2.tex",row.names="number of registed Flouribonda roses before 1930", col.names=FALSE, append=TRUE)
write.table(sum(df_class_b1930$roses_Htea),file="table2.tex",row.names="number of registed Htea roses before 1930", col.names=FALSE, append=TRUE)
write.table(sum(df_class_b1930$roses_Shrub),file="table2.tex",row.names="number of registed shrub roses before 1930", col.names=FALSE, append=TRUE)
write.table(sum(df_class_b1930$roses_Miniature),file="table2.tex",row.names="number of registed miniature roses before 1930", col.names=FALSE, append=TRUE)
df_class_a1930<-subset(df_annual_by_class,regyear>=1930)
write.table(sum(df_class_a1930$roses_Flouribonda),file="table2.tex",row.names="number of registed Flouribonda roses after 1930", col.names=FALSE, append=TRUE)
write.table(sum(df_class_a1930$roses_Htea),file="table2.tex",row.names="number of registed Htea roses after 1930", col.names=FALSE, append=TRUE)
write.table(sum(df_class_a1930$roses_Shrub),file="table2.tex",row.names="number of registed shrub roses after 1930", col.names=FALSE, append=TRUE)
write.table(sum(df_class_a1930$roses_Miniature),file="table2.tex",row.names="number of registed miniature roses after 1930", col.names=FALSE, append=TRUE)

#d 
df_annual_by_c<-df_rose %>% group_by(regyear, color) %>%
  summarise(roses_regist = n(), 
            roses_patent = sum(!is.na(patentnumber)))
df_annual_by_c_nm <- df_annual_by_c[!(df_annual_by_c$color=="" |df_annual_by_c$color=="?"), ]

df_annual_by_c_nm  = subset(df_annual_by_c, select = -roses_patent)
view(df_annual_by_c_nm)
df_annual_by_c_wide <- df_annual_by_c_nm %>% 
  gather(key, value, -regyear,-color) %>%  
  unite(new.col, c(key,color)) %>%   
  spread(new.col, value) 

view(df_annual_by_c_wide)
df_annual_by_c_table<- subset(df_annual_by_c_wide)
df_annual_by_c_table<- df_annual_by_c_table %>% select_if(~sum(!is.na(.)) > 0)
options(scipen=999)
df_annual_by_c_table2 <- tidyr::pivot_longer(df_annual_by_c_table, cols=c('roses_regist_ab', 'roses_regist_dp','roses_regist_dr','roses_regist_ly','roses_regist_w'), names_to='color', 
                                                 values_to="regist_to_plot")

df_annual_by_c_table2 <- df_annual_by_c_table2[, -c(2:17)]

ggplot(data=df_annual_by_c_table2, 
       aes(x=regyear, y=regist_to_plot, fill=color)) +
  geom_bar(stat="identity", position='dodge') + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
  xlab("Year of Registraton") +
  ylab("Number of Registrations") +
  ggtitle("Registrations by color") + 
  scale_fill_discrete(name = "Color", labels = c("Apricot", "Deep Pink","Deep red", "Light yellow", "White"))



#e report t years before and after patent law
df_annual_1900_1960<-subset(df_rose_adj, df_rose_adj$regyear<=1960 &df_rose_adj$regyear>=1900)
view(df_annual_1900_1960)
sum(df_annual_1900_1960$roses_regist)

#f
 #1
df_annual_1900_1960_long <- melt(setDT(df_annual_1900_1960), id.vars = c("regyear"), variable.name = "shade") %>%
  arrange(regyear)
view(df_annual_1900_1960_long)
plot1<-ggplot(df_annual_1900_1960_long, aes(x=regyear, y=value)) +
  geom_line(aes(group=shade, linetype=shade))+theme(legend.position = "bottom")
plot2<-plot1+geom_vline( xintercept = 1930, color ="red", linetype=1)+xlab("Year")+ylab("number of variations in roses")
plot2
 #2 share of registration by patent
df_annual_1900_1960$share<-df_annual_1900_1960$roses_patent/df_annual_1900_1960$roses_regist
view(df_annual_1900_1960)

df_share<-select(df_annual_1900_1960,-roses_patent,-roses_regist)
view(df_share)
df_share_long<- melt(setDT(df_share), id.vars = c("regyear"),variable.name = "shade") %>%
  arrange(regyear)
view(df_share_long)

plot3<-ggplot(df_share_long, aes(x=regyear, y=value))+
  geom_line(aes(group=shade))+xlab("Registration year")+ylab("share of varieties patented")
plot3
  #f3 extra credit
  


#3 time-series data
 #combination of class and color
df_variations<-df_rose %>% group_by(regyear) %>%
  summarise(roses_regist=n(), 
            roses_white = sum(color == "w"),
            roses_yellow = sum(color %in% c("ly", "my", "dy", "yb")),
            roses_pink = sum(color %in% c("mp","lp","dp")),
            roses_Flouribonda=sum(class=="F"),
            roses_Miniature=sum(class=="Min"),
            roses_Bourbon=sum(class=="B")) 
view(df_variations)
df_vari<-select(df_variations,-roses_regist)
df_variation_long <- melt(setDT(df_vari), id.vars = c("regyear"), variable.name = "shade") %>%
  arrange(regyear)
view(df_variation_long)

ggplot(df_variation_long, aes(x = regyear, y = value, color=shade)) +
  geom_line(aes(group=shade))+xlab("Year")+ylab("Number of registered roses")+
  theme(legend.position = "bottom")+geom_vline( xintercept = 1930, color ="black", linetype=1)

#4 combination with GDP
# Merge data
df_gdp <- read.csv("USGDP_1790-2013.csv",  # import the data
                   skip = 1, # the first line is a header, skip it
                   stringsAsFactors = FALSE)  # this options means that we are not using factors

# Those column names are too long, let's rename them
colnames(df_gdp) <- c("year", "ngdp", "realgdp", 
                      "deflator", "pop")

df_gdp <- df_gdp %>% mutate(ngdp = as.numeric(gsub(",", "", ngdp)), #gsub is used to replace symbols
                            realgdp = as.numeric(gsub(",", "", realgdp)),#here, we replace "," with nothing.
                            deflator = as.numeric(gsub(",", "", deflator)),
                            pop = as.numeric(gsub(",", "", pop)))

View(df_gdp)

class(df_gdp$realgdp)
# Now, we are ready to merge the datasets.

df_annual_merge <- merge(df_rose_adj, df_gdp,
                         by.x = "regyear", #name of  ID in df_annual
                         by.y = "year", #name of  ID in df_gdp
                         all.x = TRUE #keep all years in df_annual
) 
View(df_annual_merge)
df_annual_merge$reggdp=df_annual_merge$roses_regist/df_annual_merge$realgdp
view(df_annual_merge)
df_annual_merge1<- select(df_annual_merge,-ngdp,-roses_patent,-roses_regist,-realgdp,-deflator,-pop) %>%
  filter(regyear>1900)


df_merge_long<- melt(setDT(df_annual_merge1), id.vars = c("regyear"), variable.name = "shade") %>%
  arrange(regyear)
view(df_merge_long)

ggplot(df_merge_long, aes(x = regyear, y = value)) +
  geom_line(aes(group=shade) )+xlab("Year")+ylab("varieties registered/GDP")+geom_vline( xintercept = 1930, color ="red", linetype=1)

#5 Type of innovator change
df_b1930<-subset(df_rose,regyear<1930) #subset before 1930
#df_b1930<-subset(df_rose,regyear<1930) #subset before 1930
df_innovator_b1930<-df_b1930 %>% group_by(hybridizer) %>%
  summarise(roses_regist = n()) %>%
  filter(!hybridizer=="")%>%
  arrange(desc(roses_regist)) #ranking innovators
df_innovator_b1930$share<- df_innovator_b1930$roses_regist/sum(df_innovator_b1930$roses_regist)
view(df_innovator_b1930)
df_topin_b1930<-subset(df_innovator_b1930, roses_regist>60)


view(df_topin_b1930)
sumtop_b1930<-sum(df_topin_b1930$share)
sumtop_b1930
write.table(sumtop_b1930,file="table3.tex",row.names="Share of registered roses for top 10 innovator before 1930",col.names=FALSE)
ggplot(df_topin_b1930,aes(hybridizer, share))+geom_histogram(stat="identity",width=0.5, fill="blue")+ylab("number of roses registered")+xlab("hybridizer before 1930")

df_a1930<-subset(df_rose,regyear>=1930) #subset after 1930
df_innovator_a1930<-df_a1930 %>% group_by(hybridizer) %>%
  summarise(roses_regist = n() )  %>%
  filter(!hybridizer=="")%>%
  arrange(desc(roses_regist)) #ranking innovators
df_innovator_a1930$share<- df_innovator_a1930$roses_regist/sum(df_innovator_a1930$roses_regist)

view(df_innovator_a1930)
df_topin_a1930<-subset(df_innovator_a1930, roses_regist>220)
sumtop_a1930<-sum(df_topin_a1930$share)
sumtop_a1930
write.table(sumtop_a1930,file="table3.tex",row.names="Share of registered roses for top 10 innovator after 1930",col.names=FALSE, append=TRUE)
ggplot(df_topin_a1930,aes(hybridizer, share))+geom_histogram(stat="identity",width=0.5, fill="blue")+ylab("number of roses registered")+xlab("hybridizer after 1930")




