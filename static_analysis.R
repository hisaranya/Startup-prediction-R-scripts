library(plyr)
library(ggplot2)
library(xtable)
library(gridExtra)
library(maps)
library(mapdata)
library(mapproj)
library(maptools)
library(ggmap)
library(sp)
library(stringr)
library(ggsubplot)
library(reshape2)
library(taRifx)
library(XML)
library(quantmod)
library(PerformanceAnalytics)
library(gridBase)
investments <- read.csv("investment.csv", header=TRUE, stringsAsFactors = F)
companies <- read.csv("company.csv", header=TRUE, stringsAsFactors = F)
ind.na = which(companies[,"market"] == "")
companies$market[ind.na] = "Others"
rounds <- read.csv("rounds.csv", header=TRUE, stringsAsFactors = F)
acquisition <- read.csv("acquisition.csv", header=TRUE, stringsAsFactors = F)
addition <- read.csv("addition.csv", header=TRUE, stringsAsFactor = F)

## line plot investment vs time

investments <- unique(investments)
investments$investor_type = sapply(strsplit(investments$investor_permalink,"/"), "[", 2)
unique(investments$investor_type)
##invest.cate <- subset(investments, investor_category_code != "" & comapany_name != "Army & Air Force Exchange Service")
invest.status <- ddply(investments,.(investor_type), summarize, sum_funds= sum(as.numeric(raised_amount_usd), na.rm = T), num_invest = length(investor_type))
invest.status <- arrange(invest.status, desc(sum_funds))
xtable(invest.status)

invest.status <- ddply(investments,.(funded_year), summarize, sum_funds= sum(as.numeric(raised_amount_usd), na.rm = T), startup.num = length(funded_year))
invest.status$sum_funds = invest.status$sum_funds / 1E7


p <- ggplot(invest.status, aes(x = funded_year, y = sum_funds, color = startup.num, 1))+
  xlim(1987,2014)+
  geom_line(size = 2)+
  geom_point()+
  theme(panel.background = element_blank(), text = element_text(size = 15), axis.line = element_line(size = 0.8, colour = "grey75"))+
  scale_color_gradient ( limits= c(0 , 10000 ) , high ="red" , low = "blue", name = "Startup Number")+
  labs(x= "Years from 1987 to 2015", y = "investmenst in USD")+
  ggtitle("Trends of Investments v/s Years")
ggsave(filename = "plot.jpg", plot = p)


## Status of the companies

comp.status = ddply ( companies, .(status),summarize,currentstatus = length(status))
comp.status$status = toupper(comp.status [ ,1 ] )
comp.status = comp.status [order(comp.status [ , 2 ] , decreasing = TRUE) , ]
row.names ( comp.status ) = NULL

print(xtable(comp.status , digits =0,
                caption ="\\textbf{Status of Companies }" , label ="ByStatus_Table", size ="footnotesize") , #Change size ; useful for bigger tables
       include.rownames=FALSE, 
       hline.after=NULL,  
      add.to.row = list ( pos = list (-1,
                                       nrow (comp.status)) ,
                           command = c (paste("\\ top rule \n " ,
                                            "Status of the Company & Number of Companies \\\\\n " ,
                                              "\\ mid rule \n " ) ,
                                        "\\ bottom rule \n " )
      )
)

print(comp.status)



comp.status = ddply ( companies, .(market), summarize,
                      NumofOper = length(which ( status =="operating")), 
                      NumofIpo = length(which ( status =="ipo")) ,
                      NumofAcq = length(which ( status =="acquired")) ,
                      NumofClosed = length(which ( status =="closed")))
comp.status = comp.status[order(comp.status[,2] , decreasing = TRUE),]
row.names (comp.status) = NULL
print(comp.status) 



# Plot 2 Heatmap 1
# Heatmap : Hottest region and types of companies


ind.geo = which ( companies$market %in%
                    comp.status$market [ 1 : 10 ] == TRUE)
comp.region = companies [ ind.geo , ]
comp.status = ddply ( comp.region , .(region) , summarize , Total = length (region))
comp.status = comp.status [ order ( comp.status [ ,2 ] , decreasing = TRUE) , ]
row.names ( comp.status ) = NULL
ind.geo = which (comp.region$region %in% comp.status$region [ 1 : 10 ] == TRUE)
comp.status = ddply ( comp.region [ ind.geo , ] ,
                      . ( region , market ) , summarize ,
                      Num.of.Comp = length ( which ( status == "operating") ) +
                        length ( which ( status == "ipo" ) ) +
                        length ( which ( status == "acquired" )))
p2 <-ggplot ( comp.status , aes ( y = market , x = region , fill = Num.of.Comp ) ) +
  geom_tile ( colour = "white" ) +
  scale_fill_gradient ( low = "steelblue" , high = "red" , name = "# Companies" ) +
  ylab ( "Various Types of Companies" ) +
  
  xlab ( "Regions" ) +
  ggtitle ( label = "Number of Companies based on location") +
  theme ( text = element_text ( size =5) ,
          axis.text.x = element_text ( vjust =1, color = "black") ,
          axis.text.y = element_text( vjust =1, color = "black" ))

ggsave(filename="b.jpg", plot=p2)


#Plot 3 Heatmap 2
# Heatmap : Types of companies in terms of years

## In terms of years
ind.geo = which ( comp.region$founded_year > 1986 )
comp.status = comp.region [ ind.geo, ]
comp.status = ddply ( comp.region [ ind.geo, ] ,
                      . ( founded_year , market ) ,
                      summarize , Num.of.Comp = length ( founded_year ))
p3 <-ggplot ( comp.status ,
              aes ( y = market , x = founded_year , fill = Num.of.Comp )) +
  geom_tile ( colour = "white" ) +
  scale_fill_gradient( low = "steelblue" , high = "red" , name = "#??? s Companies" ) +
  xlab ( "Years" ) + ylab ( "Various Types of Companies" ) +
  ggtitle (label = "Number of Companies based on Years" ) +
  theme ( text = element_text ( size =5) ,
          axis.text.x = element_text ( vjust =1, color = "black") ,
          axis.text.y = element_text( vjust =1, color = "black" ))

ggsave(filename="c.jpg", plot=p3)
## Status of the companies

comp.status = ddply ( companies, .(status),summarize,currentstatus = length(status))
comp.status$status = toupper(comp.status [ ,1 ] )
comp.status = comp.status [order(comp.status [ , 2 ] , decreasing = TRUE) , ]
row.names ( comp.status ) = NULL

print( xtable ( comp.status , digits =0,
                caption ="\\textbf{Status of Companies }" , label ="ByStatus_Table" ) , size ="footnotesize" , #Change size ; useful for bigger tables
       include.rownames=FALSE, 
       include.colnames=FALSE,
       hline.after=NULL,  
       add.to.row = list ( pos = list (-1,
                                       nrow (comp.status)) ,
                           command = c (paste("\\ top rule \n " ,
                                              "Status of the Company & Number of Companies \\\\\n " ,
                                              "\\ mid rule \n " ) ,
                                        "\\ bottom rule \n " )
       )
)

print(comp.status)   


comp.status = ddply ( companies, .(market), summarize,
                      NumofOper = length(which ( status =="operating")), 
                      NumofIpo = length(which ( status =="ipo")) ,
                      NumofAcq = length(which ( status =="acquired")) ,
                      NumofClosed = length(which ( status =="closed")))
comp.status = comp.status[order(comp.status[,2] , decreasing = TRUE),]
row.names (comp.status) = NULL
print(comp.status) 



# Plot 2 Heatmap 1
# Heatmap : Hottest region and types of companies


ind.geo = which ( companies$market %in%
                    comp.status$market [ 1 : 10 ] == TRUE)
comp.region = companies [ ind.geo , ]
comp.status = ddply ( comp.region , .(region) , summarize , Total = length (region))
comp.status = comp.status [ order ( comp.status [ ,2 ] , decreasing = TRUE) , ]
row.names ( comp.status ) = NULL
ind.geo = which (comp.region$region %in% comp.status$region [ 1 : 10 ] == TRUE)
comp.status = ddply ( comp.region [ ind.geo , ] ,
                      . ( region , market ) , summarize ,
                      Num.of.Comp = length ( which ( status == "operating") ) +
                        length ( which ( status == "ipo" ) ) +
                        length ( which ( status == "acquired" )))
p2 <-ggplot ( comp.status , aes ( y = market , x = region , fill = Num.of.Comp ) ) +
  geom_tile ( colour = "white" ) +
  scale_fill_gradient ( low = "steelblue" , high = "red" , name = "# Companies" ) +
  ylab ( "Various Types of Companies" ) +
  
  xlab ( "Regions" ) +
  ggtitle ( label = "Number of Companies based on location") +
  theme ( text = element_text ( size =5) ,
          axis.text.x = element_text ( vjust =1, color = "black") ,
          axis.text.y = element_text( vjust =1, color = "black" ))

ggsave(filename="b.jpg", plot=p2)


#Plot 3 Heatmap 2
# Heatmap : Types of companies in terms of years

## In terms of years
ind.geo = which ( comp.region$founded_year > 1986 )
comp.status = comp.region [ ind.geo, ]
comp.status = ddply ( comp.region [ ind.geo, ] ,
                      . ( founded_year , market ) ,
                      summarize , Num.of.Comp = length ( founded_year ))
p3 <-ggplot ( comp.status ,
              aes ( y = market , x = founded_year , fill = Num.of.Comp )) +
  geom_tile ( colour = "white" ) +
  scale_fill_gradient( low = "steelblue" , high = "red" , name = "#??? s Companies" ) +
  xlab ( "Years" ) + ylab ( "Various Types of Companies" ) +
  ggtitle (label = "Number of Companies based on Years" ) +
  theme ( text = element_text ( size =5) ,
          axis.text.x = element_text ( vjust =1, color = "black") ,
          axis.text.y = element_text( vjust =1, color = "black" ))

ggsave(filename="c.jpg", plot=p3)

##########################################################################################
##    Histogram 1

investor.category <- ddply(investments,.(investor_name,investor_region,investor_country_code,investor_type), summarize, sum_funds = sum(as.numeric(raised_amount_usd), na.rm = T), num_invest = length(investor_type))
investor.category <- arrange(investor.category, desc(sum_funds))


### top 20 investor

investor.top20 <- head(investor.category, 20)
# top invested company

invested.category <- ddply(investments,.(company_market), summarize, sum_funds = sum(as.numeric(raised_amount_usd), na.rm = T), num_invest = length(company_market))
invested.category <- arrange(invested.category, desc(sum_funds))

## top most invested compan


top20.index <- head(invested.category, 10)$company_market
invested.top20 <- subset(investments, company_market %in% top20.index)

temp1 <- ddply(invested.top20,.(company_market, funding_round_type, investor_type), summarize, num_funds = length(company_market))
#temp1[temp1$investor_type == "person"]$investor_type = "person & company"
#temp1[temp1$investor_type == "company"]$investor_type = "person & company"
### histogram

p1 <- ggplot(temp1, aes(reorder(company_market, -num_funds), num_funds))+
  geom_bar(aes(fill=funding_round_type), stat = "identity")+
   scale_fill_brewer(palette = "Specteral")+
   theme(panel.background = element_blank(), axis.line = element_line(size = 0.8, colour = "grey75"), text = element_text(size = 15), axis.text.x = element_text(angle = 70, hjust = 1))+
   labs(x = "company_category",y= "count")+
   scale_fill_discrete(name = "Fund type")+
   facet_wrap(~investor_type)
ggsave(filename = "plot4.jpg", plot = p1)

temp <- ddply(invested.top20,.(company_market, funding_round_type, investor_type), summarize, sum_funds = sum(as.numeric(raised_amount_usd), na.rm = T))
temp$sum_funds = temp$sum_funds/ 1E7

p2 <- ggplot(temp, aes(reorder(company_market, -sum_funds), sum_funds))+
  geom_bar(aes(fill=funding_round_type), stat = "identity")+
  scale_fill_brewer(palette = "Specteral")+
  theme(panel.background = element_blank(), axis.line = element_line(size = 0.8, colour = "grey75"), text = element_text(size = 15), axis.text.x = element_text(angle = 70, hjust = 1))+
  labs(x = "company_category", y = "Investment")+
  scale_fill_discrete(name = "Fund type")+
  facet_wrap(~investor_type)
ggsave(filename = "plot5.jpg", plot = p2)

#pushViewport(viewport(layout = grid.layout(1,2)))

#print(p1 + ggtitle("Histogram of Top20 invested company\n which receive most round"))
#vp = viewport(layout.pos.row = 1, layout.pos.col = 1)


#print(p2 + ggtitle("Histogram of Top20 invested company\n which receive  largest fund"))
#vp = viewport(layout.pos.row = 1, layout.pos.col = 2)


########################temp#####################################################################################################################################################
#Number of rounds companies receive Versus Companies ??? names
rounds <- unique(rounds) 
rounds <- subset(rounds, company_name != "Army & air force exchange")
rounds.status <- ddply(rounds,.(company_name), summarize, sum_funds = sum(as.numeric(raised_amount_usd), na.rm = T), num_invest = length(company_name))

top20.index1 <- head(arrange(rounds.status,desc(num_invest)),20)$company_name
rounds.top20 <- subset(rounds, company_name %in% top20.index1)
rounds.temp1 <- ddply(rounds.top20,.(company_name, funding_round_type), summarize, num_invest = length(company_name))

top20.index2 <- head(arrange(rounds.status,desc(sum_funds)),20)$company_name
rounds2.top20 <- subset(rounds, company_name %in% top20.index2) 
rounds.temp2 <- ddply(rounds2.top20,.(company_name, funding_round_type), summarize, num_invest = length(company_name), sum_invest = sum(as.numeric(raised_amount_usd), na.rm = T))


## histogram plot2
p3 <- ggplot(rounds.temp1, aes(reorder(company_name, -num_invest), num_invest))+
  geom_bar(aes(fill = funding_round_type), stat = "identity")+
  scale_fill_discrete(name = "Fund Type")+
  theme(panel.background = element_blank(), axis.line = element_line(size = 0.8, colour = "grey75"), text = element_text(size = 15), axis.text.x = element_text(angle = 70, hjust = 1))+
  labs(x = "companies", y = "Rounds")
ggsave(filename = "plot6.jpg", plot = p3)

p4 <- ggplot(rounds.temp2, aes(reorder(company_name, -num_invest), num_invest))+
  geom_bar(aes(fill = funding_round_type), stat = "identity")+
  scale_fill_discrete(name = "Fund Type")+
  theme(panel.background = element_blank(), axis.line = element_line(size = 0.8, colour = "grey75"), text = element_text(size = 15), axis.text.x = element_text(angle = 70, hjust = 1))+
  labs(x = "companies", y = "Rounds")
ggsave(filename = "plot7.jpg", plot = p4)

#pushViewport(viewport(layout = grid.layout(1,2)))

#print(p1 + ggtitle("Histogram of Top20 invested company\n which receive most round"))
#vp = viewport(layout.pos.row = 1, layout.pos.col = 1)


#print(p2 + ggtitle("Histogram of Top20 invested company\n which receive  largest fund"))
#vp = viewport(layout.pos.row = 1, layout.pos.col = 2)

#############################################################################################################################################################################
#prepare map data
data(us.cities)
us.cities$name = str_replace_all(us.cities$name, "(.*)", "")

companies1 <- merge(companies, us.cities, by.x = "city", by.y = "name")

companies1$funding_total_usd <- as.numeric(companies1$funding_total_usd, na.rm = T)
companies1$state_code = "SD"
companies1 <- subset(companies1, name != "Army And Air Force Exchange Service")

state.fund <- ddply(companies1,.(state_code), summarize, num_fund = sum(as.numeric(raised_amount_usd), na.rm = T), num_length = length(city))
state.fund <- arrange(state.fund, desc(num_fund))


map <- map_data("state")
state <- read.csv("state.csv", header = TRUE, stringsAsFactors = F)
center <- read.csv("center.csv", header = TRUE, stringsAsFactors = F)

state$lon= geocode(state$state_code)$lon
state$lat= geocode(state$state_code)$lat
write.csv(state, file = "center.csv")

state$region = tolower(state$region)
state.fund1 <- merge(state.fund, state, by = "state_code")
df <- join(state.fund1, map, type = "full", by = "region" )
df$num_fund <- df$num_fund/1E7
q <- ggplot(aes(long, lat), data = map_data('state'))+
  geom_polygon(aes(group= group), color ="black", fill = I('grey100'))+
  coord_map("bonne", 40)+
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white"), axis.text = element_blank(), axis.ticks = element_blank())+
  labs(x = "", y= "")


#############################################################################################################################################
df$bin <- cut(df$num_fund, breaks =c(0,10,50,100,500,1000,2000,3000), include.lowest =T)

q + geom_polygon(data =df, aes(group = group, fill = df$bin))+
  scale_fill_brewer("Funds received by  state", palette = "Reds")+
  theme(text = element_text(size = 20))+
  ggtitle("Funds Recived by states")+
  geom_text(aes(lon,lat,label = state_code), data = center)+
  coord_map("bonne", 31)

##########################################################################################################################################
geo.comp <- ddply(companies1,.(city, state_code, status, lat, lon, pop), summarize, num_fund = sum(as.numeric(raised_amount_usd), na.rm = T), num_length = length(city))
geo.comp1 <- subset(geo.comp, geo.comp$state_code != "AK" & geo.comp$state_code != "HI")  

q + geom_polygon(data =df, aes(group = group, fill = df$bin))+
  scale_fill_brestwer("Funds received by  state", palette = "Reds")+
  theme(text = element_text(size = 20))+
  ggtitle("Funds Recived by states and Most invested cities")+
  geom_jitter(data = geo.comp1, position = position_jitter(width = 0.8,height = 0.8), aes(x= lon, y = lat, color = status))+
  scale_area(range = c(2,6))+
  geom_text(aes(lon,lat,label= state_code), data = center, color = "white", size= 5)

############################################################################################################################################
##investor performance
colnames(companies)[2]="company_name"
comp.invest = merge(companies[,c("company_name", "category_code", "status", "funding_rounds")], investments[,c("company_name", "investor_name", "company_category_code", "funding_round_type")], by = "company_name", all = TRUE)
comp.status = ddply(comp.invest,.(investor_name, status), summarize, Total = length(status))
comp.status = arrange(comp.status, desc(Total))
ind.na = which(is.na(comp.status$investor_name) == TRUE)
comp.status = comp.status[-ind.na,]
top.investor = comp.status[1:20,1]
ind.invest = which(comp.status$investor_name %in% top.investor == TRUE)
comp.status = comp.status[ind.invest,]

ggplot(comp.status, aes(x= reorder(investor_name, -Total), y= Total))+
  geom_bar(aes(fill = status, order = desc(status)), stat = "identity")+
  theme(panel.background = element_blank(),legend.title = element_blank(), axis.text.x = element_text(angle = 70, hjust = 1))+
  labs(x="investor", y = "Total Number of Companies")+
  ggtitle("top 20 investor")+
  theme(text = element_text(size = 20), axis.text.x = element_text(vjust = 1, color = "black"), axis.text.y = element_text(color = "black"))

############################################################################################################################################################################

  
  





 




































                         
                         