#change directory
setwd("C:/Users/Acer/Desktop/projects/R Data Challenge/data/")
#read files
orders=read.csv(file="Orders.csv")
returns=read.csv("Returns.csv")

#view as table
View(orders)
View(returns)

#check the datatypes of the columns
sapply(orders,class)

#extract profit and sales and convert
profit=as.numeric(orders$Profit)
sales=as.numeric(orders$Sales)

#convert directly
orders[,"Profit"]=as.numeric(orders[,"Profit"])
orders[,"Sales"]=as.numeric(orders[,"Sales"])


#EDA
library(ggplot2)
ggplot(data=orders, aes(x=Market))+geom_bar(aes(fill=Segment),position="dodge")

#load dplyr so we can group and summarise
library(dplyr)
by_market=group_by(orders, Market)
profit_by_market=summarise(by_market, total_profit=sum(Profit))
profit_by_market=arrange(profit_by_market,desc(total_profit))

sales_by_market=summarise(by_market, total_sales=sum(Sales))
sales_by_market=arrange(sales_by_market,desc(total_sales))

sales_by_market
profit_by_market

#create new column
orders$profit_margin=orders$Profit/orders$Sales
by_market=group_by(orders, Market)
pm_by_market=summarise(by_market, ave_pm=mean(profit_margin))
pm_by_market=arrange(pm_by_market,desc(ave_pm))
pm_by_market

#because of the margins, we now check what kind of product categpry is most popular in each market
#and get the average profit margin per product category
by_cat=group_by(orders, Category)
pm_by_cat=summarise(by_cat, ave_pm=mean(profit_margin))
pm_by_cat=arrange(pm_by_cat,desc(ave_pm))
pm_by_cat

by_mkt_and_cat=group_by(orders, Market,Category)
pm_by_grp=summarise(by_mkt_and_cat, ave_pm=mean(profit_margin))
pm_by_grp
sales_by_grp=summarise(by_mkt_and_cat, total_sales=sum(Sales))
sales_by_grp

ggplot(data=orders, aes(x=Market))+geom_bar(aes(fill=Category),position="fill")


### PROBLEM 2
#First converted dates to Date Format (for both order and shipping dates).
orders$Order.Date=as.Date(orders$Order.Date,"%m/%d/%y")
orders$Ship.Date=as.Date(orders$Order.Date,"%m/%d/%y")
#From class "Factor" to class "Date"

#sum sales by Order.Date then plot
by_date=group_by(orders, Order.Date)
sales_by_date=summarise(by_date, sales=sum(Sales))
g=ggplot(sales_by_date, aes(x=Order.Date,y=sales))
g+geom_point(alpha=0.7,color="orange")+geom_smooth(method=lm)

#check if there is seasonality per category
tech=filter(orders, Category=="Technology")
by_date=group_by(tech, Order.Date)
sales_by_date=summarise(by_date, sales=sum(Sales))
g=ggplot(sales_by_date, aes(x=Order.Date,y=sales))
g+geom_point(size=0.5,alpha=0.7,color="blue")+geom_smooth(color="red")+ggtitle("Tech Category")

furniture=filter(orders, Category=="Furniture")
by_date=group_by(furniture, Order.Date)
sales_by_date=summarise(by_date, sales=sum(Sales))
g=ggplot(sales_by_date, aes(x=Order.Date,y=sales))
g+geom_point(size=0.5,alpha=0.7,color="darkgreen")+geom_smooth(color="red")+ggtitle("Furniture Category")

office_supplies=filter(orders, Category=="Office Supplies")
by_date=group_by(office_supplies, Order.Date)
sales_by_date=summarise(by_date, sales=sum(Sales))
g=ggplot(sales_by_date, aes(x=Order.Date,y=sales))
g+geom_point(size=0.5,alpha=0.7,color="violet")+geom_smooth(color="red")+ggtitle("Office Supplies Category")

#merge returns and orders files by Order.ID
joined=inner_join(orders,returns,by="Order.ID")

#compute profit losses
sum(joined$Profit)
#add year to compute losses per year
joined$year=format(joined$Order.Date,"%Y")
by_year=group_by(joined,year)
losses_by_year=summarise(by_year, losses=sum(Profit))
losses_by_year
#plot losses by year
ggplot(losses_by_year,aes(x=year,y=losses))+geom_col()+geom_text(aes(label=format(losses,big.mark=",")),vjust=-1,size=4)+theme(axis.text.y=element_blank())+ylim(0,8000000)


#how many customers return more than once?
joined$count=1
by_customer=group_by(joined,Customer.Name)
summary1=summarise(by_customer,reps=sum(count))
max(summary1$reps)
ggplot(summary1, aes(x=reps))+geom_bar()+geom_text(stat='count',aes(label=..count..),vjust=-1)+ylim(0,170)

#more than once
summary2=summary1[summary1$reps>1,]
nrow(summary2)

#more than 10x
summary3=summary1[summary1$reps>10,]
nrow(summary3)

#region-level analysis
by_region=group_by(joined,Region.x)
by_region_count=summarise(by_region,reps=sum(count))
by_region_count=by_region_count[order(-by_region_count$reps),]
top10=by_region_count[1:10,]
ggplot(top10,aes(x=reorder(Region.x,-reps),y=reps))+geom_col()+theme(text = element_text(size=12),axis.text.x = element_text(angle=90, hjust=1))+xlab("Region")+ylab("# of Customers w/ Returns")

#categories most likely to be returned
by_cat=group_by(joined,Category)
by_cat_count=summarise(by_cat,reps=sum(count))
ggplot(by_cat_count,aes(x=Category,y=reps))+geom_col()+ylab("# of Returns")

#filter for top Regions
by_cat=joined %>% select('Region.x','Category','Sub.Category','count') %>% filter(Region.x %in% c('Central America','Western Europe','Western US'))
ggplot(by_cat, aes(x=Region.x))+geom_bar(aes(fill=Category))+xlab("Region")+ylab("# of Returns")

#sub-categories most likely to be returned
ggplot(by_cat, aes(x=Region.x))+geom_bar(aes(fill=Sub.Category),position="fill")+xlab("Region")+ylab("# of Returns")

by_subcat=group_by(joined,Sub.Category)
by_subcat_count=summarise(by_subcat,reps=sum(count))
by_subcat_count=by_subcat_count[order(-by_subcat_count$reps),]
top10_subcat=by_subcat_count[1:10,]
ggplot(top10_subcat,aes(x=reorder(Sub.Category,-reps),y=reps))+geom_col()+xlab("SubCategory")+ylab("# of Returns")+theme(axis.text.x=element_text(angle=90))
top10_subcat

g=ggplot(by_cat,aes(Region.x, Sub.Category))
g+geom_count(alpha=0.5,color='red')+scale_size_area(max_size=10)