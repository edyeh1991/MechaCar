demo_table <- read.csv(file='demo.csv',check.names=F,stringsAsFactors = F)
library(jsonlite)
library(tidyverse)
demo_table2 <- fromJSON('demo.json')
?fromJSON
?read.csv
demo_table2[3,'year']
demo_table2[3,5]
x <- demo_table$Vehicle_Class
x <- demo_table$Vehicle_Class[2]
x
filter_table <- demo_table2[demo_table2$price >10000,]
?subset
filter_table2 <- subset(demo_table2, price > 10000 & drive == '4wd' & 'clean' %in% title_status)
?sample
sample(c("cow", "deer", "pig", "chicken", "duck", "sheep", "dog"), 4)
demo_table[sample(1:nrow(demo_table),3),]
?mutate()
demo_table <- demo_table %>% mutate(Mileage_per_Year = Total_Miles/(2020-Year),IsActive = TRUE)
summarize_demo <- demo_table2 %>% group_by(condition) %>% summarize(Mean_Mileage = mean(odometer),Max_Price = max(price),Num_Vehicles = n())
?gather
demo_table3 <- read.csv('demo2.csv', check.names = F, stringsAsFactors = F)
long_table <- demo_table3 %>% gather(key="Metric",value="Score",buying_price:popularity)
long_table <- 
0
long_table <- demo_table3 %>% gather(key="Metric",value="Score",buying_price:popularity)
wide_table <- long_table %>% spread(key='Metric',value='Score')
all.equal(demo_table3,wide_table)
demo_table3 <- demo_table3[,order(colnames(demo_table3))]
wide_table <- wide_table[,order(colnames(wide_table))]
?ggplot
head(mpg)
mpg
plt <- ggplot(mpg,aes(x=class))
plt + geom_bar()
mpg_summary <- mpg %>% group_by(manufacturer) %>% summarize(Vehicle_Count = n())
plt <- ggplot(mpg_summary,aes(x=manufacturer, y=Vehicle_Count))
plt+geom_col() + xlab('Manufacturing Company') + ylab("Number of Vehicle in Dataset")+ theme(axis.text.x = element_text(angle = 45,hjust = 1))
mpg_summary <- subset(mpg,manufacturer =='toyota')%>%group_by(cyl)%>%summarize(Mean_Hwy=mean(hwy))
plot <- ggplot(mpg_summary,aes(x=cyl, y=Mean_Hwy))
plot + geom_line() + scale_x_discrete(limits=c(4,6,8)) + scale_y_continuous(breaks = c(15:30))
plt <- ggplot(mpg,aes(x=displ,y=cty))
plt + geom_point() + xlab('Engine Size(L)') + ylab('City Fuel Efficiency (MPG)')
plt <- ggplot(mpg,aes(x=displ, y=cty, color=class))
plt + geom_point()+labs(x="Engine Size (L)",y="City Fuel Efficienty(MPG)", color = "Vehicle Class")
plt <- ggplot(mpg,aes(x=displ, y=cty, color=class, shape=drv, size = cty))
plt + geom_point() + labs(x="engine Size", y="City Fuel Efficienty",color = 'Vehicle Class', shape = 'Type of Drive', size = 'City Fuel')
plt <- ggplot(mpg,aes(x=manufacturer,y=hwy)) #import dataset into ggplot2
plt + geom_boxplot()
mpg_summary <- mpg %>% group_by(class,year) %>% summarize (Mean_Hwy=mean(hwy))
plt <- ggplot(mpg_summary, aes(x=class, y=factor(year),fill=Mean_Hwy))
plt + geom_tile() + labs(x="vehicle class", y = 'vehicle year', fill = 'Mean Highway(MPG)')
mpg_summary <- mpg%>%group_by(model,year)%>%summarize (mean_hwy=mean(hwy))
plt <- ggplot(mpg_summary, aes(x=model, y = factor(year), fill=mean_hwy))
plt + geom_tile() + labs(x='model', y='vehicle year', fill = 'Mean Highway (MPG)') + theme(axis.text.x=element_text(angle = 90, hjust = 1))
plt <- ggplot(mpg, aes(x=manufacturer,y=hwy))
plt + geom_boxplot()+
theme(axis.text.x=element_text(angle=45, hjust =1)) +geom_point()

mpg_summary <- mpg %>% group_by(class)%>%summarize (mean_engine = mean(displ))
plt <- ggplot(mpg_summary,aes(x=class, y=mean_engine))
plt + geom_point (size=4) + labs(x='vehicle class', y = 'mean engine size')

mpg_summary <- mpg %>% group_by(class)%>%summarize(mean_engine=mean(displ),SD_engine = sd(displ))
plt <- ggplot(mpg_summary, aes(x=class, y=mean_engine))
plt + geom_point(size = 4) + labs(x='vehicle class', y ='mean engine size') + geom_errorbar(aes(ymin=mean_engine-SD_engine,ymax = mean_engine+SD_engine))

mpg_long <- mpg %>% gather(key='MPG_Type', value="Rating", c(cty,hwy))
head(mpg_long)
head(mpg)

plt <- ggplot(mpg_long, aes(x=manufacturer, y=Rating, color = MPG_Type))
plt + geom_boxplot()+theme(axis.text.x=element_text(angle=45,hjust=1))

plt <- ggplot(mpg_long,aes(x=manufacturer, y=Rating,color=MPG_Type))
plt + geom_boxplot()+facet_wrap(vars(MPG_Type))+theme(axis.text.x=element_text(angle=45,hjust=1),legend.position = 'none')

ggplot(mtcars,aes(x=wt))+geom_density()
shapiro.test(mtcars$wt)

population_table <- read.csv('used_car_data.csv',check.names = F, stringsAsFactors = F)
plt <- ggplot(population_table,aes(x=log10(Miles_Driven)))
plt+geom_density()

sample_table <- population_table %>% sample_n(50)
plt <- ggplot(sample_table,aes(x=log10(Miles_Driven)))
plt + geom_density()

t.test(log10(sample_table$Miles_Driven),mu=mean(log10(population_table$Miles_Driven)))

sample_table<-population_table%>%sample_n(50)
sample_table2<-population_table%>%sample_n(50)
t.test(log10(sample_table$Miles_Driven),log10(sample_table2$Miles_Driven))

mpg_data<-read.csv('mpg_modified.csv')
mpg_1999<-mpg_data%>%filter(year==1999)
mpg_2008<-mpg_data%>%filter(year==2008)
t.test(mpg_1999$hwy,mpg_2008$hwy,paired = T)

mtcars_filt<-mtcars[,c('hp','cyl')]
mtcars_filt$cyl<-factor(mtcars_filt$cyl)
aov(hp~cyl,data=mtcars_filt)
summary(aov(hp~cyl,data=mtcars_filt))

head(mtcars)
plt<-ggplot(mtcars,aes(x=hp,y=qsec))
plt+geom_point()
cor(mtcars$hp,mtcars$qsec)
used_cars<-read.csv('used_car_data.csv',stringsAsFactors = F)
head(used_cars)
plt<-ggplot(used_cars,aes(x=Miles_Driven,y=Selling_Price))
plt + geom_point()
cor(used_cars$Miles_Driven,used_cars$Selling_Price)
used_matrix<-as.matrix(used_cars[,c('Selling_Price','Present_Price','Miles_Driven')])
cor(used_matrix)

lm(qsec~hp,mtcars)
summary(lm(qsec~hp,mtcars))

model <- lm(hp ~ qsec,mtcars)
yvals <- model$coefficients['hp']*mtcars$hp + model$coefficients['(Intercept)']
plt <- ggplot(mtcars,aes(x=hp,y=qsec))
plt + geom_point() + geom_line(aes(y=yvals), color = "red")

summary(lm(qsec~mpg+disp+drat+wt+hp,data=mtcars))


table(mpg$class,mpg$year)
tbl<-table(mpg$class,mpg$year)
chisq.test(tbl)
