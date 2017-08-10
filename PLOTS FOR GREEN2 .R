green_buildings=read.csv("greenbuildings.csv")

green_buildings$cluster=factor(green_buildings$cluster)
attach(green_buildings)
#######
# CREATES NEW DATA SETS OF JUST GREEN/NON GREEN BUILDINGS
######
green_buildings_green=subset(green_buildings,green_rating==1)
green_buildings_not_green=subset(green_buildings,green_rating==0)
#######

#########
# Histogram plot of Rent (green vs not green)
########
green=median(green_buildings_green$Rent)
not=median(green_buildings_not_green$Rent)
figure=ggplot()
figure=figure+geom_histogram(data=green_buildings,aes(x= Rent))
figure=figure+geom_histogram(data=green_buildings_green,aes(x=Rent),fill='red')
figure=figure+geom_vline(xintercept = not,color='blue')
figure=figure+geom_vline(xintercept = green,color='red')
figure
##########
# END
#########

green_buildings_25_not=subset(green_buildings_not_green, size<=260000 & size>=240000)
green_buildings_25=subset(green_buildings_green, size<=260000 & size>=240000)

x= median(green_buildings_25_not$Rent)
median(green_buildings_25$Rent)
```
cat('refs',x)
print('')
median(green_buildings_green$Rent)

#########
# Regressions SPARSE=BEST ONE
#########
fit_all=glm(Rent~green_rating+size+empl_gr+leasing_rate+stories+age+class_a+class_b+net+amenities+cd_total_07+hd_total07+total_dd_07+Precipitation+Electricity_Costs+cluster_rent)
summary(fit_all)

fit_sparse=glm(Rent~green_rating+size+empl_gr+stories+age+class_a+class_b+net+amenities+cd_total_07+hd_total07+Electricity_Costs+cluster_rent)
summary(fit_sparse)


############
# Regression VS. guru estimates on green premium (NOT NEEDED)
#figure=ggplot()
#figure=figure+theme(panel.background = element_rect(fill = 'cyan3', colour = 'red'))
#figure=figure+labs(title = "Model Estimate VS. Guru Estimate")
#figure=figure+geom_col(aes(y=2.6,x=1),fill='red2')
#figure=figure+annotate('text',x=1,y=1,label="Excel 'guru' estimate")
#figure=figure+geom_col(aes(y=.806,x=2),fill='red2')
#figure=figure+annotate('text',x=2,y=.5,label="Model estimate")
#figure
###########

##########
# END 
##########

##########
# HIST OF LEASING RATE
##########

figure=ggplot()
figure=figure+geom_histogram(data=green_buildings,aes(x= leasing_rate),fill='snow3',color='white')
figure=figure+geom_histogram(data=green_buildings_green,aes(x=leasing_rate),fill='seagreen',color='black')
figure

#########
# END
#########

#########
# Correlation between rent and predictors
#########

cor(y=green_buildings$Rent,x=green_buildings$size)
cor(y=green_buildings$Rent,x=green_buildings$empl_gr)
cor(y=green_buildings$Rent,x=green_buildings$Rent)
cor(y=green_buildings$Rent,x=green_buildings$leasing_rate)
cor(y=green_buildings$Rent,x=green_buildings$stories)
cor(y=green_buildings$Rent,x=green_buildings$age)
cor(y=green_buildings$Rent,x=green_buildings$renovated)
cor(y=green_buildings$Rent,x=green_buildings$class_a)
cor(y=green_buildings$Rent,x=green_buildings$class_b)
cor(y=green_buildings$Rent,x=green_buildings$net)
cor(y=green_buildings$Rent,x=green_buildings$amenities)
cor(y=green_buildings$Rent,x=green_buildings$cd_total_07)
cor(y=green_buildings$Rent,x=green_buildings$hd_total07)
cor(y=green_buildings$Rent,x=green_buildings$total_dd_07)
cor(y=green_buildings$Rent,x=green_buildings$Precipitation)
cor(y=green_buildings$Rent,x=green_buildings$Gas_Costs)
cor(y=green_buildings$Rent,x=green_buildings$Electricity_Costs)
cor(y=green_buildings$Rent,x=green_buildings$cluster_rent)
#########
# END
#########

##############
# PLOTS TO SHOW THERE IS NO DIFFERENCE BETWEEN GREEN BUILDINGS AND NON GREEN BUILDINGS WHEN ACCOUNTING FOR VARIABLES POSITIVLY CORRLEATED WITH RENT
#############
figure=ggplot()
figure=figure+geom_point(data=green_buildings_not_green,aes(y=Rent,x=age),color='snow3')
figure=figure+geom_point(data=green_buildings_green,aes(y=Rent,x=age),color='olivedrab4')
figure

figure=ggplot()
figure=figure+geom_point(data=green_buildings_not_green,aes(y=Rent,x=cluster_rent),color='snow3')
figure=figure+geom_point(data=green_buildings_green,aes(y=Rent,x=cluster_rent),color='olivedrab4')
figure

figure=ggplot()
figure=figure+geom_point(data=green_buildings_not_green,aes(y=Rent,x=Electricity_Costs),color='snow3')
figure=figure+geom_point(data=green_buildings_green,aes(y=Rent,x=Electricity_Costs),color='olivedrab4')
figure

figure=ggplot()
figure=figure+geom_point(data=green_buildings_not_green,aes(y=Rent,x=leasing_rate),color='snow3')
figure=figure+geom_point(data=green_buildings_green,aes(y=Rent,x=leasing_rate),color='olivedrab4')
figure


#########
# TRYING TO ADD A LEGEND :(
#########
green=median(green_buildings_green$Rent)
not=median(green_buildings_not_green$Rent)
figure=ggplot()
figure=figure+geom_histogram(data=green_buildings,aes(x= Rent))
figure=figure+geom_histogram(data=green_buildings_green,aes(x=Rent),fill='red')
figure=figure+geom_vline(xintercept = not,color='blue')
figure=figure+geom_vline(xintercept = green,color='red')
figure

############
# HIST. WITH 2 LEGENDS :)
############
figure=ggplot()+ ggtitle('Rent Vs. Age')+
  geom_histogram(data=green_buildings_not_green, aes(x=Rent, fill='Non green buildings'))+
  geom_histogram(data=green_buildings_green,aes(x=Rent,fill='Green Buildings'))+geom_vline(aes(xintercept = not,color='Non green buildings median rent'))+geom_vline(aes(xintercept = green,color='Green buildings median rent'))+
  scale_fill_manual(name="Bar Color",
                      values=c('Non green buildings'='snow3', 'Green Buildings'='olivedrab4','Non green buildings median rent'='dimgray', 'Green buildings median rent'='darkseagreen2'),guide='legend')+
  scale_color_manual(name="Line Color",
                     values=c('Non green buildings median rent'='dimgray', 'Green buildings median rent'='darkseagreen2'),guide='legend')
figure
##########
# END
##########

##########
# CONTINUE PLOTS WITH LEGENDS
########
figure=ggplot()+ ggtitle('Rent Vs. Age')+
  geom_point(data=green_buildings_not_green, aes(y=Rent,x=age, color='Non green buildings'))+
  geom_point(data=green_buildings_green,aes(y=Rent,x=age,color='Green Buildings'))+
  scale_colour_manual(name="Dot Color",
                      values=c('Non green buildings'='snow3', 'Green Buildings'='olivedrab4'),guide='legend')
figure

figure=ggplot()+ ggtitle('Rent Vs. Cluster Rent')+
  geom_point(data=green_buildings_not_green, aes(y=Rent,x=cluster_rent, color='Non green buildings'))+
  geom_point(data=green_buildings_green,aes(y=Rent,x=cluster_rent,color='Green Buildings'))+
  scale_colour_manual(name="Dot Color",
                      values=c('Non green buildings'='snow3', 'Green Buildings'='olivedrab4'),guide='legend')
figure

figure=ggplot()+ ggtitle('Rent Vs. Electricity Costs')+
  geom_point(data=green_buildings_not_green, aes(y=Rent,x=Electricity_Costs, color='Non green buildings'))+
  geom_point(data=green_buildings_green,aes(y=Rent,x=Electricity_Costs,color='Green Buildings'))+
  scale_colour_manual(name="Dot Color",
                      values=c('Non green buildings'='snow3', 'Green Buildings'='olivedrab4'),guide='legend')
figure

figure=ggplot()+ ggtitle('Rent Vs. Leasing_rate')+
  geom_point(data=green_buildings_not_green, aes(y=Rent,x=leasing_rate, color='Non green buildings'))+
  geom_point(data=green_buildings_green,aes(y=Rent,x=leasing_rate,color='Green Buildings'))+
  scale_colour_manual(name="Dot Color",
                    values=c('Non green buildings'='snow3', 'Green Buildings'='olivedrab4'),guide='legend')
figure
#########
#
########

##############
# END
##############


