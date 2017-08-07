green_buildings=read.csv("greenbuildings.csv")

#data sets with just LEED and Energystar buildings
green_buildings_leed=subset(green_buildings,LEED==1)
green_buildings_star=subset(green_buildings, Energystar==1)

library(ggplot2)


#histogram of building rent, blue= Energy star, red=LEED
figure=ggplot(green_buildings, aes(x=Rent))
figure=figure+geom_histogram()
figure=figure+geom_histogram(data=subset(green_buildings,Energystar==1),fill='blue')
figure=figure+geom_histogram(data=subset(green_buildings,LEED==1),fill='red')
figure



# histogram of building sizes of all buildings, energy star buildings, and leed buildings
figure=ggplot()
figure=figure+geom_histogram(data=green_buildings,aes(x=size),binwidth=250000)
figure=figure+geom_histogram(data=green_buildings_star,aes(x=size),fill='red',binwidth=250000)
figure=figure+geom_histogram(data=green_buildings_leed,aes(x=size),fill='blue',binwidth=250000)
figure=figure+scale_x_continuous(labels=scales::comma)
figure+scale_x_continuous(breaks = seq(0, 4000000, by = 250000))
figure


# point plot of rent vs cluster rent, and if the Rent>greater cluster rent
green_buildings_leed_rent_high=subset(green_buildings_leed,Rent_higher_cluster==TRUE)
green_buildings_star_rent_low=subset(green_buildings_leed,Rent_higher_cluster==FALSE)

green_buildings_leed$Rent_higher_cluster=green_buildings_leed$Rent>green_buildings_leed$cluster_rent
figure=ggplot()
figure=figure+geom_point(data=green_buildings_leed,aes(x=Rent,y=cluster_rent,colour=Rent_higher_cluster==TRUE))
figure  

# new datasets for rent>cluster and rent<cluster
green_buildings_star_rent_high=subset(green_buildings_star,Rent_higher_cluster==TRUE)
green_buildings_star_rent_low=subset(green_buildings_star,Rent_higher_cluster==FALSE)

# red line=median rent price of red dots
# blue line=median rent price of red dots 
median_rent_high=median(green_buildings_star_rent_high$Rent)
median_rent_low=median(green_buildings_star_rent_low$Rent)

# point plot of rent vs cluster rent
# blue dots are if Rent>cluster rent
# red dots are if Rent<cluster Rent

green_buildings_star$Rent_higher_cluster[green_buildings_star$Rent_higher_cluster==TRUE]=TRUE
green_buildings_star$Rent_higher_cluster[green_buildings_star$Rent_higher_cluster==FALSE]=FALSE

green_buildings_star$Rents_higher_than_average=green_buildings_star$Rent>green_buildings_star$cluster_rent
figure=ggplot()
figure=figure+geom_point(data=green_buildings_star,aes(x=Rent,y=cluster_rent,colour=Rents_higher_than_average==TRUE))
figure=figure+geom_vline(xintercept=median_rent_low,color='red')
figure=figure+geom_vline(xintercept=median_rent_high,color='cyan3')
figure=figure+scale_x_continuous(breaks=c(median_rent_high,median_rent_low))
figure
# median rent for green apartments who were above their market rents was about 30
# while the median rent fo green apartments who were below their market rents was 20

figure=ggplot()
figure=figure+geom_histogram(aes(x=green_buildings_star_rent_high$size))
figure+geom_histogram(aes(x=green_buildings_star_rent_low$size),fill='red')

# a bunch of pairwise plots
pairs(green_buildings_star$Rent~green_buildings_star$cluster+green_buildings_star$size+green_buildings_star$empl_gr+green_buildings_star$leasing_rate+green_buildings_star$stories+green_buildings_star$age)
pairs(green_buildings_star$Rent~green_buildings_star$renovated+green_buildings_star$class_a+green_buildings_star$class_b+green_buildings_star$net+green_buildings_star$amenities+green_buildings_star$cd_total_07)
pairs(green_buildings_star$Rent~green_buildings_star$hd_total07+green_buildings_star$total_dd_07+green_buildings_star$Precipitation+green_buildings_star$Gas_Costs+green_buildings_star$Electricity_Costs)

#green buildings =red, non green=blue. Y=rent, x=other varaibles
#looking for "green only" relationships against non green buildings
green_buildings_not_green=subset(green_buildings,Energystar==0 & LEED==0)

median_not_green=median(green_buildings_not_green$Rent)
median_green=median(green_buildings_star$Rent)

figure=ggplot()
figure=figure+geom_histogram(data=green_buildings_not_green,aes(x=Rent),color='blue')
figure=figure+geom_histogram(data=green_buildings_star,aes(x=Rent),color='red')
figure=figure+geom_vline(xintercept = median_not_green,color='blue')
figure+geom_vline(xintercept = median_green,color='red')

figure=ggplot()
figure=figure+geom_point(data=green_buildings_not_green,aes(y=Rent,x=cluster),color='blue')
figure+geom_point(data=green_buildings_star,aes(y=Rent,x=cluster),color='red')

figure=ggplot()
figure=figure+geom_point(data=green_buildings_not_green,aes(y=Rent,x=size),color='blue')
figure+geom_point(data=green_buildings_star,aes(y=Rent,x=size),color='red')

figure=ggplot()
figure=figure+geom_point(data=green_buildings_not_green,aes(y=Rent,x=empl_gr),color='blue')
figure+geom_point(data=green_buildings_star,aes(y=Rent,x=empl_gr),color='red')

figure=ggplot()
figure=figure+geom_point(data=green_buildings_not_green,aes(y=Rent,x=leasing_rate),color='blue')
figure+geom_point(data=green_buildings_star,aes(y=Rent,x=leasing_rate),color='red')

figure=ggplot()
figure=figure+geom_point(data=green_buildings_not_green,aes(y=Rent,x=stories),color='blue')
figure+geom_point(data=green_buildings_star,aes(y=Rent,x=stories),color='red')

figure=ggplot()
figure=figure+geom_point(data=green_buildings_not_green,aes(y=Rent,x=age),color='blue')
figure+geom_point(data=green_buildings_star,aes(y=Rent,x=age),color='red')

figure=ggplot()
figure=figure+geom_point(data=green_buildings_not_green,aes(y=Rent,x=renovated),color='blue')
figure+geom_point(data=green_buildings_star,aes(y=Rent,x=renovated),color='red')

figure=ggplot()
figure=figure+geom_point(data=green_buildings_not_green,aes(y=Rent,x=class_a),color='blue')
figure+geom_point(data=green_buildings_star,aes(y=Rent,x=class_a),color='red')

figure=ggplot()
figure=figure+geom_point(data=green_buildings_not_green,aes(y=Rent,x=class_b),color='blue')
figure+geom_point(data=green_buildings_star,aes(y=Rent,x=class_b),color='red')

figure=ggplot()
figure=figure+geom_point(data=green_buildings_not_green,aes(y=Rent,x=net),color='blue')
figure+geom_point(data=green_buildings_star,aes(y=Rent,x=net),color='red')

figure=ggplot()
figure=figure+geom_point(data=green_buildings_not_green,aes(y=Rent,x=amenities),color='blue')
figure+geom_point(data=green_buildings_star,aes(y=Rent,x=amenities),color='red')

figure=ggplot()
figure=figure+geom_point(data=green_buildings_not_green,aes(y=Rent,x=cd_total_07),color='blue')
figure+geom_point(data=green_buildings_star,aes(y=Rent,x=cd_total_07),color='red')

figure=ggplot()
figure=figure+geom_point(data=green_buildings_not_green,aes(y=Rent,x=hd_total07),color='blue')
figure+geom_point(data=green_buildings_star,aes(y=Rent,x=hd_total07),color='red')

figure=ggplot()
figure=figure+geom_point(data=green_buildings_not_green,aes(y=Rent,x=total_dd_07),color='blue')
figure+geom_point(data=green_buildings_star,aes(y=Rent,x=total_dd_07),color='red')

figure=ggplot()
figure=figure+geom_point(data=green_buildings_not_green,aes(y=Rent,x=Precipitation),color='blue')
figure+geom_point(data=green_buildings_star,aes(y=Rent,x=Precipitation),color='red')

figure=ggplot()
figure=figure+geom_point(data=green_buildings_not_green,aes(y=Rent,x=Gas_Costs),color='blue')
figure+geom_point(data=green_buildings_star,aes(y=Rent,x=Gas_Costs),color='red')

figure=ggplot()
figure=figure+geom_point(data=green_buildings_not_green,aes(y=Rent,x=cluster),color='blue')
figure+geom_point(data=green_buildings_star,aes(y=Rent,x=cluster),color='red')

figure=ggplot()
figure=figure+geom_point(data=green_buildings_not_green,aes(y=Rent,x=Electricity_Costs),color='blue')
figure+geom_point(data=green_buildings_star,aes(y=Rent,x=Electricity_Costs),color='red')

figure=ggplot()
figure=figure+geom_point(data=green_buildings_not_green,aes(y=Rent,x=cluster_rent),color='blue')
figure+geom_point(data=green_buildings_star,aes(y=Rent,x=cluster_rent),color='red')

# graphs of x=green star y=other varaibles
# looking for green only trends
figure=ggplot()
figure+geom_point(data=green_buildings,aes(y=Energystar, x=cluster))

figure=ggplot()
figure+geom_point(data=green_buildings,aes(y=Energystar, x=size))

figure=ggplot()
figure+geom_point(data=green_buildings,aes(y=Energystar, x=empl_gr))

figure=ggplot()
figure+geom_point(data=green_buildings,aes(y=Energystar, x=Rent))

figure=ggplot()
figure+geom_point(data=green_buildings,aes(y=Energystar, x=leasing_rate))

figure=ggplot()
figure+geom_point(data=green_buildings,aes(y=Energystar, x=stories))

figure=ggplot()
figure+geom_point(data=green_buildings,aes(y=Energystar, x=age))

figure=ggplot()
figure+geom_point(data=green_buildings,aes(y=Energystar, x=renovated))

figure=ggplot()
figure+geom_point(data=green_buildings,aes(y=Energystar, x=class_a))

figure=ggplot()
figure+geom_point(data=green_buildings,aes(y=Energystar, x=class_b))

figure=ggplot()
figure+geom_point(data=green_buildings,aes(y=Energystar, x=net))

figure=ggplot()
figure+geom_point(data=green_buildings,aes(y=Energystar, x=amenities))

figure=ggplot()
figure+geom_point(data=green_buildings,aes(y=Energystar, x=cd_total_07))

figure=ggplot()
figure+geom_point(data=green_buildings,aes(y=Energystar, x=hd_total07))

figure=ggplot()
figure+geom_point(data=green_buildings,aes(y=Energystar, x=total_dd_07))

figure=ggplot()
figure+geom_point(data=green_buildings,aes(y=Energystar, x=Precipitation))

figure=ggplot()
figure+geom_point(data=green_buildings,aes(y=Energystar, x=Gas_Costs))

figure=ggplot()
figure+geom_point(data=green_buildings,aes(y=Energystar, x=Electricity_Costs))

figure=ggplot()
figure+geom_point(data=green_buildings,aes(y=Energystar, x=cluster_rent))




