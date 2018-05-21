library(rpart)				        # Popular decision tree algorithm
#library(rattle)					# Fancy tree plot
library(rpart.plot)				# Enhanced tree plots
library(RColorBrewer)				# Color selection for fancy tree plot
#library(party)					# Alternative decision tree algorithm
#library(partykit)				# Convert rpart object to BinaryTree
library(caret)					# Just a data source for this script
#library(mvnormtest)
library(plotly)
# but probably one of the best R packages ever. 
data(segmentationData)				# Get some data
data <- segmentationData[,-c(1,2)]
library(maptools)
library(MASS)
#------

  
  cls <- function() cat(rep("\n",100))

cls()


panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) 
{ 
  usr <- par("usr"); on.exit(par(usr)) 
  par(usr = c(0, 1, 0, 1)) 
  r <- abs(cor(x, y)) 
  txt <- format(c(r, 0.123456789), digits = digits)[1] 
  txt <- paste0(prefix, txt) 
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt) 
  text(0.5, 0.5, txt, cex = cex.cor * r) 
} 

#-Loading all commodities for the palouse 1989 - 2015

if (interactive() ){
state_i <- readline("State of interest?")  
comm_i <- readline("Commodity of interest?")
damage_i <- readline("Damage Cause of interest?")
}


PNW_sumloss_allcomm <- read.csv("/dmine/data/USDA/agmesh-scenarios/Allstates/summaries/PNW_summary_all.csv")
Southern_ID_sumloss <- subset(PNW_sumloss_allcomm, state == state_i)

Southern_ID_sumloss_all_sum  <- aggregate(loss ~ year + damagecause + county + commodity,  Southern_ID_sumloss, sum)
Southern_ID_count_all_count  <- aggregate(count ~ year + damagecause + county + commodity,  Southern_ID_sumloss, sum)

Southern_ID_sumloss_all_sum <- Southern_ID_sumloss_all_sum[Southern_ID_sumloss_all_sum$loss >= 1, ]


#-Loading all WHEAT claims for the palouse from 1989-2015

palouse_sumloss <- read.csv("/dmine/data/USDA/agmesh-scenarios/Allstates/summaries/Palouse_summary_sumloss.csv")
palouse_counts <- read.csv("/dmine/data/USDA/agmesh-scenarios/Allstates/summaries/Palouse_summary_counts.csv")
#--drought claim counts all counties 





SI_drought <- subset(Southern_ID_sumloss_all_sum, damagecause == damage_i)
SI_drought <- subset(SI_drought, commodity == comm_i)

SI_drought_count <- subset(Southern_ID_count_all_count, damagecause == damage_i)
SI_drought_count <- subset(SI_drought_count, commodity == comm_i)






#load wheat pricing

#barley <- read.csv("/dmine/data/USDAprices/barleyprices_1988_2017.csv", header=TRUE, strip.white =TRUE)
wheatprice <- read.csv("/dmine/data/USDAprices/wheatprices_1998_2017.csv", header=TRUE, strip.white =TRUE)
wheatprice_year <- aggregate(wheatprice$Price, list(wheatprice$Year), FUN="mean")
colnames(wheatprice_year) <- c("year", "price")

#merge wheat pricing with palouse_sumloss

SI_drought <- merge(SI_drought, wheatprice_year, by = "year")

#use a cube transformation on loss for WHEAT claims

Math.cbrt <- function(x) {
  sign(x) * abs(x)^(1/3)
}

#palouse_sumloss2 <- subset(palouse_sumloss, loss > 0)
Southern_ID_sumloss_all_sum$cube_loss <- Math.cbrt(Southern_ID_sumloss_all_sum$loss)
Southern_ID_count_all_count$cube_counts <- Math.cbrt(Southern_ID_count_all_count$count)


SI_drought$cube_loss <- Math.cbrt(SI_drought$loss)
SI_drought_count$cube_counts <- Math.cbrt(SI_drought_count$count)

#use a cube transformation on loss for all commodity claims

palouse_sumloss_allcomm2$cube_loss <- Math.cbrt(palouse_sumloss_allcomm2$loss)


#-use a log transform on the same WHEAT claims data

palouse_sumloss$log_loss <- log(which(!is.na(palouse_sumloss$loss)))

# - plot some qqplots to see how normal the data is

qqnorm(palouse_sumloss$loss)
qqnorm(palouse_sumloss$cube_loss)
qqnorm(palouse_sumloss$log_loss)
qqnorm(palouse_sumloss_allcomm2$cube_loss)
qqnorm(palouse_counts$count)

#box cox transformation


#-factor counties
palouse_sumloss$county = factor(palouse_sumloss$county,
                                levels=unique(palouse_sumloss$county))

#-factor years
#palouse_sumloss$year = factor(palouse_sumloss$year,
#                                levels=unique(palouse_sumloss$year))

#-plot basic interaction plots for WHEAT cube root loss using year as x and damagecause as the line
palouse_sumloss_onlydrought <- subset(palouse_sumloss, damagecause == "Drought")

palouse_sumloss_drought <- subset(palouse_sumloss, damagecause == "Drought" | damagecause == "Heat" | damagecause == "Decline in Price" | damagecause == "Cold Winter")
palouse_sumloss_drought$damagecause <- factor(palouse_sumloss_drought$damagecause)
data_loss_county <- subset(palouse_sumloss_onlydrought, county == "Latah" | county == "Umatilla" | county == "Whitman" | county == "Spokane" | county == "Adams" | county == "Lincoln" )
data_loss_county$county <- factor(data_loss_county$county)
options(scipen = 999)

print.million <- function(x, quote = FALSE, ...) {
  x <- paste0(round(x / 1e6, 1), "M")
  NextMethod(x, quote = quote, ...)
}

class(data_loss_county$loss) <- "million"

#pie chart for 2015

addNoAnswer <- function(x){
  if(is.factor(x)) return(factor(x, levels=c(levels(x), "No Answer")))
  return(x)
}


palouse_sumloss_2015 <- palouse_sumloss_2015[order(palouse_sumloss_2015$x),] 
X <- palouse_sumloss_2015[order(palouse_sumloss_2015$x),] 
colnames(X) <- c("damagecause", "loss")
summary_dataset <- X %>% filter(loss < 5000000) 
summary_dataset2 <- X %>% filter(loss > 5000000) 
colnames(summary_dataset) <- c("damagecause", "loss")
Y <- sum(summary_dataset$loss)
YY <- c("Other", Y)
YY <- data.frame(t(YY))
colnames(YY) <- c("damagecause", "loss")
YYY <- rbind(summary_dataset2, YY)
factor(YYY)

YYY <- as.data.frame(lapply(YYY, addNoAnswer))

YYY <- as.data.frame(lapply(YYY, addNoAnswer))
YYY$loss <- as.numeric(as.character(YYY$loss))
YYY1<- YYY[order(YYY$loss),] 
YYY1 <- YYY1[ nrow(YYY1):1, ]

p <- plot_ly(YYY, labels = ~damagecause, values = ~loss, type = 'pie',
  textposition = 'inside',
textinfo = 'label+percent',
insidetextfont = list(color = '#FFFFFF'),
hoverinfo = 'text',

text = ~paste('$', loss, ' million'),
marker = list(colors = colors,
              line = list(color = '#FFFFFF', width = 1)),
#The 'pull' attribute can also be used to create space between the sectors
showlegend = TRUE) %>%
  layout(title = 'Top Damage Causes for Wheat, Palouse 26 County Region: 2015',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

grid.table(YYY1)


#pie chart for 2009


palouse_sumloss_2009 <- palouse_sumloss_2009[order(palouse_sumloss_2009$x),] 
X <- palouse_sumloss_2009[order(palouse_sumloss_2009$x),] 
colnames(X) <- c("damagecause", "loss")
summary_dataset <- X %>% filter(loss < 8000000) 
summary_dataset2 <- X %>% filter(loss > 8000000) 
colnames(summary_dataset) <- c("damagecause", "loss")
Y <- sum(summary_dataset$loss)
YY <- c("Other", Y)
YY <- data.frame(t(YY))
colnames(YY) <- c("damagecause", "loss")
YYY <- rbind(summary_dataset2, YY)
factor(YYY)

YYY <- as.data.frame(lapply(YYY, addNoAnswer))

YYY <- as.data.frame(lapply(YYY, addNoAnswer))
YYY$loss <- as.numeric(as.character(YYY$loss))
YYY1<- YYY[order(YYY$loss),] 
YYY1 <- YYY1[ nrow(YYY1):1, ]


p <- plot_ly(YYY, labels = ~damagecause, values = ~loss, type = 'pie',
             textposition = 'inside',
             textinfo = 'label+percent',
             insidetextfont = list(color = '#FFFFFF'),
             hoverinfo = 'text',
             text = ~paste('$', loss, ' billions'),
             marker = list(colors = colors,
                           line = list(color = '#FFFFFF', width = 1)),
             #The 'pull' attribute can also be used to create space between the sectors
             showlegend = TRUE) %>%
  layout(title = 'Top Damage Causes for Wheat, Palouse 26 County Region: 2009',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

grid.table(YYY1)


#pie chart for 2011


palouse_sumloss_2011 <- palouse_sumloss_2011[order(palouse_sumloss_2011$x),] 
X <- palouse_sumloss_2011[order(palouse_sumloss_2011$x),] 
colnames(X) <- c("damagecause", "loss")
summary_dataset <- X %>% filter(loss < 1000000) 
summary_dataset2 <- X %>% filter(loss > 1000000) 
colnames(summary_dataset) <- c("damagecause", "loss")
Y <- sum(summary_dataset$loss)
YY <- c("Other", Y)
YY <- data.frame(t(YY))
colnames(YY) <- c("damagecause", "loss")
YYY <- rbind(summary_dataset2, YY)
factor(YYY)

YYY <- as.data.frame(lapply(YYY, addNoAnswer))

YYY <- as.data.frame(lapply(YYY, addNoAnswer))
YYY$loss <- as.numeric(as.character(YYY$loss))
YYY1<- YYY[order(YYY$loss),] 
YYY1 <- YYY1[ nrow(YYY1):1, ]


p <- plot_ly(YYY, labels = ~damagecause, values = ~loss, type = 'pie',
             textposition = 'inside',
             textinfo = 'label+percent',
             insidetextfont = list(color = '#FFFFFF'),
             hoverinfo = 'text',
             text = ~paste('$', loss, ' billions'),
             marker = list(colors = colors,
                           line = list(color = '#FFFFFF', width = 1)),
             #The 'pull' attribute can also be used to create space between the sectors
             showlegend = TRUE) %>%
  layout(title = 'Top Damage Causes for Wheat, Palouse 26 County Region: 2011',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

grid.table(YYY1)

#pie chart for 2001-2015
SI_drought_counties <- aggregate(SI_drought$loss, by = list(SI_drought$county), FUN = "sum")
var44 <- na.omit(var4)
var44 <- aggregate(var44$loss, by = list(var44$county), FUN = "sum")


SI_drought_counties_ordered <- SI_drought_counties[order(SI_drought_counties$x),] 
#X <- aggregate(SI_drought$loss, by = list(SI_drought$county), FUN = "sum")
colnames(SI_drought_counties_ordered) <- c("county", "loss")
X <- SI_drought_counties_ordered
summary_dataset <- X %>% filter(loss < 800000) 
summary_dataset2 <- X %>% filter(loss > 800000) 
colnames(summary_dataset) <- c("county", "loss")
Y <- sum(summary_dataset$loss)
YY <- c("Other", Y)
YY <- data.frame(t(YY))
colnames(YY) <- c("county", "loss")
YYY <- rbind(summary_dataset2, YY)
#factor(YYY)

YYY <- as.data.frame(lapply(YYY, addNoAnswer))
YYY$loss <- as.numeric(as.character(YYY$loss))
YYY1<- YYY[order(YYY$loss),] 
YYY1 <- YYY1[ nrow(YYY1):1, ]

p <- plot_ly(YYY1, labels = ~county, values = ~loss, type = 'pie',
             textposition = 'inside',
             textinfo = 'label+percent',
             insidetextfont = list(color = '#FFFFFF'),
             hoverinfo = 'text',
             text = ~paste('$', loss),
             marker = list(colors = colors,
                           line = list(color = '#FFFFFF', width = 1)),
             #The 'pull' attribute can also be used to create space between the sectors
             showlegend = TRUE) %>%
  layout(title = 'Top Wheat/Drought Commodity Loss Counties, 2001-2015 \n Southern Idaho',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

grid.table(YYY1)


#pie chart for 2001-2015
data3a <- na.omit(data3)
SI_drought_counties <- aggregate(data3a$loss, by = list(data3a$county), FUN = "sum")
var44 <- na.omit(var4)
var44 <- aggregate(var44$loss, by = list(var44$county), FUN = "sum")


SI_drought_counties_ordered <- SI_drought_counties[order(SI_drought_counties$x),] 
#X <- aggregate(SI_drought$loss, by = list(SI_drought$county), FUN = "sum")
colnames(SI_drought_counties_ordered) <- c("county", "loss")
X <- SI_drought_counties_ordered
summary_dataset <- X %>% filter(loss < 500000) 
summary_dataset2 <- X %>% filter(loss > 500000) 
colnames(summary_dataset) <- c("county", "loss")
Y <- sum(summary_dataset$loss)
YY <- c("Other", Y)
YY <- data.frame(t(YY))
colnames(YY) <- c("county", "loss")
YYY <- rbind(summary_dataset2, YY)
#factor(YYY)

YYY <- as.data.frame(lapply(YYY, addNoAnswer))
YYY$loss <- as.numeric(as.character(YYY$loss))
YYY1<- YYY[order(YYY$loss),] 
YYY1 <- YYY1[ nrow(YYY1):1, ]

p <- plot_ly(YYY1, labels = ~county, values = ~loss, type = 'pie',
             textposition = 'inside',
             textinfo = 'label+percent',
             insidetextfont = list(color = '#FFFFFF'),
             hoverinfo = 'text',
             text = ~paste('$', loss),
             marker = list(colors = colors,
                           line = list(color = '#FFFFFF', width = 1)),
             #The 'pull' attribute can also be used to create space between the sectors
             showlegend = TRUE) %>%
  layout(title = 'Top Wheat/Drought Commodity Loss Counties, 2001-2015, Southern Idaho',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

grid.table(YYY1)


#interaction plot for selected counties - wheat drought claims 1998-215

SI_drought$county <- factor(SI_drought$damagecause)
SI_drought$year <- factor(SI_drought$year)
par(mar=c(5,6,4,2)+0.1,mgp=c(5,1,0))
interaction.plot(x.factor     = SI_drought$year,
                 trace.factor = SI_drought$county, 
                 response     = SI_drought$loss, 
                 fun = sum,
                 las = 2,
                 type="b",
                 col=c("black","red","green", "blue", "yellow"),  ### Colors for levels of trace var.
                 #pch=c(19, 17, 15),             ### Symbols for levels of trace var.
                 fixed=FALSE,                    ### Order by factor order in data
                 leg.bty = "n",
                 ylab="Loss ($)",
                 xlab="years",
                 main="Interaction Plot - Cube Root Loss ($) vs. year for Select Counties, 2001-2015", las = 2)


#-list of individual commodity with damage cause totals

top10_damage <- aggregate(Southern_ID_sumloss_all_sum$loss, by = list(Southern_ID_sumloss_all_sum$commodity, Southern_ID_sumloss_all_sum$year), FUN = "sum")
top10_damage2 <- aggregate(top10_damage$x, by = list(top10_damage$Group.1), FUN = "sum")
top10_damage3 <- top10_damage2[order(-top10_damage2$x),] 
colnames(top10_damage3) <- c("Commodity", "Loss")
grid.table(top10_damage3)


#-list of individual commodity with damage cause totals
t10_comm <- subset(Southern_ID_sumloss_all_sum, commodity == "POTATOES")
top10_damage <- aggregate(t10_comm$loss, by = list(t10_comm$county, t10_comm$year, t10_comm$damagecause), FUN = "sum")
top10_damage2 <- aggregate(top10_damage$x, by = list(top10_damage$Group.3), FUN = "sum")
top10_damage3 <- top10_damage2[order(-top10_damage2$x),] 

#wheat
Southern_ID_sumloss_all_sum_subset <- subset(Southern_ID_sumloss_all_sum, commodity == "POTATOES")

Southern_ID_sumloss_all_sum_subset <- subset(Southern_ID_sumloss_all_sum_subset, damagecause == "Cold Wet Weather" | damagecause == "Hail" | damagecause == "Freeze" | damagecause == "Frost" | damagecause == "Failure Irrig Supply" | damagecause == "Excess Moisture/Precip/Rain" | damagecause == "Heat")
#potatoes
Southern_ID_sumloss_all_sum_subset <- subset(Southern_ID_sumloss_all_sum, commodity == "WHEAT")

Southern_ID_sumloss_all_sum_subset <- subset(Southern_ID_sumloss_all_sum_subset, damagecause == "Cold Wet Weather" | damagecause == "Hail" | damagecause == "Decline in Price" | damagecause == "Frost" | damagecause == "Drought" | damagecause == "Excess Moisture/Precip/Rain" | damagecause == "Heat")
#barley
Southern_ID_sumloss_all_sum_subset <- subset(Southern_ID_sumloss_all_sum, commodity == "BARLEY")
Southern_ID_sumloss_all_sum_subset <- subset(Southern_ID_sumloss_all_sum_subset, damagecause == "Faiure Irrig Supply" | damagecause == "Hail" | damagecause == "Decline in Price" | damagecause == "Cold Winter" | damagecause == "Drought" | damagecause == "Excess Moisture/Precip/Rain" | damagecause == "Heat")


#--loss vs. year for all counties, for select damage causes 1998-2015
Southern_ID_sumloss_all_sum_subset$damagecause <- factor(Southern_ID_sumloss_all_sum_subset$damagecause)
par(mar=c(5,6,4,2)+0.1,mgp=c(5,1,0))
interaction.plot(x.factor     = Southern_ID_sumloss_all_sum_subset$year,
                 trace.factor = Southern_ID_sumloss_all_sum_subset$damagecause, 
                 response     = Southern_ID_sumloss_all_sum_subset$loss, 
                 fun = sum,
                 las = 2,
                 type="b",
                 col=c("black","red","green"),  ### Colors for levels of trace var.
                 pch=c(19, 17, 15, 13, 11, 9, 7),             ### Symbols for levels of trace var.
                 fixed=FALSE,                    ### Order by factor order in data
                 leg.bty = "n",
                 ylab="Loss ($)",
                 xlab="years",
                 main="Interaction Plot - Loss ($) vs. year for Select Damage Causes for Idaho - WHEAT", las = 2)

library(plotrix)
whitman_drought <- subset(palouse_sumloss_onlydrought, county == "Whitman")
adams_drought <- subset(palouse_sumloss_onlydrought, county == "Adams")

twoord.plot(c(1998:2015), adams_drought$loss, c(1998:2015), rylim=c(100, 350), lylim=c(18877,25000000), adams_drought$price, ylab = "Wheat Commodity Loss ($)",  rylab = "Wheat Price ($)", xlab = "Years", type=c("bar", "b"), lcol = "green", rcol = "blue", main = "Wheat Commodity Losses due to Drought vs. Wheat Prices, 1998-2015 \n Adams County, WA")
twoord.plot(c(1998:2015), whitman_drought$loss, c(1998:2015), rylim=c(100, 350), lylim=c(18877,23000000), whitman_drought$price, ylab = "Wheat Commodity Loss ($)",  rylab = "Wheat Price ($)", xlab = "Years", type=c("bar", "b"), lcol = "green", rcol = "blue", main = "Wheat Commodity Losses due to Drought vs. Wheat Prices, 1998-2015 \n Whitman County, WA")




#-interaction plot with WHEAT Counts vs year as x and county as line
data_county <- subset(palouse_counts, county == "Whitman" | county == "Spokane" | county == "Adams" | county == "Lincoln" | county == "Grant" | county == "Grant" | county == "Douglas" | county == "Asotin" | county == "Walla Walla" )
data_county <- subset(palouse_counts, county == "Whitman" | county == "Spokane" | county == "Adams" | county == "Lincoln" )

data_county$county <- factor(data_county$county)
interaction.plot(x.factor     = data_county$year,
                 trace.factor = data_county$county, 
                 response     = data_county$count, 
                 fun = mean,
                 las = 2,
                 type="b",
                 col=c("black","red","green"),  ### Colors for levels of trace var.
                 pch=c(19, 17, 15, 13, 11, 9, 7),             ### Symbols for levels of trace var.
                 fixed=TRUE,                    ### Order by factor order in data
                 leg.bty = "n",
                 ylab="PR (mm)",
                 xlab="years",
                 main="Interaction Plot - loss vs. year for all damage causes, select Palouse Counties", las = 2)
                
#--interaction plot loss vs year for all damage causes, all Palouse counties

interaction.plot(x.factor     = palouse_counts$year,
                 trace.factor = palouse_counts$county, 
                 response     = palouse_counts$count, 
                 fun = sum,
                 las = 2,
                 type="b",
                 col=c("black","red","green"),  ### Colors for levels of trace var.
                 pch=c(19, 17, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1),             ### Symbols for levels of trace var.
                 fixed=TRUE,                    ### Order by factor order in data
                 leg.bty = "n",
                 ylab="Commodity Claim Counts", 
                 main="Interaction Plot - Loss Claim Counts vs. year for all damage causes,  Palouse Region (WA, ID, OR)", las = 2)
                 
#--interaction plot - loss vs year for wheat drought claims, all counties in Palouse

interaction.plot(x.factor     = pc_drought$year,
                 trace.factor = pc_drought$county, 
                 response     = pc_drought$count, 
                 fun = sum,
                 las = 2,
                 type="b",
                 col=c("black","red","green"),  ### Colors for levels of trace var.
                 pch=c(19, 17, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1),             ### Symbols for levels of trace var.
                 fixed=TRUE,                    ### Order by factor order in data
                 leg.bty = "n",
                 ylab="Commodity Claim Counts", 
                 main="Interaction Plot - Drought Loss Claim Counts vs. year for the Palouse Region (WA, ID, OR)", las = 2)


#---Bartlett Test - homogeneity of variances

bartlett.test(palouse_sumloss_allcomm2$loss, palouse_sumloss_allcomm2$county)

bartlett.test(palouse_sumloss$loss, palouse_sumloss$county)

bartlett.test(palouse_sumloss$loss, palouse_sumloss$year)

qchisq(0.95, 25)

#--All Bartlett tests show that the variances are not homogeneous

#--Fligner-Killeen test for homoskedasticity

fligner.test(palouse_sumloss_allcomm2$loss, palouse_sumloss_allcomm2$year)





#------

#AOV

#-is there any significant difference in loss betwen different damage causes
#-are the variations between the damage cause means due to true differences about 
#-the populations means or just due to sampling variability?
#-Ftest compares variation of sample means among damage causes to the variation between damage causes (within).


#--performing an single interaction aov for loss transformed by a cube root function
#--by damagecuase.  This is for ALL commodities


fit <- aov(loss~damagecause, data=palouse_sumloss_allcomm2)
bc <- boxcox(loss~damagecause, data=palouse_sumloss_allcomm2, lambda = seq(-0.05, 0.1, len = 20))

boxcox.lambda(bc, method = c("guerrero", "loglik"), lower = -1, upper = 2)



summary(fit) #Type I ANOVA table
drop1(fit,~.,test="F") # type III SS and Ftests

#plot(fit)

library(broom)
library(magrittr)

#--ad hoc test

tuk <- TukeyHSD(fit)

psig=as.numeric(apply(tuk$`damagecause`[,2:3],1,prod)>=0)+1
plot(tuk,col=psig,yaxt="n")

tuk2 <- as.data.frame(broom::tidy(tuk))
subset(tuk2, adj.p.value < .05)

psig=as.numeric(apply(tuk2$`damagecause`[,2:3],1,prod)>=0)+1



#-listing combos where the mean differences in damage causes are significant
#Damage causes coming to forefront include: Drought and Decline in Price 

tuk3 <- subset(tuk2, adj.p.value < .05)

tuk3


#---------



#--performing an single interaction aov for loss transformed by a cube root function
#--by damagecuase.  This is for ONLY WHEAT loss

fit <- aov(cube_loss~damagecause, data=palouse_sumloss)
summary(fit) #Type I ANOVA table
drop1(fit,~.,test="F") # type III SS and Ftests

#plot(fit)

library(broom)
library(magrittr)
#--ad hoc test

tukk <- TukeyHSD(fit)

psig=as.numeric(apply(tukk$`damagecause`[,2:3],1,prod)>=0)+1
plot(tukk,col=psig,yaxt="n")

tukk2 <- as.data.frame(tidy(tukk))
subset(tukk2, adj.p.value < .05)

#-listing combos where the mean differences in damage causes are significant
#Damage causes coming to forefront include: Drought and Decline in Price 
tukk3 <- subset(tukk2, adj.p.value < .05)

tukk3

#---------


#--performing an single interaction aov for loss transformed by a cube root function
#--by damagecuase.  This is for ONLY WHEAT counts

fit <- aov(count~damagecause, data=palouse_counts)
summary(fit) #Type I ANOVA table
drop1(fit,~.,test="F") # type III SS and Ftests

#plot(fit)

library(broom)
library(magrittr)
#--ad hoc test

tukk <- TukeyHSD(fit)

psig=as.numeric(apply(tukk$`damagecause`[,2:3],1,prod)>=0)+1
plot(tukk,col=psig,yaxt="n")

tukk2 <- as.data.frame(tidy(tukk))
subset(tukk2, adj.p.value < .05)

#-listing combos where the mean differences in damage causes are significant
#Damage causes coming to forefront include: Drought and Decline in Price 
tukk3 <- subset(tukk2, adj.p.value < .05)

tukk3


#----

#--performing an single interaction aov for loss transformed by a cube root function
#--by commodity  This is for ALL commodities


fit <- aov(cube_loss~commodity, data=palouse_sumloss_allcomm2)
summary(fit) #Type I ANOVA table
drop1(fit,~.,test="F") # type III SS and Ftests

#plot(fit)

library(broom)
library(magrittr)
#--ad hoc test

tukk <- TukeyHSD(fit)

psig=as.numeric(apply(tukk$`commodity`[,2:3],1,prod)>=0)+1
plot(tukk,col=psig,yaxt="n")

tukk2 <- as.data.frame(tidy(tukk))
subset(tukk2, adj.p.value < .05)

#-listing combos where the mean differences in damage causes are significant
#Damage causes coming to forefront include: Drought and Decline in Price 
tukk3 <- subset(tukk2, adj.p.value < .05)

tukk3


#----

#--performing an single interaction aov for loss transformed by a cube root function
#--by year  This is for ALL commodities


fit <- aov(loss~county, data=palouse_sumloss_allcomm2)
summary(fit) #Type I ANOVA table
drop1(fit,~.,test="F") # type III SS and Ftests

#plot(fit)

library(broom)
library(magrittr)
#--ad hoc test

tukk <- TukeyHSD(fit)

psig=as.numeric(apply(tukk$`county`[,2:3],1,prod)>=0)+1
plot(tukk,col=psig,yaxt="n")

tukk2 <- as.data.frame(tidy(tukk))
subset(tukk2, adj.p.value < .05)

#-listing combos where the mean differences in damage causes are significant
#Damage causes coming to forefront include: Drought and Decline in Price 
tukk3 <- subset(tukk2, adj.p.value < .05)

tukk3



#---------

fit_c = lm(formula = palouse_sumloss_allcomm2$loss ~ palouse_sumloss_allcomm2$county)
anova(fit_c)
fit_d = lm(formula = palouse_sumloss_allcomm2$loss ~ palouse_sumloss_allcomm2$damagecause)
anova(fit_d)
fit_co = lm(formula = palouse_sumloss_allcomm2$loss ~ palouse_sumloss_allcomm2$commodity)
anova(fit_co)
fit_y = lm(formula = palouse_sumloss_allcomm2$loss ~ factor(palouse_sumloss_allcomm2$year))
anova(fit_y)

#---If F for test is above tabulated F - reject hypothesis.  Group means are not statistically equal

qf(0.950, 6692, 30)


all1 <- anova(lm(loss ~ county + damagecause + year, data = palouse_sumloss))

all_lm1 <- lm(loss ~ county + damagecause + year, data = palouse_sumloss)

palouse_sumloss_allcomm2$year <- factor(palouse_sumloss_allcomm2$year)

all2 <- anova(lm(loss ~ commodity + county + damagecause + year + commodity:damagecause, data = palouse_sumloss_allcomm2))

all_lm2 <- lm(loss ~ commodity + county + damagecause + year + commodity:damagecause, data = palouse_sumloss_allcomm2)

#--ad hoc tukey for multiple 
#-Post-hoc testing with lsmeans
#-Because the main effects were significant, we will want to perform post-hoc mean separation tests 
#-for each main effect factor variable.

library(lsmeans)

lsmeans(all_lm1,
        pairwise ~ county, 
        adjust="tukey")  


#---
#--all commodities in Palouse
plot(cube_loss ~ county + commodity + damagecause + year, data=palouse_sumloss_allcomm2, las=2) #all commodities

plot(cube_loss ~ county + damagecause + year, data=palouse_sumloss, las=2) #Wheat

#--accessing output of design matrix/time lag data based on monthly selection from dashboard runs

var1 <- read.csv("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix_outputs_ID/pr_sep5_cube_root_loss_climatecorrelation.csv")
colnames(var1)[9] <- paste(colnames(var1)[2], "_zscore", sep="")


var2 <- read.csv("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix_outputs_ID/pet_sep5_cube_root_loss_climatecorrelation.csv")
colnames(var2)[9] <- paste(colnames(var2)[2], "_zscore", sep="")
var3 <- read.csv("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix_outputs_ID/tmmx_sep4_cube_root_loss_climatecorrelation.csv")
colnames(var3)[9] <- paste(colnames(var3)[2], "_zscore", sep="")

var4 <- read.csv("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix_outputs_ID/pr_sep5_loss_climatecorrelation.csv")
colnames(var4)[9] <- paste(colnames(var4)[2], "_zscore", sep="")


var5 <- read.csv("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix_outputs_ID/pet_sep5_loss_climatecorrelation.csv")
colnames(var5)[9] <- paste(colnames(var5)[2], "_zscore", sep="")
var6 <- read.csv("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix_outputs_ID/tmmx_sep3_loss_climatecorrelation.csv")
colnames(var6)[9] <- paste(colnames(var6)[2], "_zscore", sep="")


var7 <- read.csv("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix_outputs/pr_sep5_loss_climatedata.csv")
colnames(var4)[9] <- paste(colnames(var7)[2], "_zscore", sep="")


var8 <- read.csv("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix_outputs/pet_sep5_loss_climatedata.csv")
colnames(var5)[9] <- paste(colnames(var8)[2], "_zscore", sep="")
var9 <- read.csv("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix_outputs/tmmx_jul2_loss_climatedata.csv")
colnames(var6)[9] <- paste(colnames(var9)[2], "_zscore", sep="")

var7a <- read.csv("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix_outputs/pr_sep5_loss_climatecorrelation.csv")
colnames(var4)[9] <- paste(colnames(var7)[2], "_zscore", sep="")


var8a <- read.csv("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix_outputs/pet_sep5_loss_climatecorrelation.csv")
colnames(var5)[9] <- paste(colnames(var8)[2], "_zscore", sep="")
var9a <- read.csv("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix_outputs/tmmx_jul2_loss_climatecorrelation.csv")
colnames(var6)[9] <- paste(colnames(var9)[2], "_zscore", sep="")

irrigated <- read.csv("/dmine/code/git/clim401/irrigated.csv", header = TRUE)




data1 <- cbind(var1, var2[2], var2[9], var3[2], var3[9])
data1_model <- na.omit(cbind(data1[9], data1[10], data1[12], data1[14], data1[16]))


setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix_outputs_ID")
write.csv(data1_model, file = "data1.csv")

data2 <- cbind(var1[1:6], var2[2], var3[2])

data3 <- cbind(var4[1:6], var5[2], var4[9], var5[9], var6[2], var6[9])
data3a <- cbind(var4[1:6], var5[2], var6[2])


data1 <- merge(data1, irrigated, by = "county")
data1$notirrigated <- 1-data1$irrigated

data3 <- merge(data3, irrigated, by = "county")
data3$notirrigated <- 1-data3$irrigated


data3 <- plyr::join(data3, wheatprice_year, by = "year")

data4 <- cbind(var7[1:6], var8[2], var9[2])

data4a <- left_join(data4, var7a, by = c("year" = "year", "county" = "county"))
data4aa <- na.omit(data4a)

colnames(data4aa) <- c("X", "pr", "year", "pr_zscore", "damagecause", "county", "pet", "tmmx", "X.y", "pr2", "loss", "state", "commodity", "matrixnumber", "clim_zscore", "loss_zscore")
write.csv(data4aa, file = "/dmine/data/USDA/agmesh-scenarios/Allstates/summaries/lag_palouse1.csv")
colnames(data3) <- c("X", "pr", "acres", "year", "state", "county", "pet", "tmmx")

cor2 <- cor(data1) 
pairs(loss_zscore ~ pr_zscore + tmmx_zscore + pet_zscore, data = data1,
      lower.panel=panel.smooth, upper.panel=panel.cor, main = "initial pairs plot")

#--acres
pairs(acres ~ pr + tmmx + pet, data = data3,
      lower.panel=panel.smooth, upper.panel=panel.cor, main = "initial pairs plot")

#--loss
pairs(loss ~ pr + tmmx + pet, data = data3a,
      lower.panel=panel.smooth, upper.panel=panel.cor, main = "initial pairs plot")
#--end

#--boxplot

SI_drought$county <- factor(SI_drought$county)
data1$tmmx_celsius <- data1$tmmx-278
boxplot(loss ~ county, data = na.omit(SI_drought), ylab = "Loss ($)", col="lightblue", las = 3, outline=FALSE, main = "Wheat/Drought Loss by County: Southern ID")
boxplot(loss ~ year, data = na.omit(SI_drought), ylab = "Loss ($)", xlab = "Years", col="lightblue", las = 3, outline=FALSE, main = "Wheat/Drought Loss by Year: Southern ID")
boxplot(pr ~ county, data = data3, col="lightblue", las = 3, ylab = "Total Precipitation (Inches)", main = "Precipitation by Year for Selected Southern ID Counties")
boxplot(tmmx_celsius ~ county, data = data1, col="green", las = 3, ylab = "Temperature (Celsius)", main = "Max Temperature by Year for Selected Southern ID Counties")
boxplot(pet ~ county, data = data3, col="lightblue", las = 3, ylab = "PET", main = "PET by County for Selected Southern ID Counties")

#climate aggregation for climographs by year for individual counties

tmmxmean <- aggregate(data3$tmmx, by = list(data3$county, data3$year), FUN = 'mean')
colnames(tmmxmean) <- c("county", "year", "tmmx")
power_tmmx <- subset(tmmxmean, county == "Power")
power_tmmx$tmmx_celsius <- power_tmmx$tmmx-278

petmean <- aggregate(data3$pet, by = list(data3$county, data3$year), FUN = 'mean')
colnames(petmean) <- c("county", "year", "pet")
power_pet <- subset(petmean, county == "Power")

prmean <- aggregate(data3$pr, by = list(data3$county, data3$year), FUN = 'mean')
colnames(prmean) <- c("county", "year", "pr")
power_pr <- subset(prmean, county == "Power")

lossmean <- aggregate(SI_drought$loss, by = list(SI_drought$county, SI_drought$year), FUN = 'sum')
colnames(lossmean) <- c("county", "year", "loss")
power_loss <- subset(lossmean, county == "Power")



barplot(power_pet$pet, names.arg = power_pet$year, las = 3)
twoord.plot(c(2002:2015), power_tmmx$tmmx_celsius, c(2002:2015), rylim=c(2,9), lylim=c(20, 25), round(power_pr$pr, 1), ylab = "Max Temp (C)",  rylab = "Precipitation (total - inches)", xlab = "Years", type=c("bar", "b"), lcol = "green", rcol = "blue", main = "Power County, ID: Max Temp vs Total Precipitation \n 2002-2015", las = 2)

twoord.plot(c(2002:2015), power_tmmx$tmmx_celsius, c(2002:2015), rylim=c(4.7, 5.5), lylim=c(20, 25), round(power_pet$pet, 1), ylab = "Max Temp (C)",  rylab = "PET (Inches)", xlab = "Years", type=c("bar", "b"), lcol = "green", rcol = "blue", main = "Power County, ID: Max Temp vs PET \n 2002-2015", las = 2)

twoord.plot(c(2002:2015), power_tmmx$tmmx_celsius, c(2002:2015), rylim=c(0, 2600000), lylim=c(20, 25), round(power_loss$loss, 1), ylab = "Max Temp (C)",  rylab = "Loss ($)", xlab = "Years", type=c("bar", "b"), lcol = "green", rcol = "blue", main = "Power County, ID: Wheat/drought loss vs TMMX \n 2002-2015", las = 2)

twoord.plot(c(2002:2015), power_pr$pr, c(2002:2015), rylim=c(0, 2600000), lylim=c(3,9), round(power_loss$loss, 1), ylab = "Total Precipitation (inches)",  rylab = "Loss ($)", xlab = "Years", type=c("bar", "b"), lcol = "green", rcol = "blue", main = "Power County, ID: Wheat/drought loss vs PR \n 2002-2015", las = 2)

twoord.plot(c(2002:2015), power_pet$pet, c(2002:2015), rylim=c(0, 2600000), lylim=c(4.8, 5.5), round(power_loss$loss, 1), ylab = "PET (inches)",  rylab = "Loss ($)", xlab = "Years", type=c("bar", "b"), lcol = "green", rcol = "blue", main = "Power County, ID: Wheat/drought loss vs PET \n 2002-2015", las = 2)


tmmxmean <- aggregate(data3$tmmx, by = list(data3$county, data3$year), FUN = 'mean')
colnames(tmmxmean) <- c("county", "year", "tmmx")
power_tmmx <- subset(tmmxmean, county == "Franklin")
power_tmmx$tmmx_celsius <- power_tmmx$tmmx-278

petmean <- aggregate(data3$pet, by = list(data3$county, data3$year), FUN = 'mean')
colnames(petmean) <- c("county", "year", "pet")
power_pet <- subset(petmean, county == "Franklin")

prmean <- aggregate(data3$pr, by = list(data3$county, data3$year), FUN = 'mean')
colnames(prmean) <- c("county", "year", "pr")
power_pr <- subset(prmean, county == "Franklin")

lossmean <- aggregate(SI_drought$loss, by = list(SI_drought$county, SI_drought$year), FUN = 'sum')
colnames(lossmean) <- c("county", "year", "loss")
power_loss <- subset(lossmean, county == "Franklin")
power_loss <- power_loss[-1,]

twoord.plot(c(2002:2015), power_tmmx$tmmx_celsius, c(2002:2015), rylim=c(4.5,13), lylim=c(18, 25), round(power_pr$pr, 1), ylab = "Max Temp (C)",  rylab = "Precipitation (total - inches)", xlab = "Years", type=c("bar", "b"), lcol = "green", rcol = "blue", main = "Franklin County, ID: Max Temp vs Total Precipitation \n 2002-2015", las = 2)

twoord.plot(c(2002:2015), power_tmmx$tmmx_celsius, c(2002:2015), rylim=c(4.6, 5.5), lylim=c(18, 25), round(power_pet$pet, 1), ylab = "Max Temp (C)",  rylab = "PET (Inches)", xlab = "Years", type=c("bar", "b"), lcol = "green", rcol = "blue", main = "Franklin County, ID: Max Temp vs PET \n 2002-2015", las = 2)

twoord.plot(c(2002:2015), power_tmmx$tmmx_celsius, c(2002:2015), rylim=c(0, 5000000), lylim=c(18, 25), round(power_loss$loss, 1), ylab = "Max Temp (C)",  rylab = "Loss ($)", xlab = "Years", type=c("bar", "b"), lcol = "green", rcol = "blue", main = "Franklin County, ID: Wheat/drought loss vs TMMX \n 2002-2015", las = 2)

twoord.plot(c(2002:2015), power_pr$pr, c(2002:2015), rylim=c(0, 5000000), lylim=c(4.5, 13), round(power_loss$loss, 1), ylab = "Total Precipitation (inches)",  rylab = "Loss ($)", xlab = "Years", type=c("bar", "b"), lcol = "green", rcol = "blue", main = "Franklin County, ID: Wheat/drought loss vs PR \n 2002-2015", las = 2)

twoord.plot(c(2002:2015), power_pet$pet, c(2002:2015), rylim=c(0, 5000000), lylim=c(4.5, 5.5), round(power_loss$loss, 1), ylab = "PET (inches)",  rylab = "Loss ($)", xlab = "Years", type=c("bar", "b"), lcol = "green", rcol = "blue", main = "Franklin County, ID: Wheat/drought loss vs PET \n 2002-2015", las = 2)






#-multiple regression

fit <- lm(loss_zscore ~ pr_zscore + tmmx_zscore + pet_zscore, data = data1)
fit1 <- lm(loss ~ pr + tmmx + pet, data = data3)
fit2 <- lm(loss ~ pet, data = data3)
fit3 <- lm(loss_zscore ~ notirrigated, data = data1)
anova(fit2, fit1)

library(relaimpo)
calc.relimp(fit,type=c("lmg","last","first","pratt"),
            rela=TRUE)

# Bootstrap Measures of Relative Importance (1000 samples) 
boot <- boot.relimp(fit, b = 1000, type = c("lmg", 
                                            "last", "first", "pratt"), rank = TRUE, 
                    diff = TRUE, rela = TRUE)
booteval.relimp(boot) # print result
plot(booteval.relimp(boot,sort=TRUE)) # plot result







#--aov test
fit <- aov(loss ~ pr_zscore + tmmx_zscore + pet_zscore, data = data1)

fit <- aov(loss ~ pr + tmmx + pet, data = data3)

layout(matrix(c(1,2,3,4),2,2)) # optional layout 
plot(fit) # diagnostic plots

summary(fit) # display Type I ANOVA table
fit2 <- drop1(fit,~.,test="F") # type III SS and F Tests


par(mfrow=c(1,1))
#--Two-way Interaction Plot 

data3a <- subset(data2, year >= 2001)
data4b <- subset(data4a, year >= 2001)
#attach(mtcars)
county <- factor(data4b$county)
year <- factor(data4b$year)
interaction.plot(year, county, data4b$pr, type="b", col=c(1:3), 
                 leg.bty="o", leg.bg="beige", lwd=2, pch=c(1:25),	
                 fun = mean,
                 xlab=" ", 
                 ylab="mean PR (mm)", 
                 main="Interaction Plot - PRECIP vs. year for palouse region counties, by county", las = 2)

data3a <- subset(data2, year >= 2001)

#attach(mtcars)
county <- factor(data3a$county)
year <- factor(data3a$year)
colnames(data3a) <- c("X", "pr", "loss", "year", "state", "county", "pet", "tmmx")

interaction.plot(year, county, data3a$tmmx, type="b", col=c(1:3), 
                 leg.bty="o", leg.bg="beige", lwd=2, pch=c(1:25),	
                 xlab=" ", 
                 ylab="TMMX (K)", 
                 main="Interaction Plot - TMMX vs. year for PNW states", las = 2)


#attach(mtcars)
county <- factor(data3$state)
year <- factor(data3$year)
interaction.plot(year, county, data3$acres, type="b", col=c(1:3), 
                 leg.bty="o", leg.bg="beige", lwd=2, pch=c(1:25),	
                 xlab=" ", 
                 ylab="acres", 
                 main="Interaction Plot - Wheat/Drought loss (acres) vs. year for PNW states", las = 2)


data2$loss.c <- scale(data2$loss, center = TRUE, scale = FALSE)[,]
data2$pr.c <- scale(data2$pr, center = TRUE, scale = FALSE)[,]

data2$tmmx.c <- scale(data2$tmmx, center = TRUE, scale = FALSE)[,]

data2$pet.c <- scale(data2$pet, center = TRUE, scale = FALSE)[,]

data22 <- na.omit(data2)






data11 <- na.omit(data1)
library(caret)
set.seed(993)

inTrain <- createDataPartition(y=c(data11$pr_zscore, data11$pet_zscore, data11$tmmx_zscore), p=0.5, list=FALSE)
traindata1 <- data11[inTrain,]
testdata1 <- data11[-inTrain,]
fit <- lm(loss_zscore ~ pet_zscore * pr_zscore * tmmx_zscore, data = data11)
fit <- lm(loss_zscore ~ pet_zscore, data = data11)

plot(traindata1$pet_zscore, traindata1$loss_zscore, pch=19, col="blue")

lines(na.omit(traindata1$pet_zscore), fit$fitted.values,  lwd=3)

library(ggplot2)

ggplot(data = traindata1)+
  geom_line(aes(x = pr_zscore, y = fit$fitted.values))+
  geom_point(data = cars, aes(x=speed, y = dist))




train_control<- trainControl(method="cv", number=10, savePredictions = TRUE)
formfit <- as.formula(loss_zscore ~ pr_zscore + pet_zscore + tmmx_zscore + irrigated)

model<- train(formfit, data=data1, trControl=train_control, method="lm")

pred <- predict(model, data1)
pred <- data.frame(pred = pred, speed = cars$speed)

fit <- lm(loss_zscore ~ pr_zscore * irrigated + pet_zscore * irrigated + tmmx_zscore * irrigated, data = data1)
library(jtools)
interact_plot(fit, pred = "pr_zscore", modx = "irrigated", plot.points = TRUE)


fit <- lm(loss ~ pr * irrigated + pet * irrigated + tmmx * irrigated, data = data3)
library(jtools)
interact_plot(fit, pred = "pr_zscore", modx = "irrigated", plot.points = TRUE)









fit <- lm(loss ~ pr * pet, data = data2)
library(jtools)
interact_plot(fit, pred = "tmmx_zscore", modx = "pr_zscore", plot.points = TRUE)

fit <- lm(loss ~ tmmx * pr, data = data2)
library(jtools)
interact_plot(fit, pred = "tmmx", modx = "pr", plot.points = TRUE, centered = "none")


# Make big tree
form <- as.formula(loss_zscore ~ pr_zscore + tmmx_zscore + pet_zscore + notirrigated)
form1 <- as.formula(loss ~ pr + tmmx + pet)

form2 <- as.formula(loss ~ pr + tmmx + pet)
form3 <- as.formula(loss ~ pr + pet + tmmx + irrigated)
tree.1 <- rpart(form2,data=data2,control=rpart.control(minsplit=30,cp=0))
tree.2 <- rpart(form1,data=data3,control=rpart.control(minsplit=30,cp=0))
tree.3 <- rpart(form,data=data1,control=rpart.control(minsplit=30,cp=0))
tree.4 <- rpart(form3,data=data3,control=rpart.control(minsplit=30,cp=0))
# 
plot(tree.1)					# Will make a mess of the plot
text(tree.1, cex = .5)
# 
prp(tree.1)					# Will plot the tree
prp(tree.1,varlen=5)				# Shorten variable names

# Interatively prune the tree
new.tree.1 <- prp(tree.1,snip=TRUE)$obj # interactively trim the tree
prp(new.tree.1) # display the new tree
#
#-------------------------------------------------------------------

data2b <- cbind(data2$loss, data2$pr, data2$pet, data2$tmmx)
colnames(data2b) <- c("loss", "pr", "pet", "tmmx")

data4b <- cbind(data4a$loss, data4a$pr.x, data4a$pet, data4a$tmmx)
colnames(data4b) <- c("loss", "pr", "pet", "tmmx")
# load libraries
library(caret)
library(rpart)

# define training control
train_control<- trainControl(method="cv", number=10, savePredictions = TRUE)

data2b <- data.frame(data2b)
data2b <- na.omit(data2b)

data4b <- data.frame(data4b)
data4b <- na.omit(data4b)
# train the model 
model<- train(form2, data=data2b, trControl=train_control, method="rpart")

# make predictions
predictions<- predict(model,data2)

# append predictions
mydat<- cbind(data2,predictions)

# summarize results
confusionMatrix<- confusionMatrix(mydat$predictions,mydat$loss)





trainIndex  <- sample(1:nrow(data4), 0.8 * nrow(data4))
train <- data4[trainIndex,]
test <- data4[-trainIndex,]

ctrl = rpart.control(maxdepth=4)
tree.2 <- rpart(form2,data2, method = "anova", control=ctrl)	

tree.3 <- rpart(form2,data4a, method = "anova", control=ctrl)


rpart.plot(tree.4, digits = -3)



# A more reasonable tree
prp(tree.2)                                     # A fast plot													
fancyRpartPlot(tree.2)	

rpart.plot(tree.1, digits = -2)

#--loss
pairs(loss ~ pr + tmmx + pet, data = data2,
      lower.panel=panel.smooth, upper.panel=panel.cor, main = "initial pairs plot - cube root loss transformation")


library(caret)

tc <- trainControl("cv",10)
rpart.grid <- expand.grid(.cp=0.5)

data2b <- cbind(data2$loss, data2$pet, data2$pr, data2$tmmx)
colnames(data2b) <- c("loss", "pet", "pr", "tmmx")
data2b <- data.frame(data2b)

train.rpart <- train(loss ~ ., data=na.omit(data2b), method="rpart",trControl=tc,tuneGrid=rpart.grid)



# A fancy plot from rattle
#
#-------------------------------------------------------------------
#sends to json for use with d3
jsontree <- json_prsr(tree.1)
setwd(paste("/dmine/data/USDA/agmesh-scenarios/palouse/d3_tree_1/", sep=""))
jsontree2 <- gsub("'", '"', jsontree)
write(jsontree2, file="palousetree.JSON")



#------additional tree
# Regression Tree Example
library(rpart)

# grow tree 
fit <- rpart(loss ~ pr + tmmx + pet, 
             method="anova", data=data3)

printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

# create additional plots 
par(mfrow=c(1,2)) # two plots on one page 
rsq.rpart(fit) # visualize cross-validation results  	

# plot tree 
plot(fit, uniform=TRUE, 
     main="Regression Tree palouse spring wheat ")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

# create attractive postcript plot of tree 
post(fit, file = "c:/tree2.ps", 
     title = "Regression Tree palouse spring wheat ")

rpart.plot(fit, digits = -3)
#---random forest

library(randomForest)

rfit <- randomForest(loss_zscore ~ pr_zscore + tmmx_zscore + pet_zscore,  data=na.omit(data1))
print(rfit) # view results 
importance(rfit) # importance of each predictor

getTree(rfit, 1, labelVar=TRUE)

#--count data

count(train, 'damagecause')

regre <- mgcv::gam(pet_zscore ~ pr_zscore + tmmx_zscore, data=data1)
VIF1 <- (1/(1-.89))

#manova

manova(loss ~ pr + tmmx + pet, data = data3)
res.man <- manova(cbind(pet, pr, tmmx, year) ~ county*loss, data = data2)
summary(res.man)
