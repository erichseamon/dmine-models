library(rpart)				        # Popular decision tree algorithm
library(rattle)					# Fancy tree plot
library(rpart.plot)				# Enhanced tree plots
library(RColorBrewer)				# Color selection for fancy tree plot
library(party)					# Alternative decision tree algorithm
library(partykit)				# Convert rpart object to BinaryTree
library(caret)					# Just a data source for this script
library(mvnormtest)
library(plotly)
# but probably one of the best R packages ever. 
data(segmentationData)				# Get some data
data <- segmentationData[,-c(1,2)]
library(maptools)
library(MASS)
#------

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

palouse_sumloss_allcomm <- read.csv("/dmine/data/USDA/agmesh-scenarios/Allstates/summaries/palouse_summary_all.csv")
palouse_sumloss_allcomm2  <- aggregate(loss ~ year + damagecause + county + commodity,  palouse_sumloss_allcomm, sum)
palouse_count_allcomm2  <- aggregate(count ~ year + damagecause + county + commodity,  palouse_sumloss_allcomm, sum)

palouse_sumloss_allcomm2 <- palouse_sumloss_allcomm2[palouse_sumloss_allcomm2$loss >= 1, ]


#-Loading all WHEAT claims for the palouse from 1989-2015

palouse_sumloss <- read.csv("/dmine/data/USDA/agmesh-scenarios/Allstates/summaries/Palouse_summary_sumloss.csv")
palouse_counts <- read.csv("/dmine/data/USDA/agmesh-scenarios/Allstates/summaries/Palouse_summary_counts.csv")
#--drought claim counts all counties 
pc_drought <- subset(palouse_counts, damagecause == "Drought")

palouse_sumloss <- aggregate(loss ~ year + damagecause + county,  palouse_sumloss, sum)
palouse_counts <- aggregate(count ~ year + damagecause + county,  palouse_counts, sum)
pc_drought <- aggregate(count ~ year + damagecause + county,  pc_drought, sum)


palouse_sumloss_aggregate <- aggregate(palouse_sumloss$loss, list(palouse_sumloss$damagecause), FUN = "sum")


palouse_sumloss_2007_2015_aggregate <- subset(palouse_sumloss, year >= 2007 & year <= 2015 )
#-is there a normal signal using just wheat, drought claims across all of the pacific northwest
palouse_sumloss_2007_2015_aggregate <- aggregate(palouse_sumloss_2007_2015_aggregate$loss, list(palouse_sumloss_2007_2015_aggregate$damagecause), FUN = "sum")

palouse_sumloss_2015 <- subset(palouse_sumloss, year == 2015 )
palouse_sumloss_2015 <- aggregate(palouse_sumloss_2015$loss, list(palouse_sumloss_2015$damagecause), FUN = "sum")

palouse_sumloss_2009 <- subset(palouse_sumloss, year == 2009 )
palouse_sumloss_2009 <- aggregate(palouse_sumloss_2009$loss, list(palouse_sumloss_2009$damagecause), FUN = "sum")

palouse_sumloss_2011 <- subset(palouse_sumloss, year == 2011 )
palouse_sumloss_2011 <- aggregate(palouse_sumloss_2011$loss, list(palouse_sumloss_2011$damagecause), FUN = "sum")

palouse_sumloss_drought <- subset(palouse_sumloss, damagecause == "Drought")
qqnorm(palouse_sumloss_drought$loss)

#load wheat pricing

#barley <- read.csv("/dmine/data/USDAprices/barleyprices_1988_2017.csv", header=TRUE, strip.white =TRUE)
wheatprice <- read.csv("/dmine/data/USDAprices/wheatprices_1998_2017.csv", header=TRUE, strip.white =TRUE)
wheatprice_year <- aggregate(wheatprice$Price, list(wheatprice$Year), FUN="mean")
colnames(wheatprice_year) <- c("year", "price")

#merge wheat pricing with palouse_sumloss

palouse_sumloss <- merge(palouse_sumloss, wheatprice_year, by = "year")

#use a cube transformation on loss for WHEAT claims

Math.cbrt <- function(x) {
  sign(x) * abs(x)^(1/3)
}

#palouse_sumloss2 <- subset(palouse_sumloss, loss > 0)

palouse_sumloss$cube_loss <- Math.cbrt(palouse_sumloss$loss)
palouse_counts$cube_counts <- Math.cbrt(palouse_counts$count)

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

#pie chart for 2007-2015

palouse_sumloss_2007_2015_aggregate <- palouse_sumloss_2007_2015_aggregate[order(palouse_sumloss_2007_2015_aggregate$x),] 
X <- palouse_sumloss_2007_2015_aggregate[order(palouse_sumloss_2007_2015_aggregate$x),] 
colnames(X) <- c("damagecause", "loss")
summary_dataset <- X %>% filter(loss < 40000000) 
summary_dataset2 <- X %>% filter(loss > 40000000) 
colnames(summary_dataset) <- c("damagecause", "loss")
Y <- sum(summary_dataset$loss)
YY <- c("Other", Y)
YY <- data.frame(t(YY))
colnames(YY) <- c("damagecause", "loss")
YYY <- rbind(summary_dataset2, YY)
factor(YYY)

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
  layout(title = 'Top Damage Causes for Wheat, Palouse 26 County Region: 2007-2015',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

grid.table(YYY1)

#-pie chart for 1989-2015

p <- plot_ly(palouse_sumloss_aggregate, labels = ~Group.1, values = ~x, type = 'pie',
             textposition = 'inside',
             textinfo = 'label+percent',
             insidetextfont = list(color = '#FFFFFF'),
             hoverinfo = 'text',
             text = ~paste('$', x, ' billions'),
             marker = list(colors = colors,
                           line = list(color = '#FFFFFF', width = 1)),
             #The 'pull' attribute can also be used to create space between the sectors
             showlegend = TRUE) %>%
  layout(title = 'United States Personal Expenditures by Categories in 1960', showlegend = T,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))



#interaction plot for selected counties - wheat drought claims 1998-215


par(mar=c(5,6,4,2)+0.1,mgp=c(5,1,0))
interaction.plot(x.factor     = data_loss_county$year,
                 trace.factor = data_loss_county$county, 
                 response     = data_loss_county$loss, 
                 fun = sum,
                 las = 2,
                 type="b",
                 col=c("black","red","green", "blue", "yellow"),  ### Colors for levels of trace var.
                 #pch=c(19, 17, 15),             ### Symbols for levels of trace var.
                 fixed=FALSE,                    ### Order by factor order in data
                 leg.bty = "n",
                 ylab="Loss ($)",
                 xlab="years",
                 main="Interaction Plot - Loss ($) vs. year for Select Counties, 1998-2015", las = 2)



#--cube root loss vs year for all damage causes in Palouse

par(mar=c(5,6,4,2)+0.1,mgp=c(5,1,0))
interaction.plot(x.factor     = palouse_sumloss$year,
                 trace.factor = palouse_sumloss$damagecause, 
                 response     = as.numeric(as.character(palouse_sumloss$cube_loss)), 
                 fun = sum,
                 las = 2,
                 type="b",
                 col=c("black","red","green", "blue", "orange"),  ### Colors for levels of trace var.
                 pch=c(23, 22, 21, 20, 19, 17, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1),             ### Symbols for levels of trace var.
                 fixed=FALSE,                    ### Order by factor order in data
                 leg.bty = "n",
                 ylab="Loss ($)",
                 xlab="years",
                 main="Interaction Plot - Cube Root Loss ($) vs. year for Damage Causes in Palouse Region (WA, ID, OR)", las = 2)

#--loss vs. year for all counties, for select damage causes 1998-2015

par(mar=c(5,6,4,2)+0.1,mgp=c(5,1,0))
interaction.plot(x.factor     = palouse_sumloss_drought$year,
                 trace.factor = palouse_sumloss_drought$damagecause, 
                 response     = palouse_sumloss_drought$loss, 
                 fun = sum,
                 las = 2,
                 type="b",
                 col=c("black","red","green"),  ### Colors for levels of trace var.
                 pch=c(19, 17, 15),             ### Symbols for levels of trace var.
                 fixed=FALSE,                    ### Order by factor order in data
                 leg.bty = "n",
                 ylab="Loss ($)",
                 xlab="years",
                 main="Interaction Plot - Loss ($) vs. year for Select Damage Causes Palouse Region (WA, ID, OR)", las = 2)

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

var1 <- read.csv("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix_outputs/pr_jun2_cube_root_loss_climatecorrelation.csv")
colnames(var1)[9] <- paste(colnames(var1)[2], "_zscore", sep="")


var2 <- read.csv("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix_outputs/pet_jul3_cube_root_loss_climatecorrelation.csv")
colnames(var2)[9] <- paste(colnames(var2)[2], "_zscore", sep="")
var3 <- read.csv("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix_outputs/tmmx_jul2_cube_root_loss_climatecorrelation.csv")
colnames(var3)[9] <- paste(colnames(var3)[2], "_zscore", sep="")

var4 <- read.csv("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix_outputs/pr_jun2_cube_root_acres_climatecorrelation.csv")
colnames(var4)[9] <- paste(colnames(var4)[2], "_zscore", sep="")


var5 <- read.csv("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix_outputs/pet_jun2_loss_per_acre_climatecorrelation.csv")
colnames(var5)[9] <- paste(colnames(var5)[2], "_zscore", sep="")
var6 <- read.csv("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix_outputs/tmmx_jun1_cube_root_acres_climatecorrelation.csv")
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







data1 <- cbind(var1, var2[9], var3[9])
data2 <- cbind(var1[1:6], var2[2], var3[2])

data3 <- cbind(var4[1:6], var5[2], var6[2])

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

#--aov test
fit <- aov(loss_zscore ~ pr_zscore + tmmx_zscore + pet_zscore, data = data1)

fit <- aov(loss ~ pr + tmmx + pet, data = data2)

layout(matrix(c(1,2,3,4),2,2)) # optional layout 
plot(fit) # diagnostic plots

summary(fit) # display Type I ANOVA table
drop1(fit,~.,test="F") # type III SS and F Tests


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


fit <- lm(loss ~ pr + tmmx + pet, data = data2)







# Make big tree
form <- as.formula(loss_zscore ~ pr_zscore + tmmx_zscore + pet_zscore)

form2 <- as.formula(loss ~ pr + tmmx + pet)
tree.1 <- rpart(form2,data=data2,control=rpart.control(minsplit=30,cp=0))
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


rpart.plot(tree.3, digits = -3)



# A more reasonable tree
prp(tree.2)                                     # A fast plot													
fancyRpartPlot(tree.2)	

rpart.plot(tree.2, digits = -3)

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
fit <- rpart(loss ~ pr + tmmx + pet + price, 
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


#---random forest

library(randomForest)

rfit <- randomForest(loss ~ pr + tmmx + pet,  data=na.omit(data4a))
print(rfit) # view results 
importance(rfit) # importance of each predictor

getTree(rfit, 1, labelVar=TRUE)

#--count data

count(train, 'damagecause')

regre <- mgcv::gam(pet_zscore ~ pr_zscore + tmmx_zscore, data=data1)
VIF1 <- (1/(1-.89))

#manova

manova(loss ~ pr + tmmx + pet, data = data2)
res.man <- manova(cbind(pet, pr, tmmx, year) ~ county*loss, data = data2)
summary(res.man)
