########################
### Sooahn Shin
## SNU PolMeth Boot Camp
## [Day 3] Data preprocess & visualization
## Part 2. Plot

# specify the working directory
setwd("~/Google Drive/Sooahn/SNU Method Camp/")
# preliminary work
source("part1_Tidy.R")

########################
# 2. Group with counts: Barplot
########################
subdat4

labordat_wide <- dat[dat$`Indicator Code` %in% c("SL.UEM.TOTL.FE.ZS", # Unemployment, female (% of female labor force) (modeled ILO estimate)
                                          "SL.UEM.TOTL.MA.ZS",   # Unemployment, male (% of male labor force) (modeled ILO estimate)
                                          "SL.TLF.TOTL.FE.IN",   # Labor force, female
                                          "SL.TLF.TOTL.IN"),]    # Labor force, total
# data after 1991
labordat_wide <- labordat_wide[, colnames(labordat_wide) %in% c("Country Name","Country Code","Indicator Name","Indicator Code",
                                           as.character(1991:2019))]
labordat_wide
str(labordat_wide)

labordat <- labordat_wide %>%
  select(-`Indicator Name`) %>%
  gather(-`Country Name`, -`Country Code`, -`Indicator Code`, key = "year", value = "value") %>%
  spread(key = `Indicator Code`, value = "value") %>%
  mutate(SL.TLF.TOTL.MA.IN = SL.TLF.TOTL.IN - SL.TLF.TOTL.FE.IN) %>%
  mutate(FE = (100-SL.UEM.TOTL.FE.ZS)*SL.TLF.TOTL.FE.IN) %>%
  mutate(MA = (100-SL.UEM.TOTL.MA.ZS)*SL.TLF.TOTL.MA.IN) %>%
  select(`Country Name`, `Country Code`, year, FE, MA) %>%
  gather(-`Country Name`, -`Country Code`,-year, key = "sex", value = "employment")

nrow(subdat4)  
3*nrow(labordat)

empdat <- left_join(subdat4, labordat, by = c("Country Name", "Country Code", "sex", "year"))

empdat <- empdat %>%
  mutate(counts = percent*employment/100) %>%
  select(-employment)

# ROK?
ggplot(data=subset(empdat, `Country Code`=="KOR"&year==2019), aes(x=sector, y=counts)) +
  geom_bar(stat="identity")

ggplot(data=subset(empdat, `Country Code`=="KOR"&year==2019), aes(x=sector, y=counts, fill=sex)) +
  geom_bar(stat="identity")

ggplot(data=subset(empdat, `Country Code`=="KOR"&year==2019), aes(x=sector, y=counts, fill=sex)) +
  geom_bar(stat="identity") +
  theme_classic()

ggplot(data=subset(empdat, `Country Code`=="KOR"&year==2019), aes(x=sector, y=counts, fill=sex)) +
  geom_bar(stat="identity") +
  theme_classic() +
  scale_fill_manual(name=NULL,values=c('orange','darkgreen'), breaks = c("FE", "MA"), labels=c("Female","Male"))

ggplot(data=subset(empdat, `Country Code`=="KOR"&year==2019), aes(x=sector, y=counts, fill=sex)) +
  geom_bar(stat="identity") +
  theme_classic() +
  scale_fill_manual(name=NULL,values=c('orange','darkgreen'), breaks = c("FE", "MA"), labels=c("Female","Male")) +
  labs(title = "Employment of the Rep. of Korea (2019)",
       caption = "source: World Bank")

ggplot(data=subset(empdat, `Country Code`=="KOR"&year==2019), aes(x=sector, y=counts, fill=sex)) +
  geom_bar(stat="identity", position=position_dodge()) + 
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_fill_manual(name=NULL,values=c('orange','darkgreen'), breaks = c("FE", "MA"), labels=c("Female","Male")) +
  scale_x_discrete(breaks = c("AGR","IND","SRV"), labels=c("Agriculture","Industry","Service"))+
  labs(title = "Employment of the Rep. of Korea (2019)",
       caption = expression(italic("Source: World Bank")))

# Stacked barplot
p1 <- ggplot(data=subset(empdat, `Country Code`=="KOR"&year==2019), aes(x=sector, y=counts, fill=sex)) +
  geom_bar(stat="identity", position=position_dodge()) + 
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_fill_manual(name=NULL,values=c('orange','darkgreen'), breaks = c("FE", "MA"), labels=c("Female","Male")) +
  scale_x_discrete(breaks = c("AGR","IND","SRV"), labels=c("Agriculture","Industry","Service"))+
  labs(title = "Employment of the Rep. of Korea",
       caption = expression(italic("Source: World Bank")))
p1

# Faceted plot
p2 <- ggplot(data=subset(empdat, `Country Code`=="KOR"&year%in%c(1999,2019)), aes(x=sector, y=counts, fill=sex)) +
  geom_bar(stat="identity", position=position_dodge()) + 
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_fill_manual(name=NULL,values=c('orange','darkgreen'), breaks = c("FE", "MA"), labels=c("Female","Male")) +
  scale_x_discrete(breaks = c("AGR","IND","SRV"), labels=c("Agriculture","Industry","Service"))+
  labs(title = "Employment of the Rep. of Korea",
       caption = expression(italic("Source: World Bank")))
p2
p2 <- p2 + facet_grid(. ~ year)
p2

# Time trends
p3 <- ggplot(data=subset(empdat, `Country Code`=="KOR"), aes(x=year, y=counts, fill=sex)) +
  geom_bar(stat="identity", position=position_dodge()) + 
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_fill_manual(name=NULL,values=c('orange','darkgreen'), breaks = c("FE", "MA"), labels=c("Female","Male")) +
  labs(title = "Employment of the Rep. of Korea",
       caption = expression(italic("Source: World Bank"))) +
  coord_flip()
p3

# Save plot
pdf(file = "Bar plot.pdf", width = 10, height = 5, family = "sans")
p3
dev.off()

# See http://www.sthda.com/english/wiki/ggplot2-barplots-quick-start-guide-r-software-and-data-visualization for more details.

##### Tips!
# color
library(wesanderson)
wes_palettes
wes_palette("GrandBudapest1")

ggplot(data=subset(empdat, `Country Code`=="KOR"&year==2019), aes(x=sector, y=counts, fill=sex)) +
  geom_bar(stat="identity", position=position_dodge()) + 
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_fill_manual(name=NULL,values=wes_palettes$GrandBudapest1[2:1], breaks = c("FE", "MA"), labels=c("Female","Male")) +
  scale_x_discrete(breaks = c("AGR","IND","SRV"), labels=c("Agriculture","Industry","Service"))+
  labs(title = "Employment of the Rep. of Korea (2019)",
       caption = expression(italic("Source: World Bank")))

# function
my.barplot <- function(country = "USA", period = 1991, title = NULL) {
  period <- as.character(period)
  if(is.null(title)) title <- paste0("Employment of ", country, " (", period,")")
  p <- ggplot(data=subset(empdat, `Country Code`%in%country&year%in%period), aes(x=sector, y=counts, fill=sex)) +
    geom_bar(stat="identity", position=position_dodge()) + 
    theme_minimal() +
    theme(legend.position = "bottom") +
    scale_fill_manual(name=NULL,values=wes_palettes$GrandBudapest1[2:1], breaks = c("FE", "MA"), labels=c("Female","Male")) +
    scale_x_discrete(breaks = c("AGR","IND","SRV"), labels=c("Agriculture","Industry","Service"))+
    labs(title = title,
         caption = expression(italic("Source: World Bank")))
  return(p)
}

my.barplot()
my.barplot(country = "JPN", period = 2019)
my.barplot(country = c("KOR", "JPN"), period = c(1999, 2019), title = "Employment") +
  facet_grid(`Country Code` ~ year)

########################
# 3. Individual with attributes: Scatter plot
########################

anes.all <- readstata13::read.dta13("data/anes_timeseries_2016_dta/anes_timeseries_2016_Stata13.dta")
anes <- anes.all[,c("V161158x", # party
               "V162180x", # Econ1: Gov should do more/less to regulate banks
               "V161201",  # Econ2: environment-jobs tradeoff self-placement
               "V162148",  # Econ3: Favor govt trying to reduce income inequality
               "V162140",  # Econ4: Favor or oppose tax on millionaires
               "V162186",  # Econ5: How much govt regulation of business is good for society
               "V161232",  # Soci1: STD Abortion: self-placement
               "V161231",  # Soci2: R position on gay marriage
               "V161228x", # Soci3: Transgender policy
               "V162364",  # Soci4: Discrimination in the U.S. against Muslims
               "V162357",  # Soci5: Discrimination in the U.S. against Blacks
               "V161265x")]# Religion
m <- ncol(anes)-1

# Remove non-response for simplicity 
nonrsps <- list()
nonrsps[[1]] <- c(-8,-9)
nonrsps[[2]] <- c(-6,-7,-8,-9)
nonrsps[[3]] <- c(99,-8,-9)
nonrsps[[4]] <- c(-6,-7,-8,-9)
nonrsps[[5]] <- c(-6,-7,-8,-9)
nonrsps[[6]] <- c(-6,-7,-8,-9)
nonrsps[[7]] <- c(5,-8,-9)
nonrsps[[8]] <- c(-8,-9)
nonrsps[[9]] <- c(-8,-9)
nonrsps[[10]] <- c(-5,-6,-7,-9)
nonrsps[[11]] <- c(-5,-6,-7,-9)
nonrsps[[12]] <- c(-2)

for(i in 1:m+1) {
  anes[anes[,i] %in% nonrsps[[i]],i] <- NA
}

anes[,4] <- recode(anes[,4], `2` = 3, `3` = 2)
anes[,5] <- recode(anes[,5], `2` = 3, `3` = 2)

range.rsps <- list()
for (i in c(1:3)) range.rsps[[i]] <- 1:7
for (i in c(4,5,8)) range.rsps[[i]] <- 1:3
for (i in c(6,10,11)) range.rsps[[i]] <- 1:5
for (i in c(9)) range.rsps[[i]] <- 1:6
for (i in c(7)) range.rsps[[i]] <- 1:4

for (k in 1:m) {
  range.var <- range.rsps[[k]]
  anes[,k] <- (anes[,k]-median(range.var))/max(range.var-median(range.var))
}
for (k in c(7,9)) {
  anes[,k] <- -anes[,k]
}

anes <- na.omit(anes)

anes$d1 <- rowMeans(anes[,2:6],na.rm = T)
anes$d2 <- rowMeans(anes[,7:11],na.rm = T)
# preprocess done! let's make a plot

cor(anes$d1,anes$d2)

plot(x=anes$d1, y=anes$d2, pch=19, col=rgb(red = 0.7, green = 0.2, blue = 0.2, 
                                           alpha=0.2)) # Tip!!!


p <- anes %>%
  ggplot(aes(x = d1, y = d2, col = V161158x)) + 
  geom_point(alpha=0.6, shape=18) +
  theme_bw() +
  theme(legend.position = "bottom",axis.title=element_text(size=24)) +
  labs(x="Economic Dimension",y="Social Dimension",title=NULL,
       caption = substitute(italic("Data: ANES 2016 Time Series Study"))) +
  scale_color_gradient2(low = "blue", mid = "grey",
                        high = "red", guide = "legend",
                        breaks = c(-1,0,1),
                        name=NULL,
                        labels = c("Democrat", "Independent",
                                   "Republican"),
                        limits = c(-1,1)) + 
  coord_fixed(xlim=c(-1,1),ylim=c(-1,1)) +
  geom_abline(slope = 1, linetype="dashed", size=0.8)

p
pdf(file="anes.pdf", family="sans", width=5, height=5)
p
dev.off()

## Boxplot
colnames(anes)
ggplot(anes, aes(x=as.factor(V161265x), y=d1)) + 
  geom_boxplot() +
  scale_x_discrete(labels = c("Mainline\nProtestant",
                              "Evangelical\nProtestant",
                              "Black\nProtestant",
                              "Roman\nCatholic",
                              "Undifferentiated\nChristian",
                              "Jewish",
                              "Other\nreligion",
                              "Not\nreligious")) +
  labs(x = "religion", y="Economic Dimension") +
  theme_bw() +
  stat_summary(fun.y=mean, geom="point", shape=5, size = 3, color="red") 
  
## Heatmap
sample <- anes[1:20,1:6]
head(sample)
sample$id <- 1:20
sample <- sample %>% gather(-id,key="variable", value = "value")
ggplot(sample, aes(variable, id)) + 
  geom_tile(aes(fill = value), colour = "white") +
  theme_light() + 
  scale_fill_gradient(low = "white",high = "steelblue") +
  coord_fixed(ylim=c(1,20), ratio = 6/20) 


########################
# 4. Regression: Prediction plot
########################

# just as a toy example... 
model <- lm(data = anes, d2~d1)
summary(model)

new.d1 <- data.frame(d1 = seq(from = -1, to = 1, by = 0.001))

predict(model, newdata = new.d1)[1:5]
predict(model, newdata = new.d1, interval = "confidence")[1:5,]
predict(model, newdata = new.d1, interval = "prediction")[1:5,]
pred.int <- predict(model, interval = "prediction")

pred.dat <- cbind(anes, pred.int)

pred.p <- ggplot(pred.dat, aes(d1, d2)) +
  geom_point(col="darkgrey",alpha=0.4) +
  stat_smooth(method = lm) +
  theme_bw() +
  labs(x="Economic Dimension",y="Social Dimension",title=NULL)
  
# add prediction intervals
pred.p + geom_line(aes(y = lwr), color = "red", size=1.3)+
  geom_line(aes(y = upr), color = "red", size=1.3)


## Tips!!!
colnames(anes)
model1 <- lm(data = anes, d2~V161158x+V162180x)
model2 <- lm(data = anes, d2~V162180x+V161201)
model3 <- lm(data = anes, d2~d1)

stargazer::stargazer(model1, model2, model3)


