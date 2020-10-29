rm(list=ls())
graphics.off()
if (substring(getwd(),2,2) == ":") {
  setpath <- "/MEGAsync/Work/RA HKU/CSRP"
} else {
  setpath <- ""
}
setwd(sprintf("~%s", setpath))
source("helper_functions.R")

library(magrittr) # pipe 
library(dplyr) # group_by, etc
library(ggplot2) # ggplot and associated functions

setwd(sprintf("~%s/suicide_law", setpath))
df <- openxlsx::read.xlsx("191219_datafile for criminalization of suicide_updated.xlsx" )

df[df=='-' | 
     df=='..'] <- NA

names(df)[names(df) == "Country"] <- "country"
names(df)[names(df) == "Geographic.Region"] <- "regions"
df$regions <- relevel(as.factor(df$regions), ref='Eastern Asia')

names(df)[names(df) == "Region"] <- "continents" # continents
names(df)[names(df) == "HDI_2"] <- "HDI1" # HDI 1=high/veryhigh, 2=low/medium
names(df)[names(df) == "HDI"] <- "HDI2" # HDI factors
names(df)[names(df) == "2012.Human.Development.Index.(HDI)"] <- "HDI" # 2012 HDI index
df$HDI <- as.numeric(df$HDI)

names(df)[names(df) == "Income.Level"] <- "incomelv" 
names(df)[names(df) == "Income.Group.July.2013.by.World.Bank.(based.on.2012.GNI)"] <- "incomegroups" 
df$incomegroups <-  factor(df$incomegroups, levels = c("Low income", "Lower middle income", "Upper middle income", 
                                                        "High income: OECD", "High income: nonOECD"))

names(df)[names(df) == "Male.Suicide.Rate_2008.(every.100,000.persons)"] <- "male_suicide2008" 
names(df)[names(df) == "Male.Suicide.Rate_2012.(every.100,000.persons)"] <- "male_suicide2012"
names(df)[names(df) == "Female.Suicide.Rate_2008.(every.100,000.persons)"] <- "female_suicide2008" 
names(df)[names(df) == "Female.Suicide.Rate_2012.(every.100,000.persons)"] <- "female_suicide2012"
names(df)[names(df) == "Suicide.Rate_2008.(every.100,000.persons)"] <- "suicide2008" 
names(df)[names(df) == "Suicide.Rate_2012.(every.100,000.persons)"] <- "suicide2012"

df[,9:14] <- apply(df[,9:14], 2, function(x) as.numeric(as.character(x))); # change suicide rates from char to numeric

names(df)[names(df) == "Gross.national.income.(GNI).per.capita.(2011.PPP.)"] <- "GNI"
df$GNI <- as.numeric(df$GNI)

names(df)[names(df) == "criminalization.of.suicide_2012.(0=no,.1=yes)"] <- "illegal"

names(df)[names(df) == "%Muslim_2012"] <- "muslimpercent"
df$muslimpercent[df$muslimpercent=='> 99.0%'] <- '0.991'
df$muslimpercent[df$muslimpercent=='< 0.1%'] <- '0.00099'
df$muslimpercent <- as.numeric(df$muslimpercent)

names(df)[names(df) == "Unemploy-ment.rate_Male_.2008"] <- "male_unemployed2008"
names(df)[names(df) == "Unemploy-ment.rate_Female_2008"] <- "female_unemployed2008"
names(df)[names(df) == "Unemploy-ment.rate_Total_.2008"] <- "unemployed2008"
names(df)[names(df) == "Unemploy-ment.rate_Male_.2012"] <- "male_unemployed2012"
names(df)[names(df) == "Unemploy-ment.rate_Female_2012"] <- "female_unemployed2012"
names(df)[names(df) == "Unemploy-ment.rate_Total_.2012"] <- "unemployed2012"

# df$male_suicide_2008[which(df$country=='Hong Kong')] <- 17.5
# df$female_suicide_2008[which(df$country=='Hong Kong')] <- 10.9
# df$suicide2008[which(df$country=='Hong Kong')] <- 14
# df$suicide2012[which(df$country=='Hong Kong')] <- 12.8

df$suicide_diff2008 <- df$male_suicide2008 - df$female_suicide2008
df$suicide_diff2012 <- df$male_suicide2012 - df$female_suicide2012
df$unemployed_diff <- df$unemployed2012 - df$unemployed2008

summary(lm(female_suicide2012 ~ illegal + muslimpercent + HDI + factor(incomegroups) + factor(regions), data = df))

summary(glm(male_suicide2012 ~  illegal, data = df))

as.data.frame(
  df %>% 
    group_by(incomegroups, illegal) %>% 
    summarise_at(.vars = 'male_suicide2012', funs(n = length(.[!is.na(.)]),
                                                  mean(., na.rm=TRUE),
                                                  median(., na.rm=TRUE)
                                                  )
                 )
)

x <- df$muslimpercent
y <- df$suicide2012
z <- df$illegal
colors <- c("#999999", "#E69F00")
scatterplot3d::scatterplot3d(x, z, y, pch = 16, color = colors[as.factor(z)],
              grid = TRUE, box = FALSE, xlab = "Proportion of Muslims", 
              ylab = "unemployed2012", zlab = "2012 Suicide Rate (%)")


# Plot with main and axis titles
# Change point shape (pch = 19) and remove frame.
plot(x, y, main = "Main title",
     xlab = "X axis title", ylab = "Y axis title",
     pch = 19, 
     frame = FALSE)
# Add regression line
plot(x, y, main = "Main title",
     xlab = "X axis title", ylab = "Y axis title",
     # pch = 19, 
     frame = FALSE)
abline(lm(y ~ x, data = df), col = "blue")

# scatter plots by income groups
x <- df$muslimpercent
y <- df$suicide2012
ggplot(df, aes(x = x, y = y, colour = df$incomegroups)) +
  geom_point() + geom_smooth(method="lm") +
  facet_wrap( ~ df$incomegroups) + xlab("Proportion of Muslims") + ylab("2012 Suicide Rate (%)") + labs(colour = "Income Groups") 
  
# coloured boxplot suicide2012 by illegal & incomegroups
e <- ggplot(df, aes(x = incomegroup, y = male_suicide2008))
e2 <- e + geom_boxplot(
  aes(fill = ordered(plyr::mapvalues(df$illegal, from = c(0, 1), to = c("No", "Yes")), levels = c("Yes", "No"))),
  position = position_dodge(0.9) 
) + xlab("Income Groups") + ylab("2012 Male Suicide Rate (%)") + labs(fill = "Suicide Illegal") + scale_fill_hue(l=60, c=80)
e2


e + geom_boxplot(notch = TRUE, fill = "lightgray")+
  stat_summary(fun.y = mean, geom = "point",
               shape = 18, size = 2.5, color = "#FC4E07")

e2 <- e + geom_boxplot(
  aes(fill = supp),
  position = position_dodge(0.9) 
) +
  scale_fill_manual(values = c("#999999", "#E69F00"))
e2



