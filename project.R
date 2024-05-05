#Link to download the dataset : https://www.kaggle.com/datasets/dianaddx/aircraft-wildlife-strikes-1990-2023
strike_reports=read.csv("./data/STRIKE_REPORTS.csv")

install.packages(c('dplyr', 'leaflet', 'leaflet.extras', 'sp', 'plotly', 'arules', 'arulesViz', 'rpart', 'rpart.plot', 'caret', 'Metrics'))

#Load the packages
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(sp)
library(plotly)
library(arules)
library(arulesViz)

#Deleting the rows with missing localisation values to plot the heatmap
strike_reports_clean <- strike_reports[!is.na(strike_reports$LATITUDE) & !is.na(strike_reports$LONGITUDE), ]

#Creation of the heatmap of the accidents
map <- leaflet() %>%
  addTiles() %>%
  setView(lng = 0, lat = 0, zoom = 2) %>%
  addHeatmap(data = strike_reports_clean, lng=~LONGITUDE, lat=~LATITUDE, intensity = ~1, radius = 10)

#Show the map of the world
map

#Histogram of the number of strikes through the years
hist(strike_reports$INCIDENT_YEAR, main = "Number of wildlife strikes between 1990 and 2023", xlab="Year of the incident", ylab = "Number of wildlife strikes", xlim = c(1990,2023), breaks = c(1990:2023))

#Histogram of the number of strikes through the months
hist(strike_reports$INCIDENT_MONTH, main = "Number of wildlife strikes in relation to the month of the year", xlab="Month of the incident", ylab="Number of wildlife strikes", xlim=c(1,12), breaks=c(1:12))


#Interactive pie chart of the Airport
occurences= table(strike_reports$AIRPORT)
df_airport=as.data.frame(occurences)
colnames(df_airport)=c("AIRPORT", "Occurences")


fig=plot_ly(data = df_airport, labels=~AIRPORT, values=~Occurences, type='pie', textinfo='none')
fig


#Interactive pie chart of the operator
occurences= table(strike_reports$OPERATOR)
df_operator=as.data.frame(occurences)
colnames(df_operator)=c("OPERATOR", "Occurences")


fig=plot_ly(data = df_operator, labels=~OPERATOR, values=~Occurences, type='pie', textinfo='none')
fig

#Interactive pie chart of the time of day of the accidents
occurences= table(strike_reports$TIME_OF_DAY[strike_reports$TIME_OF_DAY!=""])
df_tod=as.data.frame(occurences)
colnames(df_tod)=c("TOD", "Occurences")


fig=plot_ly(data = df_tod, labels=~TOD, values=~Occurences, type='pie')
fig

#Interactive pie of the aircraft class
occurences= table(strike_reports$AC_CLASS[strike_reports$AC_CLASS!=""])
df_ac_class=as.data.frame(occurences)
colnames(df_ac_class)=c("AC_CLASS", "Occurences")

fig=plot_ly(data = df_ac_class, labels=~AC_CLASS, values=~Occurences, type='pie')
fig

#We will then replace every empty string by a N/A
strike_reports[strike_reports==""]=NA

#We will look at the average of missing values through the year
years=unique(strike_reports$INCIDENT_YEAR)
avg_missing_values=list()
for (year in years){
  sum_na=sum(is.na(strike_reports[strike_reports$INCIDENT_YEAR==year, ]))
  count_row=nrow(strike_reports[strike_reports$INCIDENT_YEAR==year, ])
  avg_missing_values=c(avg_missing_values,sum_na/count_row)
}
avg_missing_values=unlist(avg_missing_values)

plot(years, avg_missing_values)
#Interstingly enough, the average of missing value seems to rise through the years

#We will try to mine association rules for the possible cause of the accident
#For that we will try to select attributes that could cause the accident(i.e. TOD, phase of the flight, etc)
#We will use almost a all the columns to discover information. Information that we will delete are recurrent information(ie. code of Airport), irrelevant information(ie. Number of flight), unsuable information(ie. hour of day (a lot of missing information), or number of flight)

irrelevant_features=c("INDEX_NR", "INCIDENT_DATE", "INCIDENT_MONTH", "INCIDENT_YEAR",
                      "TIME", "AIRPORT_ID", "LATITUDE", "LONGITUDE", "RUNWAY", "FAAREGION",
                      "LOCATION", "ENROUTE_STATE", "OPID", "REG", "FLT", "AMA", "AMO", "EMA",
                      "EMO", "COST_REPAIRS", "INDICATED_DAMAGE", "COST_OTHER", "OTHER_SPECIFY",
                      "EFFECT_OTHER", "REMARKS", "SPECIES_ID", "BIRD_BAND_NUMBER", "COMMENTS",
                      "REPORTED_NAME", "REPORTED_TITLE", "REPORTED_DATE" , "SOURCE", "PERSON",
                      "LUPDATE", "TRANSFER")
strike_reports_cause=strike_reports[,!names(strike_reports) %in% irrelevant_features]
attributes_causes=c("TIME_OF_DAY", "AIRPORT", "STATE", "OPERATOR", "AIRCRAFT", "AC_CLASS",
                    "AC_MASS", "TYPE_ENG", "NUM_ENGS", "ENG_1_POS","ENG_2_POS","ENG_3_POS",
                    "ENG_4_POS", "ING_ENG1", "STR_ENG2", "ING_ENG2", "STR_ENG3", "ING_ENG3",
                    "STR_ENG4", "ING_ENG4", "STR_PROP", "STR_WING_ROT", "STR_FUSE", "STR_LG",
                    "STR_TAIL", "STR_LGHTS", "STR_OTHER", "INGESTED_OTHER", "PHASE_OF_FLIGHT",
                    "HEIGHT", "SPEED", "DISTANCE", "SKY", "PRECIPITATION", "SPECIES", "WARNED",
                    "NUM_SEEN", "NUM_STRUCK", "SIZE")
attributes_consequence=c("COST_REPAIRS_INFL_ADJ", "COST_OTHER_INFL_ADJ", "DAMAGE_LEVEL",
                         "DAM_RAD","DAM_WINDSHLD","DAM_NOSE", "DAM_ENG1", "DAM_ENG2",
                         "DAM_ENG3", "DAM_ENG4","DAM_PROP", "DAM_WING_ROT","DAM_FUSE",
                         "DAM_LG", "DAM_TAIL","DAM_LGTHS", "DAM_OTHER", "EFFECT",
                         "REMAINS_COLLECTED","REMAINS_SENT", "NR_INJURIES", "NR_FATALITIES")



rules=apriori(strike_reports_cause, 
              parameter=list(supp=0.3, conf=0.8, target="rules"))
inspect(rules)

#We find a lot of different rules but most of them are found because we have a high number of values and they are correlated but not relevant (ie. the type of engine and the position of the engine)
#To change that we will create a set of attributes for "causes" and a set of attributes for "consequences"
lhs_names=rules@lhs@itemInfo$labels
causes=list()
for (i in 1:length(lhs_names)){
  for (j in 1:length(attributes_causes)){
    if (grepl(attributes_causes[j], lhs_names[i])){
      causes=c(causes, list((lhs_names[i])))
      break
    }
  }
}
causes=unlist(causes)

rhs_names=rules@rhs@itemInfo$labels
consequences=list()
for (i in 1:length(rhs_names)){
  for (j in 1:length(attributes_consequence)){
    if (grepl(attributes_consequence[j], rhs_names[i])){
      consequences=c(consequences, list((rhs_names[i])))
      break
    }
  }
}
consequences=unlist(consequences)

filtered_rules=subset(rules, (lhs %in% causes) & (rhs %in% consequences))
#We remove the redundant rules
filtered_rules=filtered_rules[!is.redundant(filtered_rules)]
inspect(filtered_rules)

#Top 10 rules by lift
inspect(head(sort(filtered_rules, by="lift"), n=10))

#We have a lot of rules but they do not make a lot of sense, to see clearer in the rules
#To see clearer, we propose to look at a graphic representation of the different rules we have
#We sort them by lift
subrules=head(filtered_rules, n=8, by='lift')
plot(subrules, method='graph', engine="htmlwidget")

#We are more specifically searching for causes of damage, we will focus on damage level
inspect(subset(filtered_rules, subset = rhs %pin% "DAMAGE_LEVEL"))

#As we inspect those rules, we can see that all of them are for no damage level, this is probably because there is a majority of strike that did not damage the plane
occurences= table(strike_reports_cause$DAMAGE_LEVEL[!is.na(strike_reports_cause$DAMAGE_LEVEL)])
df_damage=as.data.frame(occurences)
colnames(df_damage)=c("DAMAGE_LEVEL", "Occurences")

fig=plot_ly(data = df_damage, labels=~DAMAGE_LEVEL, values=~Occurences, type='pie')
fig

#As we can see nearly 90% of the strikes did not damage the airplane. We will then focus only on the operation that did damage

strike_reports_damage=strike_reports_cause[!is.na(strike_reports_cause$DAMAGE_LEVEL) & strike_reports_cause$DAMAGE_LEVEL!="N",]

#Since we have narrowed down our dataset, we can mine new rules
#As we have less data, we will lower our minimum support and confidence to find more rules
rules=apriori(strike_reports_damage, parameter=list(supp=0.2, conf=0.5, target="rules"))
inspect(rules)

#We filter those rules
lhs_names=rules@lhs@itemInfo$labels
causes=list()
for (i in 1:length(lhs_names)){
  for (j in 1:length(attributes_causes)){
    if (grepl(attributes_causes[j], lhs_names[i])){
      causes=c(causes, list((lhs_names[i])))
      break
    }
  }
}
causes=unlist(causes)

rhs_names=rules@rhs@itemInfo$labels
consequences=list()
for (i in 1:length(rhs_names)){
  for (j in 1:length(attributes_consequence)){
    if (grepl(attributes_consequence[j], rhs_names[i])){
      consequences=c(consequences, list((rhs_names[i])))
      break
    }
  }
}
consequences=unlist(consequences)

filtered_rules=subset(rules, (lhs %in% causes) & (rhs %in% consequences))
#We remove the redundant rules
filtered_rules=filtered_rules[!is.redundant(filtered_rules)]
inspect(filtered_rules)

#Top 10 rules by lift
inspect(head(sort(filtered_rules, by="lift"), n=10))

#Graphic representation
#We sort them by lift
subrules=head(filtered_rules, n=8, by='lift')
plot(subrules, method='graph', engine="htmlwidget")
#We can see from the most common rules that a struck wing or rotor almost always result in damage in the same region

#We find a lot of different rules but most of them are found because we have a high number of values and they are correlated but not relevant (ie. the type of engine and the position of the engine)
inspect(subset(filtered_rules, subset=rhs %pin% "DAMAGE_LEVEL"))


#We did not find relevant information for the cause but we can try to find the consequences of the damage
inspect(subset(rules, subset=lhs %pin% "DAMAGE_LEVEL" & rhs %in% consequences))

#We will try doing a decision tree to see if we can predict if there is damage or not
#For that, we use all the causes and use the the damage level as a label

library(rpart)
library(rpart.plot)

cause_df=strike_reports[,c(attributes_causes, "DAMAGE_LEVEL")]
cause_df=cause_df[!is.na(cause_df$DAMAGE_LEVEL),]
cause_df$HEIGHT=as.numeric(cause_df$HEIGHT)
cause_df$SPEED=as.numeric(cause_df$SPEED)
cause_df[sapply(cause_df, is.character)] = lapply(cause_df[sapply(cause_df, is.character)], as.factor)
#Since all the integer columns are also levels, we also convert them to factors
cause_df[sapply(cause_df, is.integer)] = lapply(cause_df[sapply(cause_df, is.integer)], as.factor)

#We remove the columns with too many factors, otherwise the training would take too long
heavy_features=c("SPECIES", "AIRPORT","OPERATOR","STATE","AIRCRAFT")
cause_df=cause_df[,!names(cause_df) %in% heavy_features]

#We split the dataset between train and test set
set.seed(42)
split=sample(c(TRUE, FALSE), nrow(cause_df), replace=TRUE, prob=c(0.7,0.3))
train_cause=cause_df[split,]
test_cause=cause_df[!split,]

damage_tree=rpart(DAMAGE_LEVEL~., data=train_cause)

prp(damage_tree, extra=1)

predict(damage_tree, test_cause)
#As we can see from our resulting tree, since we have a highly imbalanced dataset, the tree is ineffective, we will try to balance that
#A first thing we can do is instead of having the damage level, we will focus on if the aircraft indicated damage or not

cause_df=strike_reports[,c(attributes_causes, "INDICATED_DAMAGE")]
cause_df=cause_df[!is.na(cause_df$INDICATED_DAMAGE),]
cause_df$INDICATED_DAMAGE=factor(cause_df$INDICATED_DAMAGE)
cause_df$HEIGHT=as.numeric(cause_df$HEIGHT)
cause_df$SPEED=as.numeric(cause_df$SPEED)
cause_df[sapply(cause_df, is.character)] = lapply(cause_df[sapply(cause_df, is.character)], as.factor)
#Since all the integer columns are also levels, we also convert them to factors
cause_df[sapply(cause_df, is.integer)] = lapply(cause_df[sapply(cause_df, is.integer)], as.factor)

#We remove the columns with too many factors, otherwise the training would take too long
heavy_features=c("SPECIES", "AIRPORT","OPERATOR","STATE","AIRCRAFT")
cause_df=cause_df[,!names(cause_df) %in% heavy_features]

#We split the dataset between train and test set
set.seed(42)
split=sample(c(TRUE, FALSE), nrow(cause_df), replace=TRUE, prob=c(0.7,0.3))
train_cause=cause_df[split,]
test_cause=cause_df[!split,]

table(cause_df$INDICATED_DAMAGE)

#We will now undersample the train set to save imbalancy
library(caret)
undersampled_data <- downSample(x = train_cause[, -ncol(train_cause)],
                                y = train_cause$INDICATED_DAMAGE)
undersampled_data = undersampled_data %>% rename(INDICATED_DAMAGE=Class)

damage_tree=rpart(INDICATED_DAMAGE~., data=undersampled_data)
prp(damage_tree, extra=1)

prediction=predict(damage_tree, test_cause, type="class")

table(test_cause$INDICATED_DAMAGE, prediction)

library(Metrics)
accuracy(test_cause$INDICATED_DAMAGE, prediction)
recall(as.logical(test_cause$INDICATED_DAMAGE), as.logical(prediction))
