getwd()


cust = read.csv('Customer_Churn.csv')
View(cust)

cust = cust[-c(1)]

churners = cust[cust$Churn == 'Yes', ]

View(churners)


#changing senior citizens to factors

churners$SeniorCitizen =  as.factor(churners$SeniorCitizen)

max(churners$tenure)
min(churners$tenure)

#Discretize tenure  and give labing 

churners$tenure = cut(churners$tenure,
                      breaks = c(0,12,24,36,48, 72,Inf),
                      labels = c("one year", "two years", "three years","four years", "five years", "senior" ))






#Discretize Monthly charges  and give labing 

min(churners$MonthlyCharges)
max(churners$MonthlyCharges)

churners$MonthlyCharges = cut(churners$MonthlyCharges,
                              breaks = c(0,25,50,75,100,Inf),
                              labels = c("frugal", "average", "moderate", "spender", "lavish" ))





#Discretize Monthly charges  and give labing 

min(churners$TotalCharges)
max(churners$TotalCharges)

churners$TotalCharges = cut(churners$TotalCharges,
                            breaks = c(0,2000,4000,6000,8000,Inf),
                            labels = c("small", "average", "medium", "old spender", "big spender" ))






#customer who churned


install.packages('arules')
library(arules)





itemFrequencyPlot(cust, topN = 10)

# Training Apriori on the dataset
rules = apriori(data = churners, parameter = list(support = 0.50, confidence = 0.60))

rules


# Visualising the results
inspect(sort(rules, by = 'lift')[1:10])
