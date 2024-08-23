train=read.csv("C:/Users/User/Downloads/train.csv")
train.backup=read.csv("C:/Users/User/Downloads/train.csv")
TEST=read.csv("C:/Users/User/Downloads/test.csv")
TEST.backup=read.csv("C:/Users/User/Downloads/test.csv")
head(train)
head(TEST)


df=train
df.TEST=TEST

summary(df)
summary(df.TEST)

#There are no missing values
#THere are 4200 outlets

#Checking Variable Sturctures
fs=factor(train$freezer_status)
pie(table(fs))
levels(fs)

#Checking WeekDate
date_column=train$week_start_date

#Function to check if a date is valid
is_valid_date=function(date_str){
  tryCatch({
    as.Date(date_str)
    return(TRUE)
  },error=function(e){
    return(FALSE)
  })
}

#Check for invalid dates
invalid_dates=date_column[!sapply(date_column, is_valid_date)]

#Display invalid dates
if(length(invalid_dates)>0) {
  cat("Invalid dates found:\n")
  print(invalid_dates)
}else{
  cat("No invalid dates found.\n")
}

#OUtlet Region
head(df$outlet_region)
or=factor(train$outlet_region)
levels(or)
pie(table(or))

#outlet_code
oc=factor(train$outlet_code)
length(oc)
length(levels(oc))

#Number of times each outlet is repeated in the dataset
as.integer(dim(df)[1]/length(levels(oc)))


#Data Cleaning
library(dplyr)

#Changing the format of the expected_rainfall variable
er=df$expected_rainfall
library(stringr)

er=as.numeric(substr(df$expected_rainfall,1,nchar(df$expected_rainfall)-2))
df$expected_rainfall=er
str(df$expected_rainfall)
head(df)

TEST$expected_rainfall=as.numeric(substr(TEST$expected_rainfall,1,nchar(TEST$expected_rainfall)-2))
head(TEST)

#Cleaning freezer_status
fs=df$freezer_status
fs[fs==" no freezers available "]="no freezers available"
fs[fs!="no freezers available"]="freezers available"
summary(factor(fs))
df$freezer_status=factor(fs)
str(df)

levels(factor(TEST$freezer_status))
TEST$freezer_status[TEST$freezer_status==" no freezers available "]="no freezers available"
TEST$freezer_status[TEST$freezer_status!="no freezers available"]="freezers available"
TEST$freezer_status=factor(TEST$freezer_status)

#Getting the dates
dates=as.Date(df$week_start_date,format = "%m/%d/%Y")
df$week_start_date=dates
str(df)

df2 = df %>% arrange(df$week_start_date)

df=df2

TEST$week_start_date=as.Date(TEST$week_start_date,format = "%m/%d/%Y")

#Getting the structue of the cleaned data
str(df)
str(TEST)

train=df

#Factorise outler_region and outlet_centre
df$outlet_region=factor(df$outlet_region)
df$outlet_code=factor(df$outlet_code)

TEST$outlet_region=factor(TEST$outlet_region)
TEST$outlet_code=factor(TEST$outlet_code)

str(df)
str(TEST)

x=df$week_start_date

weekno=as.numeric(format(x,"%d"))
daychar=as.character(format(x,"%d"))
monthno=as.numeric(format(x,"%m"))
monthchar=as.character(format(x,"%m"))

#Getting the number of weeks
df$month=monthno
df$Week=weekno
df$Week_No=paste(as.character(df$month),as.character(df$Week),sep = "-")
x=df$Week_No

head(x)
unique(x)
for (i in 1:length(unique(x))){
  x[x==unique(x)[i]]=i
}
df$Week_No=as.numeric(x)
x=factor(df$Week_No)

x=TEST$week_start_date

monthno=as.numeric(format(x,"%m"))

TEST$month=monthno
TEST$Week_No=28

summary(df)
tail(df)
sum(is.na(df$expected_rainfall))
train=df
dim(train)[1]

#Dividing Training and Testing Dataset
number.of.points.for.training=4200*25
main.train=train[1:number.of.points.for.training,]

main.test=train[(number.of.points.for.training+1):dim(train)[1],]

df=main.train
#Data Visualisation and EDA
library(ggplot2)

#Plot of the Density of the Expected Rainfall
plot(density(df$expected_rainfall),col = 'skyblue3',lwd=3, main="Distrubution of Expected rainfall")

#Pie Charts of Freezer Status and Outlet Region
pie(table(df$freezer_status),main="Pie Chart of Freezer status",col=c("orange","yellow"))
pie(table(df$outlet_region),main="Pie Chart of Outlet region",col=c("skyblue","pink","lightgreen"))

#Summary statistics for the training dataset
summary(df)

#Exploring the relationship between sales and expected rainfall
ggplot(df, aes(x = expected_rainfall, y = sales_quantity)) +
  geom_point() +
  labs(title = "Relationship between Sales and Expected Rainfall",
       x = "Expected Rainfall",
       y = "Sales Quantity") +
  theme_minimal()

cat("Correlation between the above 2 is", cor(df$expected_rainfall,df$sales_quantity))

# Exploring the relationship between sales and freezer status
ggplot(df, aes(x = freezer_status, y = sales_quantity, fill = freezer_status)) +
  geom_boxplot() +
  labs(title = "Relationship between Sales and Freezer Status",
       x = "Freezer Status",
       y = "Sales Quantity") +
  scale_fill_manual(values = c("skyblue", "orange")) +  
  theme_minimal()

# Exploring the relationship between sales and outlet region
ggplot(df, aes(x = outlet_region, y = sales_quantity,fill=outlet_region)) +
  geom_boxplot() +
  labs(title = "Relationship between Sales and Outlet Region",
       x = "Outlet Region",
       y = "Sales Quantity") +
  scale_fill_manual(values = c("skyblue", "green","purple"))+
  theme_minimal()

#Distribution among of rainfall across region
ggplot(df, aes(x = outlet_region, y = expected_rainfall,fill=outlet_region)) +
  geom_boxplot() +
  labs(title = "Relationship between Rainfall and Outlet Region",
       x = "Outlet Region",
       y = "Expected Rainfall")+
  theme_minimal()

tail(df)

#Distribution of sales acrross months
xm=factor(df$month)
str(df)

boxplot(df$sales_quantity~df$month)
ggplot(df, aes(x=xm, y=sales_quantity,fill=xm)) +
  geom_boxplot() +
  labs(title = "Relationship between sales quantity by month",
       x = "Month Number",
       y = "Sales Quantity") +
  scale_fill_manual(values = c("lightblue", "orange","yellow","green","pink","purple"))+
  theme_minimal()

#Plotting weekly sales quantity
agg.week.qty=aggregate(data=df,sales_quantity~factor(Week_No),mean)
ggplot(df, aes(x=Week_No,y=sales_quantity))+geom_point()+labs(title = "Weekly Sales Quantity",
                                                             x = "Week Number",
                                                             y = "Sales Quantity")


#Plotting monthly sales quantity
agg.month.qty=aggregate(data=df,sales_quantity~factor(month),mean)
ggplot(df, aes(x=month,y=sales_quantity))+geom_point()+labs(title = "Monthly Sales Quantity",
                                                            x = "Month",
                                                            y = "Sales Quantity")

#Feature Engineering

#New Feature : Categorizing Rainfall
high_rainfall_threshold= 150 
medium_rainfall_threshold = 50 

# Creating the expected_rainfall_category variable
df$expected_rainfall_category= cut(df$expected_rainfall,breaks = c(0, medium_rainfall_threshold, high_rainfall_threshold, 200),labels = c("Low", "Medium", "High"),include.lowest = TRUE)
main.test$expected_rainfall_category= cut(main.test$expected_rainfall,breaks = c(0, medium_rainfall_threshold, high_rainfall_threshold, 200),labels = c("Low", "Medium", "High"),include.lowest = TRUE)

#Converting to a factor variable
df$expected_rainfall_category=factor(df$expected_rainfall_category, levels = c("Low", "Medium", "High"))
main.test$expected_rainfall_category=factor(main.test$expected_rainfall_category, levels = c("Low", "Medium", "High"))

main.train=df

str(df)

#Extra Questions

df2=df[,c("outlet_region","sales_quantity","Week_No")]
unique(df2$outlet_region)

#average weekly sales volumes for each outlet region
library(dplyr)

df$Week_No <- as.numeric(df$Week_No)

# Group by outlet_region and Week_No, then calculate the mean sales_quantity
average_sales_by_region <- df %>%
  group_by(outlet_region, Week_No) %>%
  summarise(average_sales = mean(sales_quantity, na.rm = TRUE))

# Display the result
View(average_sales_by_region)



#testing the correlation between rainfall and the total weekly sales
res <- cor.test(df$expected_rainfall, df$sales_quantity, method = "pearson")
res

str(main.test)
str(main.train)

main.test=subset(main.test, select = -Week)
main.train=subset(main.train,select = -Week)

######################
# Assuming 'main.train' and 'main.test' are your training and testing datasets

# Create a lagged variable for sales (e.g., lag by one week)
main.train$lagged_sales <- c(NA, head(main.train$sales_quantity, -1))

# Remove rows with missing values introduced by lagging
main.train <- main.train[complete.cases(main.train), ]

# Build a linear regression model using lagged sales as a predictor
model <- lm(sales_quantity ~ lagged_sales, data = main.train)

# Make predictions on the test set
main.test$lagged_sales <- c(NA, head(main.test$sales_quantity, -1))  # Create lagged variable for test set
predictions <- predict(model, newdata = main.test)

# Calculate MAPE (Mean Absolute Percentage Error)
mape <- mean(abs((main.test$sales_quantity - predictions) / main.test$sales_quantity), na.rm = TRUE) * 100

# Display the MAPE
cat("Mean Absolute Percentage Error (MAPE):", mape, "%\n")
