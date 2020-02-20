wbcd <- read.csv("C:/Users/hp/Downloads/wbcd.csv")
dim(wbcd)
str(wbcd)

wbcd <- wbcd[-1]


table(wbcd$diagnosis)
barplot(table(wbcd$diagnosis))
barplot(table(wbcd$diagnosis), col=c("Green", "Red"), main="Bar Plot of Diagnosis")
text(barplot(table(wbcd$diagnosis), col=c("Green", "Red"), main="Bar Plot of Diagnosis"), 0, table(wbcd$diagnosis), cex=2, pos=3)

# Recode diagnostic variable
wbcd$diagnosis <- factor(wbcd$diagnosis, levels= c('B','M'), labels=c('Benign', 'Malignant'))
table(wbcd$diagnosis)

text(barplot(table(wbcd$diagnosis), col=c("Green", "Red"), main="Bar Plot of Diagnosis"), 0, table(wbcd$diagnosis), cex=2, pos=3)

round(prop.table(table(wbcd$diagnosis))*100, digits=2)

# Normalization
normalize <- function(x){return((x-min(x))/(max(x)-min(x)))}
normalize(c(1,2,3,4,5))
normalize(c(10,20,30,40,50))

# lapply()
wbcd_n <- as.data.frame(lapply(wcbd[2:31], normalize))
str(wbcd_n)
str(wbcd)

# Train and test data
wbcd_train <- wbcd_n[1:469,]
wbcd_test <- wbcd_n[470:569,]

# LaBEL VECTOR
wbcd_train_labels <- wbcd[1:469,1]
wbcd_test_labels <- wbcd[470:569,1]

str(wbcd_train_labels)
str(wbcd_test_labels)

# Training
#install.packages("class")
library(class)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, c1=wbcd_train_labels k=21)
summary(wbcd_test_pred)

install.packages('gmodels')
library(gmodels)
CrossTable(x=wbcd_test_labels, y=wbcd_test_pred, prop.chisq = FALSE)
