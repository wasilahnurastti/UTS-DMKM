# UTS-DMKM
27 Oktober 2020

## Library

```{r message=FALSE, warning=FALSE}
library(naivebayes)
library(psych)
library(caret)
library(corrplot)
library(mice)
library(dplyr)
```



## Import Data

```{r}
data <- read.csv("F:/!KULIAH/SEMESTER 5/MIDTERM V/DMKM/uts_dmkm/lymphography.data", header=F)
View(data)
head(data)
```


## cek missing values
```{r}
summary(data)
```


### Konversi Data
Ubah tipe variabel menjadi tipe faktor
```{r}
for(i in names(data)){
  data[,i]= as.factor(data[,i])
}
str(data)
```
merupakan target class yaitu car V1

### Pair Plot
Melihat korelasi dari tiap variabel, kalau ada korelasi yang tinggi, hilangkan salah satu variabel
```{r}
pairs.panels(data)
cor(data[, 1:19])

korelasi <- cor(data[,1:19])
corrplot(korelasi, method="circle")

```

### Split Data
Memecah data menjadi data training(80% dari data awal) dan data test (20% dari data awal)

```{r}
set.seed(1234)
sampel <- sample(2, nrow(data), replace = T, prob = c(0.8,0.2))
trainingdata <- data[sampel==1, ]
testingdata <- data[sampel==2, ]
print(paste("Jumlah Train Data: ", nrow(trainingdata), "| Jumlah Test Data: ", nrow(testingdata)))
```

### Membuat Model
Gunakan atribut `laplace` untuk menghilangkan zero probability problem
```{r message=FALSE, warning=FALSE}
modelnaive <- naive_bayes(V1~.,data=trainingdata)
modelnaive
```




## Prior probabilities

```{r}
summary(modelnaive)

```

### Confusion Matrix
```{r}
prediksi <- predict(modelnaive, testingdata)
confusionMatrix(table(prediksi,testingdata$V1))
```



##hapus V4

KOK SAMA HASILNYA

```{r}
newdata = select(data, -4)
head(newdata)
str(newdata)
sampel2 <- sample(2, nrow(newdata), replace = T, prob = c(0.8,0.2))
trainingdata2 <- newdata[sampel==1, ]
testingdata2 <- newdata[sampel==2, ]
print(paste("Jumlah Train Data: ", nrow(trainingdata2), "| Jumlah Test Data: ", nrow(testingdata2)))
modelnaive2 <- naive_bayes(V1~.,data=trainingdata2)
modelnaive2
summary(modelnaive2)
prediksi2 <- predict(modelnaive2, testingdata2)
confusionMatrix(table(prediksi2,testingdata2$V1))
```




## hapus v4 dan v5

KOK SAMA LAGI HASILNYA???

```{r}
newdata2 = select(data, -4:-5)
head(newdata2)
str(newdata2)
sampel3 <- sample(2, nrow(newdata2), replace = T, prob = c(0.8,0.2))
trainingdata3 <- newdata2[sampel==1, ]
testingdata3 <- newdata2[sampel==2, ]
print(paste("Jumlah Train Data: ", nrow(trainingdata3), "| Jumlah Test Data: ", nrow(testingdata3)))
modelnaive3 <- naive_bayes(V1~.,data=trainingdata3)
modelnaive3
summary(modelnaive3)
prediksi3 <- predict(modelnaive3, testingdata3)
confusionMatrix(table(prediksi3,testingdata3$V1))


```


