#Φόρτωση Βιβλιοθηκών :
install.packages("tidyverse")
library(tidyverse)

#Φόρτωση του dataset :
mall_data <- read.csv("C:/Users/sarel/OneDrive/Υπολογιστής/ΣΕΜΙΝΑΡΙΟ DATA ANALYST/project/DATA PROJECT WITH Rstudio/archivefreelancer.project/Mall_Customers.csv", header = T)
View(mall_data)

#Επισκόπηση των δεδομένων :
head(mall_data)
str(mall_data)
summary(mall_data)

# Κατανομή Ηλικίας Πελατών, για να δω πόσο συχνά εμφανίζονται διάφορες ηλικίες στο dataset:
hist(mall_data$Age, main = "Κατανομή Ηλικίας των Πελατών", xlab = "Ηλικία", col = "skyblue", border = "black")

# Κατανομή Ετήσιου Εισοδήματος, για να δω αν οι περισσότεροι πελάτες έχουν χαμηλό,μεσαίο ή υψηλό εισόδημα:
hist(mall_data$Annual.Income..k.., main = "Κατανομή Ετήσιου Εισοδήματος", xlab = "Ετήσιο Εισόδημα (χιλιάδες $)", col = "lightgreen", border = "black")

#Σχέση Εισοδήματος και Δείκτη Δαπανών, για να δω αν οι πελάτες με υψηλό εισόδημα ξοδεύουν 
#περισσότερο ή αν υπάρχουν διαφορετικά μοτίδα :
plot(mall_data$Annual.Income..k.., mall_data$Spending.Score..1.100., main = "Σχέση Εισοδήματος και Δαπανών", xlab = "Ετήσιο Εισόδημα (χιλιάδες $)", ylab = "Spending Score", col = "red", pch = 16)

#Boxplot για Ηλικία ανά Φύλο, για να δω αν υπάρχουν διαφορές στις ηλικίες ανδρών και γυναικών στο dataset:
boxplot(mall_data$Age ~ mall_data$Genre, main = "Ηλικία ανά Φύλο", xlab = "Φύλο", ylab = "Ηλικία", col = c("pink", "lightblue"))

#Θα κάνω k-means clustering για να ομαδοποιήσω τους πελάτες με βάση το Ετήσιο Εισόδημα και το Spending Score

#ΠΡΟΕΤΟΙΜΑΣΙΑ ΤΩΝ ΔΕΔΟΜΕΝΩΝ, επιλέγω τις μεταβλητές Ετήσιο Εισόδημα και Spending Score:

mall_data_cluster <- mall_data[, c("Annual.Income..k..", "Spending.Score..1.100.")]

#Εύρεση του βέλτιστου αριθμού clusters (Elbow Method), φόρτωση της βιβλιοθήκης για clustering:

install.packages("factoextra")
library(factoextra)

# Υπολογισμός WCSS (Within-Cluster Sum of Squares) για k από 1 έως 10:

set.seed(123)
wcss <- vector()
for (i in 1:10) {
  kmeans_model <- kmeans(mall_data_cluster, centers = i, nstart = 25)
  wcss[i] <- kmeans_model$tot.withinss
  
}

# Σχεδίαση του Elbow Plot :
plot(1:10, wcss, type = "b", pch = 19, frame = FALSE, xlab = "Αριθμός Clusters (k)", ylab = "WCSS", main = "Elbow Method για εύρεση βέλτιστου k")

#Εφαρμογή K-Means Clustering, επιλέγω το k=5 που βρήκα από το Elbow Method :
set.seed(123)
kmeans_result <- kmeans(mall_data_cluster, centers = 5, nstart = 25)

#Προσθέτω τα clusters στα δεδομένα :
mall_data$Cluster <- as.factor(kmeans_result$cluster)


#Οπτικοποίηση των Clusters:
library(ggplot2)

#Σχεδίαση των Clusters:
ggplot(mall_data, aes(x = Annual.Income..k.., y = Spending.Score..1.100., color = Cluster)) + geom_point(size = 3) + labs(title = "Τμηματοποίηση Πελατών με k-Means", x = "Ετήσιο Εισόδημα (k$)", y = "Spending Score (1-100)") + theme_minimal()

#Ανάλυση χαρακτηριστικών των Clusters

#Θα υπολογίσω μέσες τιμές για τις μεταβλητές Ετήσιο Εισόδημα και Spending Score σε κάθε cluster:

aggregate(mall_data[, c("Annual.Income..k..", "Spending.Score..1.100.")],
          by = list(Cluster = mall_data$Cluster),
          FUN = function(x) c(mean = mean(x), median = median(x), sd = sd(x)))

#Θέλω να καταλάβω πως διαφέρουν τα Clusters μεταξύ τους, οπότε φτιάχνω boxplot :

library(ggplot2)

# Boxplot για Ετήσιο Εισόδημα ανά cluster:

ggplot(mall_data, aes(x = as.factor(Cluster), y = Annual.Income..k.., fill = as.factor(Cluster))) + 
  geom_boxplot() + 
  theme_minimal() +
  labs(title = "Ετήσιο Εισόδημα ανά Cluster", x = "Cluster", y = "Ετήσιο Εισόδημα (k$)")


# Boxplot για Spending Score ανά cluster:

ggplot(mall_data, aes(x = as.factor(Cluster), y = Spending.Score..1.100., fill = as.factor(Cluster))) + 
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Spending Score ανά Cluster", x = "Cluster", y = "Spending Score (1-100)")

#Προβλέψεις με MACHINE LEARNING

#Φόρτωση και Προετοιμασία των Δεδομένων, χρειάζομαι τα (Annual_Income_k$,Spending_Score_1_100,Cluster):
install.packages("caret")
library(caret)

# Επιλογή χαρακτηριστικών:
data_ml <- mall_data[, c("Annual.Income..k..", "Spending.Score..1.100.", "Cluster")]
colnames(mall_data)
# Μετατροπή του Cluster σε factor (κατηγορία):
data_ml$Cluster <- as.factor(data_ml$Cluster)

# Διαχωρισμός σε training (80%) και testing (20%) set:
set.seed(123)
index <- createDataPartition(data_ml$Cluster, p = 0.8, list = FALSE)
train_data <- data_ml[index,]
test_data <- data_ml[-index, ]

# Εμφάνιση μερικών γραμμών:
head(train_data)

#θα χρησιμοποιήσω Random Forest:
install.packages("randomForest")
library(randomForest)
library(caret)

#Εκπαίδευση μοντέλου Random Forest:
set.seed(123) #για αναπαραγωγιμότητα
model_rf <- randomForest(Cluster ~ ., data = train_data, ntree = 100)

#Πρόβλεψη στο test set:
predictions <- predict(model_rf, newdata = test_data)

#Αξιολόγηση του μοντέλου:
conf_matrix <- confusionMatrix(predictions, test_data$Cluster)

#ΟΠΤΙΚΗ ΑΝΑΠΑΡΑΣΤΑΣΗ ΜΕ ggplot και reshape2 :
library(ggplot2)
library(reshape2)

#heatmap :

#Μετατροπή του confusion matrix σε dataframe για ggplot:
conf_matrix_table <- as.table(conf_matrix$table)
df_cm <- as.data.frame(conf_matrix_table)

#Heatmap του confusion matrix:
ggplot(df_cm, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "White", size = 5) +
  scale_fill_gradient(low = "lightblue", high = "red") +
  labs(title = "Confusion Matrix Heatmap",
       x = "Πραγματική Κλάση",
       y = "Προβλεπόμενη Κλάση") + theme_minimal()


#Ανάλυση Συσχετίσεων (Correlation Analysis), για να δω πως σχετίζονται οι μετ/τές μεταξύ τους:

#Θα υπολογίσω τη συσχέτιση μεταξύ των βασικών αριθμητικών μεταβλητών (Age, Annual.Income..k.., Spending.Score..1.100.):

# Υπολογισμός του πίνακα συσχέτισης:
cor_matrix <- cor(mall_data[, c("Age", "Annual.Income..k..", "Spending.Score..1.100.")])
names(mall_data)

#Οπτικοποίηση με Heatmap:
install.packages("ggcorrplot")
library(ggcorrplot)
ggcorrplot(cor_matrix, lab = TRUE, colors = c("blue", "white", "red"))



#Ηλικία vs Δείκτης Κατανάλωσης (για να δω αν οι μεγαλύτεροι καταναλωτές είναι πιο νέοι),scatter plot1
#Ετήσιο Εισόδημα vs Δείκτης Κατανάλωσης (για να δω αν το εισόδημα επηρεάζει την καταναλωτική συμπεριφορά),scatter plot2

library(ggplot2)

#Scatter Plot 1: Ηλικία vs Δείκτης Κατανάλωσης :
ggplot(mall_data, aes(x = Age, y = Spending.Score..1.100.)) +
  geom_point(color = "blue", alpha = 0.6) +
  labs(title = "Ηλικία vs Δείκτης Κατανάλωσης", x = "Ηλικία", y = "Δείκτης Κατανάλωσης (1-100)") +
  theme_minimal()


#Scatter Plot 2: Ετήσιο Εισόδημα vs Δείκτης Κατανάλωσης:
ggplot(mall_data, aes(x = Annual.Income..k.., y = Spending.Score..1.100.)) +
  geom_point(color = "red", alpha = 0.6) + 
  labs(title = "Ετήσιο Εισόδημα vs Δείκτης Κατανάλωσης", x = "Ετήσιο Εισόδημα", "Δείκτης Κατανάλωσης (1-100)") +
  theme_minimal()





