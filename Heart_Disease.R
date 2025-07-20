library(tidyverse)
library(hrbrthemes)

getwd()

setwd("C:\\Users\\Nicol\\R")

heart_disease<-read.csv('Datasets\\heart.csv')


View(heart_disease)

sapply(heart_disease, class)


dim(heart_disease)

#--------------------------------------------


heart_disease[is.na(heart_disease),]
sum(is.na(heart_disease))

heart_disease[duplicated(heart_disease),]
sum(duplicated(heart_disease))

## deleteing duplicate rows 
heart_disease<-heart_disease[!duplicated(heart_disease),]

dim(heart_disease)
#--------------------------------------------


## age info 
#summary(heart_disease$age)





summary_stats <- boxplot.stats(heart_disease$age)$stats
media_edad <- mean(heart_disease$age)

min_val   <- summary_stats[1]
q1_val    <- summary_stats[2]
median_val<- summary_stats[3]
q3_val    <- summary_stats[4]
max_val   <- summary_stats[5]



box_age <- ggplot(heart_disease, aes(x = "", y = age)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  
 
  annotate("text", x = 1.1, y = min_val,    label = paste("Min:", min_val), size = 3.5, hjust = 0) +
  annotate("text", x = 1.1, y = q1_val-1.5,     label = paste("Q1:", q1_val), size = 3.5, hjust = 0) +
  annotate("text", x = 1.0, y = median_val+1.5, label = paste("Mediana:", median_val), size = 3.5, hjust = 0) +
  annotate("text", x = 1.1, y = q3_val+1.5,     label = paste("Q3:", q3_val), size = 3.5, hjust = 0) +
  annotate("text", x = 1.1, y = max_val,    label = paste("Max:", max_val), size = 3.5, hjust = 0) +
  annotate("text", x = 0.9, y = media_edad, label = paste("Media:", round(media_edad, 1)), size = 3.5, hjust = 1, color = "red") +
  
  labs(title = "Distribución de la edad", y = "Edad", x = "") +
  theme_minimal()






breaks <- hist(heart_disease$age, breaks = "sturges", plot = FALSE)$breaks
histogram_age<-ggplot(heart_disease,aes(x=age))+geom_histogram(breaks=breaks,
                                                               fill="skyblue",
                                                               color="white",
                                                               linewidth = 1.5)+
  stat_bin(breaks = breaks, 
           geom = "text", 
           aes(label = ..count..), 
           vjust = -0.5, 
           color = "blue")

print(histogram_age)

print(box_age)


#----------------------------------------------

##Sex



# convirtiendo el genero en hombre y mujer para poder graficar mejor

heart_disease$sex_text <- factor(heart_disease$sex,
                            levels = c(0, 1),
                            labels = c("Mujer", "Hombre"))


bar_sex <- ggplot(heart_disease, aes(x = sex_text)) +
  geom_bar(fill = "skyblue", color = "white", linewidth = 1.5)+
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, color = "blue")
  labs(title = "Distribución por genero", x = "Sexo", y = "Frecuencia")
  
  
  
  pie_sex <- ggplot(heart_disease, aes(x = "", fill = sex_text)) +
    geom_bar( stat = "count", color = "white") +
    coord_polar(theta = "y") + 
    geom_text(stat = "count", 
              aes(label = paste0(round(100 * ..count../sum(..count..)), "%")), 
              position = position_stack(vjust = 0.5), 
              color = "white") +
              labs(title = " Distribución por Género", x = "", y = "")+
    theme_void()
 

print(bar_sex)
print(pie_sex)


#----------------------------------------------------------------------

## chest pain

# conviritendo a texto para graficar mejor

heart_disease$cp_text <- factor(heart_disease$cp,
                           levels = c(0, 1, 2, 3),
                           labels = c("Tp", "Atp", "No", "Asin"))




bar_cp <- ggplot(heart_disease, aes(x = cp_text)) +
  geom_bar(fill = "skyblue", color = "white", linewidth = 1.5)+
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, color = "blue")+
labs(title = "Chest pain ", x = "", y = "Frecuencia")


  pie_cp <- ggplot(heart_disease, aes(x = "", fill =cp_text)) +
    geom_bar( stat = "count", color = "white") +
    coord_polar(theta = "y") + #pie
    geom_text(stat = "count", 
              aes(label = paste0(round(100 * ..count../sum(..count..)), "%")), 
              position = position_stack(vjust = 0.5), 
              color = "white") +
    labs(title = " Chest pain", x = "", y = "")+
    theme_void()


print(bar_cp)
print(pie_cp)


#------------------------------------------------------------------------------
## resting blood pressure


summary_stats_rest <- boxplot.stats(heart_disease$trestbps)$stats
media_rest<- mean(heart_disease$trestbps)

cat("promedio",mean(heart_disease$trestbps))
cat("standard deviation",sd(heart_disease$trestbps))
    
min_val   <- summary_stats_rest[1]
q1_val    <- summary_stats_rest[2]
median_val<- summary_stats_rest[3]
q3_val    <- summary_stats_rest[4]
max_val   <- summary_stats_rest[5]


box_rest <- ggplot(heart_disease, aes(x = "", y = trestbps)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  
  
  annotate("text", x = 1.1, y = min_val,    label = paste("Min:", min_val), size = 3.5, hjust = 0) +
  annotate("text", x = 1.1, y = q1_val-3.5,     label = paste("Q1:", q1_val), size = 3.5, hjust = 0) +
  annotate("text", x = 1.0, y = median_val-2.5, label = paste("Mediana:", median_val), size = 3.5, hjust = 0) +
  annotate("text", x = 1.1, y = q3_val+3.5,     label = paste("Q3:", q3_val), size = 3.5, hjust = 0) +
  annotate("text", x = 1.1, y = max_val,    label = paste("Max:", max_val), size = 3.5, hjust = 0) +
  annotate("text", x = 0.9, y = media_rest+1.5, label = paste("Media:", round(media_rest, 1)), size = 3.5, hjust = 1, color = "red") +
  
  labs(title = "Distribución de rest blood pressure", y = "", x = "") +
  theme_minimal()

breaks <- hist(heart_disease$trestbps, breaks = "sturges", plot = FALSE)$breaks
histogram_box<-ggplot(heart_disease,aes(x=trestbps))+geom_histogram(breaks=breaks,
                                                               fill="skyblue",
                                                               color="white",
                                                               linewidth = 1.5)+ stat_bin(breaks = breaks, 
           geom = "text", 
           aes(label = ..count..), 
           vjust = -0.5, 
           color = "blue")+labs(x='resting blood pressure')


print(histogram_box)
print(box_rest)


#-----------------------------------------------------------------------

## cholesterol

summary_stats_chol <- boxplot.stats(heart_disease$chol)$stats
media_chol<- mean(heart_disease$chol)

cat("standard deviation",sd(heart_disease$chol))

min_val   <- summary_stats_chol[1]
q1_val    <- summary_stats_chol[2]
median_val<- summary_stats_chol[3]
q3_val    <- summary_stats_chol[4]
max_val   <- summary_stats_chol[5]

box_chol <- ggplot(heart_disease, aes(x = "", y = chol)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  
  
  annotate("text", x = 1.1, y = min_val,    label = paste("Min:", min_val), size = 3.5, hjust = 0) +
  annotate("text", x = 1.1, y = q1_val-5.5,     label = paste("Q1:", q1_val), size = 3.5, hjust = 0) +
  annotate("text", x = 1.0, y = median_val-6.0, label = paste("Mediana:", median_val), size = 3.5, hjust = 0) +
  annotate("text", x = 1.1, y = q3_val+9.5,     label = paste("Q3:", q3_val), size = 3.5, hjust = 0) +
  annotate("text", x = 1.1, y = max_val,    label = paste("Max:", max_val), size = 3.5, hjust = 0) +
  annotate("text", x = 0.9, y = media_chol+4.5, label = paste("Media:", round(media_chol, 1)), size = 3.5, hjust = 1, color = "red") +
  
  labs(title = "cholesterol", y = "", x = "") +
  theme_minimal()


breaks <- hist(heart_disease$chol, breaks = "sturges", plot = FALSE)$breaks
histogram_chol<-ggplot(heart_disease,aes(x=chol))+geom_histogram(breaks=breaks,
                                                                    fill="skyblue",
                                                                    color="white",
                                                                    linewidth = 1.5)+stat_bin(breaks = breaks, 
           geom = "text", 
           aes(label = ..count..), 
           vjust = -0.5, 
           color = "blue")+labs(title='cholestherol',x='')
  

print(hist(heart_disease$chol, breaks = "Sturges", plot = FALSE)$breaks) # vienddo los bins a detalle


print(histogram_chol)
print(box_chol)


#---------------------------------------------
## fasting blood sugar 

heart_disease$fbs_text <- factor(heart_disease$fbs,
                                levels = c(0, 1),
                                labels = c("No","Si"))


bar_fbs <- ggplot(heart_disease, aes(x = fbs_text)) +
  geom_bar(fill = "skyblue", color = "white", linewidth = 1.5)+
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, color = "blue")+
  labs(title = "fasting blood sugar > 120 mg/dl ", x = "", y = "Frecuencia")


pie_fbs <- ggplot(heart_disease, aes(x = "", fill =fbs_text)) +
  geom_bar( stat = "count", color = "white") +
  coord_polar(theta = "y") + #pie
  geom_text(stat = "count", 
            aes(label = paste0(round(100 * ..count../sum(..count..)), "%")), 
            position = position_stack(vjust = 0.5), 
            color = "white") +
  labs(title = " fasting blood sugar > 120 mg/dl", x = "", y = "")+
  theme_void()

print(pie_fbs)
print(bar_fbs)

#----------------------------------------------------------------------

## electrocardiographic results


heart_disease$restecg_text <- factor(heart_disease$restecg,
                                     levels = c(0, 1, 2),
                                     labels = c("Normal", "Leve", "Grave"))

bar_restecg <- ggplot(heart_disease, aes(x =restecg_text)) +
  geom_bar(fill = "skyblue", color = "white", linewidth = 1.5)+
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, color = "blue")+
  labs(title = "electrocardiographic intensity ", x = "", y = "Frecuencia")

pie_restecg <- ggplot(heart_disease, aes(x = "", fill =restecg_text)) +
  geom_bar( stat = "count", color = "white") +
  coord_polar(theta = "y") + #pie
  geom_text(stat = "count", 
            aes(label = paste0(round(100 * ..count../sum(..count..)), "%")), 
            position = position_stack(vjust = 0.5), 
            color = "white") +
  labs(title = " electrocardiographic intensity", x = "", y = "")+
  theme_void()

print(pie_restecg)
print(bar_restecg)


#------------------------------------------------------------------------------

## maximum heart rate achieved

summary_stats_thalach <- boxplot.stats(heart_disease$thalach)$stats
media_thalach<- mean(heart_disease$thalach)

cat("standard deviation",sd(heart_disease$thalach))

min_val   <- summary_stats_thalach[1]
q1_val    <- summary_stats_thalach[2]
median_val<- summary_stats_thalach[3]
q3_val    <- summary_stats_thalach[4]
max_val   <- summary_stats_thalach[5]


box_thalach <- ggplot(heart_disease, aes(x = "", y = thalach)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  
  
  annotate("text", x = 1.1, y = min_val,    label = paste("Min:", min_val), size = 3.5, hjust = 0) +
  annotate("text", x = 1.1, y = q1_val-5.5,     label = paste("Q1:", q1_val), size = 3.5, hjust = 0) +
  annotate("text", x = 1.0, y = median_val-6.0, label = paste("Mediana:", median_val), size = 3.5, hjust = 0) +
  annotate("text", x = 1.1, y = q3_val+9.5,     label = paste("Q3:", q3_val), size = 3.5, hjust = 0) +
  annotate("text", x = 1.1, y = max_val,    label = paste("Max:", max_val), size = 3.5, hjust = 0) +
  annotate("text", x = 0.9, y = media_thalach+7.5, label = paste("Media:", round(media_thalach, 1)), size = 3.5, hjust = 1, color = "red") +
  
  labs(title = "maximum heart rate achieved", y = "", x = "") +
  theme_minimal()



breaks <- hist(heart_disease$thalach, breaks = "sturges", plot = FALSE)$breaks
histogram_thalach<-ggplot(heart_disease,aes(x=thalach))+geom_histogram(breaks=breaks,
                                                                 fill="skyblue",
                                                                 color="white",
                                                                 linewidth = 1.5)+stat_bin(breaks = breaks, 
                                                                                           geom = "text", 
                                                                                           aes(label = ..count..), 
                                                                                           vjust = -0.5, 
                                                                                           color = "blue")+labs(title='maximun heart rate achieved',x='')


print(hist(heart_disease$thalach, breaks = "Sturges", plot = FALSE)$breaks)



print(box_thalach)

print(histogram_thalach)

#----------------------------------
# exang

heart_disease$exang_text <- factor(heart_disease$exang,
                                     levels = c(0, 1),
                                     labels = c("No", "Yes"))

bar_exang <- ggplot(heart_disease, aes(x =exang_text)) +
  geom_bar(fill = "skyblue", color = "white", linewidth = 1.5)+
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, color = "blue")+
  labs(title = "enigna(pain in the chest because lack of oxygen) ", x = "", y = "Frecuencia")

pie_exang <- ggplot(heart_disease, aes(x = "", fill =exang_text)) +
  geom_bar( stat = "count", color = "white") +
  coord_polar(theta = "y") + #pie
  geom_text(stat = "count", 
            aes(label = paste0(round(100 * ..count../sum(..count..)), "%")), 
            position = position_stack(vjust = 0.5), 
            color = "white") +
  labs(title = " enigna(pain in the chest because lack of oxygen)", x = "", y = "")+
  theme_void()

print(pie_exang)
print(bar_exang)

#-------------------------------------------------------------------------------------
##oldpeak

summary_stats_oldpeak <- boxplot.stats(heart_disease$oldpeak)$stats
media_oldpeak<- mean(heart_disease$oldpeak)

summary(heart_disease$oldpeak)

cat("standard deviation",sd(heart_disease$oldpeak))

min_val   <- summary_stats_oldpeak[1]
q1_val    <- summary_stats_oldpeak[2]
median_val<- summary_stats_oldpeak[3]
q3_val    <- summary_stats_oldpeak[4]
max_val   <- summary_stats_oldpeak[5]


breaks <- hist(heart_disease$oldpeak, breaks = "sturges", plot = FALSE)$breaks
histogram_oldpeak<-ggplot(heart_disease,aes(x=oldpeak))+geom_histogram(breaks=breaks,
                                                                       fill="skyblue",
                                                                       color="white",
                                                                       linewidth = 1.5)+stat_bin(breaks = breaks, 
                                                                                                 geom = "text", 
                                                                                                 aes(label = ..count..), 
                                                                                                 vjust = -0.5, 
                                                                                                 color = "blue")+labs(title='depression of the ST segment exam ',x='')


print(hist(heart_disease$oldpeak, breaks = "Sturges", plot = FALSE)$breaks)

print(histogram_oldpeak)


#-----------------------------------------------------------

## slope


heart_disease$slope_text <- factor(heart_disease$slope,
                                   levels = c(0,1,2),
                                   labels = c("down", "flat","up"))

bar_slope <- ggplot(heart_disease, aes(x =slope_text)) +
  geom_bar(fill = "skyblue", color = "white", linewidth = 1.5)+
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, color = "blue")+
  labs(title = "Type of slop (st segment exam)", x = "", y = "Frecuencia")

pie_slope <- ggplot(heart_disease, aes(x = "", fill =slope_text)) +
  geom_bar( stat = "count", color = "white") +
  coord_polar(theta = "y") + #pie
  geom_text(stat = "count", 
            aes(label = paste0(round(100 * ..count../sum(..count..)), "%")), 
            position = position_stack(vjust = 0.5), 
            color = "white") +
  labs(title = "Type of slop (st segment exam)", x = "", y = "")+
  theme_void()

print(pie_slope)
print(bar_slope)

#----------------------------------------------------------------------------------

## ca

bar_ca <- ggplot(heart_disease, aes(x =ca)) +
  geom_bar(fill = "skyblue", color = "white", linewidth = 1.5)+
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, color = "blue")+
  labs(title = "number of main coronary vessels affected :", x = "", y = "Frecuencia")

heart_disease$ca_tx <- factor(heart_disease$ca, levels = 0:4)

pie_ca <- ggplot(heart_disease, aes(x = "", fill =ca_tx)) +
  geom_bar( stat = "count", color = "white") +
  coord_polar(theta = "y") + #pie
  geom_text(stat = "count", 
            aes(label = paste0(round(100 * ..count../sum(..count..)), "%")), 
            position = position_stack(vjust = 0.5), 
            color = "white") +
  labs(title = "number of main coronary vessels affected : ", x = "", y = "")+
  theme_void()

print(pie_ca)
print(bar_ca)

#---------------------------------------------------------------------------------

## thal 

bar_thal <- ggplot(heart_disease, aes(x =thal)) +
  geom_bar(fill = "skyblue", color = "white", linewidth = 1.5)+
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, color = "blue")+
  labs(title = "type of defect the main vassels have   :", x = "", y = "Frecuencia")

heart_disease$thal_tx <- factor(heart_disease$thal, levels = 0:4)

pie_thal <- ggplot(heart_disease, aes(x = "", fill =thal_tx)) +
  geom_bar( stat = "count", color = "white") +
  coord_polar(theta = "y") + #pie
  geom_text(stat = "count", 
            aes(label = paste0(round(100 * ..count../sum(..count..)), "%")), 
            position = position_stack(vjust = 0.5), 
            color = "white") +
  labs(title = "type of defect the main vassels have  : ", x = "", y = "")+
  theme_void()

print(pie_thal)
print(bar_thal)

#-----------------------------------------------------------------------------------
## target

bar_target <- ggplot(heart_disease, aes(x =target)) +
  geom_bar(fill = "skyblue", color = "white", linewidth = 1.5)+
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, color = "blue")+
  labs(title = "does the person have a heart disease   :", x = "", y = "Frecuencia")

heart_disease$target_tx <- factor(heart_disease$target, levels = c(0, 1), labels = c("No", "Yes"))

pie_target <- ggplot(heart_disease, aes(x = "", fill =target_tx)) +
  geom_bar( stat = "count", color = "white") +
  coord_polar(theta = "y") + #pie
  geom_text(stat = "count", 
            aes(label = paste0(round(100 * ..count../sum(..count..)), "%")), 
            position = position_stack(vjust = 0.5), 
            color = "white") +
  labs(title = "does the person have a heart disease  : ", x = "", y = "")+
  theme_void()

print(pie_target)
print(bar_target)


#-------------------------------------------------------------------------

## deleting outliers




min_value <- min(heart_disease$thalach)

index_to_remove <- which(heart_disease$thalach == min_value)

heart_disease <- heart_disease[-index_to_remove, ]

dim(heart_disease)

heart_disease[heart_disease$thalach == 71,]

#----------------------------

## bivariate-- MULTIVARIATE analysis

## CORRELACION

install.packages("corrplot")
library(corrplot)

heart_disease_continuous <- heart_disease[, c('oldpeak', 'thalach', 'chol', 'trestbps', 'age')]

cor_matrix <- cor(heart_disease_continuous)

corrplot(cor_matrix, method = "number")



## CHI CUADRADO


heart_disease_categorical <- heart_disease[, c('sex', 'cp', 'fbs', 'restecg', 'exang','slope','ca','thal','target')]
heart_disease_categorical$target <- as.factor(heart_disease_categorical$target)

nombres_var <- c()
p_valores <- c()
significativo <- c()

for (columna in names(heart_disease_categorical)) {
  if (columna != "target") {
    tabla <- table(heart_disease_categorical[[columna]], heart_disease_categorical$target)
    resultado <- chisq.test(tabla)
    nombres_var <- c(nombres_var, columna)
    p_valores <- c(p_valores, resultado$p.value)
    significativo <- c(significativo, ifelse(resultado$p.value < 0.05, "Sí", "No"))
  }
}

resultado_chi <- data.frame(
  Variable = nombres_var,
  P_Valor = p_valores,
  Relación_Significativa = significativo
)

print(resultado_chi)



## SCATTER MATRIX
install.packages("gridExtra")
library(gridExtra)

categoricas <- c('sex_text', 'cp_text', 'fbs_text', 'restecg_text', 'exang_text','slope_text','ca_tx','thal_tx')


graficos <- list()


# age vs categorical
for (var in categoricas) {
  p <- ggplot(heart_disease, aes_string(x =var, y ="age", color = "factor(target)")) +
    geom_jitter(width = 0.2, alpha = 0.7) +
    labs(x =var, y ="age", color = "Target") +
    ggtitle(paste("Edad vs", var)) +
    theme_minimal()
  
  graficos[[var]] <- p
}


do.call(grid.arrange, c(graficos, ncol = 3))


# trestbps VS categorical


for (var in categoricas) {
  p <- ggplot(heart_disease, aes_string(x =var, y ="trestbps", color = "factor(target)")) +
    geom_jitter(width = 0.2, alpha = 0.7) +
    labs(x =var, y ="trestbps", color = "Target") +
    ggtitle(paste("trestbps vs", var)) +
    theme_minimal()
  
  graficos[[var]] <- p
}


do.call(grid.arrange, c(graficos, ncol = 3))



# CHOL VS categorical


for (var in categoricas) {
  p <- ggplot(heart_disease, aes_string(x =var, y ="chol", color = "factor(target)")) +
    geom_jitter(width = 0.2, alpha = 0.7) +
    labs(x =var, y ="chol", color = "Target") +
    ggtitle(paste("chol vs", var)) +
    theme_minimal()
  
  graficos[[var]] <- p
}


do.call(grid.arrange, c(graficos, ncol = 3))



# thalach VS categorical


for (var in categoricas) {
  p <- ggplot(heart_disease, aes_string(x =var, y ="thalach", color = "factor(target)")) +
    geom_jitter(width = 0.2, alpha = 0.7) +
    labs(x =var, y ="thalach", color = "Target") +
    ggtitle(paste("thalach vs", var)) +
    theme_minimal()
  
  graficos[[var]] <- p
}


do.call(grid.arrange, c(graficos, ncol = 3))


# Oldpeak VS categorical


for (var in categoricas) {
  p <- ggplot(heart_disease, aes_string(x =var, y ="oldpeak", color = "factor(target)")) +
    geom_jitter(width = 0.2, alpha = 0.7) +
    labs(x =var, y ="oldpeak", color = "Target") +
    ggtitle(paste("oldpeak vs", var)) +
    theme_minimal()
  
  graficos[[var]] <- p
}


do.call(grid.arrange, c(graficos, ncol = 3))





#Regresión logística

#eliminando columnas texto
heart_disease <- heart_disease[, -((ncol(heart_disease)-8):ncol(heart_disease))]


modelo_logit <- glm(target ~ ., data = heart_disease, family = binomial)
summary(modelo_logit)



install.packages("car")
library(car)

vif(modelo_logit)





