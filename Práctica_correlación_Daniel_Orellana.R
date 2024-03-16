getwd()
nuevo_dir <- "C:/practicacorrelacion"
setwd(nuevo_dir)
if(file.exists(nuevo_dir)) {
  cat("Directorio creado correctamente: ", nuevo_dir, "\n")
} else {
  cat("Fallo al crear direcorio: ", nuevo_dir, "\n")
}

library(readxl)
View(data)
print(data)

#Actividad 3
correlacion_variables <- cor(data)
print(correlacion_variables)

#Actividad 4


panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0 ,1))
  Cor <- abs(cor(x, y)) 
  txt <- paste0(prefix, format(c(Cor, 0.123456789), digits = digits)[1])
  if(missing(cex.cor)) {
    cex.cor <- 0.4 / strwidth(txt)
  }
  text(0.5, 0.5, txt,
       cex = 1 + cex.cor*Cor)
}

pairs(data,
      upper.panel = panel.cor, 
      lower.panel = panel.smooth)


#Actividad 5
library(correlation)
matriz <- correlation(data)
matriz
print(matriz)


#Actividad 6
library(ggpubr)
library(ggplot2)
ggscatter(data, x = "altura", y = "peso",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "altura piezas (mm)", ylab = "peso piezas (mg)")

#Actividad 7
library(corrplot)
corrplot(cor(data))


#Actividad 8

#A)
distancia <- c( 1.1,100.2,90.3,5.4,57.5,6.6,34.7,65.8,57.9,86.1)
n_piezas <- c(110,2,6,98,40,94,31,5,8,10)
datos_act8 <- data.frame(distancia, n_piezas)
print(datos_act8)

#B
correlacion_datos_act8 <- cor(datos_act8)
print(correlacion_datos_act8)

#C
significancia_datos_act8 <- cor.test(datos_act8$distancia, datos_act8$n_piezas)$p.value
print(significancia_datos_act8)

#D
intervaloconfianza_datos_act8 <- cor.test(datos_act8$distancia, datos_act8$n_piezas)$conf.int
print(intervaloconfianza_datos_act8)


#Actividad 9

library(ggplot2)

set.seed(123) 
x_linear <- seq(1, 10, length.out = 100)
y_linear <- 2 * x_linear + rnorm(100, sd = 2)

x_monotonic <- seq(1, 10, length.out = 100)
y_monotonic <- sin(x_monotonic) + rnorm(100, sd = 0.5)

data_linear <- data.frame(x = x_linear, y = y_linear)
data_monotonic <- data.frame(x = x_monotonic, y = y_monotonic)

ggplot(data_linear, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Relación lineal", x = "Variable X", y = "Variable Y")

ggplot(data_monotonic, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Relación monótona no lineal", x = "Variable X", y = "Variable Y")

#Actividad 10

set.seed(123) 
x_monotonic <- seq(1, 10, length.out = 100)
y_monotonic <- sin(x_monotonic) + rnorm(100, sd = 0.5)
data_monotonic <- data.frame(x = x_monotonic, y = y_monotonic)
correlation <- cor.test(data_monotonic$x, data_monotonic$y, method = "spearman")
print(correlation)
