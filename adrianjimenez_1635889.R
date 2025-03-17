##############################
##############################
## SEMINARIO ECG Y OUTLIERS ##
##############################
##############################

###################################
# Adrian Jimenez Franco - 1635889 #
###################################



# Leer el archivo .hea para ver la información
hea_content <- readLines("ECGPCG0003.hea")
print(hea_content)

num_muestras <- 240000 * 2  # Multiplicamos por 2 porque hay 2 señales (ECG y PCG)

ecg_data <- readBin("ECGPCG0003.dat", what = "integer", size = 2, signed = TRUE, endian = "little", n = num_muestras)

# Verificar la cantidad de datos leídos
length(ecg_data)
head(ecg_data)
length(ecg_data) == 480000 #comprobar que se ha leido bien


# Extraer valores de ECG y PCG del archivo .dat
ecg <- ecg_data[seq(1, length(ecg_data), by = 2)]
pcg <- ecg_data[seq(2, length(ecg_data), by = 2)]

# Verificar que ambas señales tengan la mitad de los datos
length(ecg)  # Debería ser 240000
length(pcg)  # Debería ser 240000


# unidades físicas (mV)
# Factores de conversión desde el archivo .hea
gain_ecg <- 110554.8863
baseline_ecg <- 10148

# Convertir ECG a mV
ecg_mV <- (ecg - baseline_ecg) / gain_ecg


# Crear el eje de tiempo
fs <- 8000  # Frecuencia de muestreo
t <- seq(0, length(ecg_mV) - 1) / fs


# Cargar la librería de gráficos
library(ggplot2)

# Convertir datos a un data frame
df <- data.frame(Time = t, Signal = ecg_mV)

# Graficar
ggplot(df, aes(x = Time, y = Signal)) +
  geom_line() +
  labs(title = "Señal ECG", x = "Tiempo (s)", y = "Amplitud (mV)") +
  theme_minimal()



##
# Detectar picos R en el ECG
##

install.packages("pracma")
library(pracma)

# Filtrar la señal ECG para mejorar la detección
ecg_filtered <- movavg(ecg_mV, n = 5, type = "s")  # Media móvil para suavizar

# Detectar picos R (máximos locales)
picos_R <- findpeaks(ecg_filtered, nups = 1, npeaks = 1000)  # Ajusta npeaks según el ECG

# Extraer tiempos de los picos R
fs <- 8000
tiempos_picos_R <- picos_R[,2] / fs  # Convertir a segundos

# Graficar la señal con los picos R
plot(seq(0, length(ecg_mV) - 1) / fs, ecg_mV, type = "l", col = "blue", xlab = "Tiempo (s)", ylab = "Amplitud (mV)")
points(tiempos_picos_R, ecg_mV[picos_R[,2]], col = "red", pch = 19)  # Marcar picos R en rojo


##
# Calcular intervalos RR
##

intervalos_RR <- diff(tiempos_picos_R)  # Diferencia entre tiempos de picos R

# Graficar la serie de intervalos RR
plot(intervalos_RR, type = "l", col = "darkgreen", xlab = "Latido", ylab = "Intervalo RR (s)", main = "Serie de Intervalos RR")



##
# Detectar valores anómalos en los intervalos RR
##

Q1 <- quantile(intervalos_RR, 0.25)
Q3 <- quantile(intervalos_RR, 0.75)
IQR_val <- Q3 - Q1

# Definir límites de valores normales
lim_inf <- Q1 - 1.5 * IQR_val
lim_sup <- Q3 + 1.5 * IQR_val

# Identificar valores anómalos
anomalos <- which(intervalos_RR < lim_inf | intervalos_RR > lim_sup)

# Graficar con anomalías resaltadas
plot(intervalos_RR, type = "l", col = "darkgreen", xlab = "Latido", ylab = "Intervalo RR (s)", main = "Intervalos RR con Anomalías")
points(anomalos, intervalos_RR[anomalos], col = "red", pch = 19)  # Marcar anomalías en rojo


