# Bienvenidx
En este repositorio encontrarás mi Tesis de licenciatura **Estimación Paramétrica para Procesos de Difusión y Aplicaciones en Finanzas**. En esta tesis se da una base teórica sobre cálculo estocástico, ecuaciones diferenciales estocásticas, procesos de difusión, y simulación y estimación paramétrica de dichos procesos. 

La parte teórica sobre simulacoón y estimación paramétrica se complementa con implementación computacional de simuladores y estimadores. Se realiza estimación máximo verosímil y estimadores EM para los procesos de Vasicek y movimiento browniano geométrico.

Finalmente, las bases son aplicadas a dos aplicaciones financieras: modelos de tasa corta usando el proceso de Vasicek, valuación de opciones usando movimiento browniano geométrico. A partir de bases de datos reales se ajustan los modelos y se realiza estimación paramétrica.

Dentro de la carpeta Códigos se encuentran los diferentes scripts utilizados para la realización de la Tesis. 
-En los archivos MBGeometrico.R, MovimientoBrowniano.R, PuenteBrowniano.R, PuenteDifusion.R y Vasicek.R se tiene la simulación de los procesos correspondientes. 
-En el archivo Calibrar_puente_vasicek.R se hace una calibración para verificar que el la distribución de los puentes de difusión simulados (en PuenteDifusión.R) coicida con la distribución teórica. 
-En los archivos MLEVasicek.R y MLEmbg.R se implementan los estimadores máximo verosímiles para los procesos de Vasicek y movimiento browniano geométrico, respectivamente.
-En los archivos EM_Vasicek.R y EM_mbg.R se implementan los estimadores EM para los procesos de Vasicek y movimiento browniano geométrico, respectivamente.
-En los archivos Opciones.R y TasaCorta_Canada.R se encuentra la aplicación a modelos financieros.

Dentro de la carpeta Bases de Datos se encuentra:
-En los archivos Tasas_Canada.csv y VOO.csv los datos reales usados para la aplicación de modelos de tasa corta y valuación de opciones, respectivamente.
-En los archivos Estimadores_MBG.xlsx, Estimadores_Vasicek.xlsx y Estimadores_Vasicek_sin_sesgo.xlsx los estimadores obtenidos para cada proceso, con los diferentes métodos de estimación
-En los archivos Muestras_Validación_Opciones.xlsx y Muestras_Validación_Tasa_Corta.xlsx las muestras utilizadas para validar que los modelos propuestos y parámetros propuestos sí se ajustan a los datos reales.
