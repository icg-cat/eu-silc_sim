---
title: "README"
author: "Irene Cruz, Lara Navarro-Varas, Sergio Porcel. Àrea Cohesió Social i Urbana - IERMB"
output:
  html_document:
    keep_md: yes
---


Este proyecto contiene el código empleado para ejecutar una micro-simulación de las rentas individuales a partir de los datos de la EU-SILC (ECV) y la LFS (EPA). Se trata de una simulación Monte Carlo al estilo *nowcasting*. El ejemplo que aquí se expone emplea datos para España (EU-SILC 2019, LFS 2020), pero podría adaptarse a cualquier país donde estos datos sean accesibles. 

A continuación se describe el procedimiento seguido: 

## Método de simulación

La simulación se divide en un proceso de cuatro etapas, cada una de las cuales se define por el tipo de resultado que produce.

> i.	En la primera etapa, usamos los datos más recientes sobre la situación del mercado laboral y extraemos los parámetros a nivel macro (p.e., tasas de desempleo) y a nivel micro (p.e., probabilidades individuales de sufrir desempleo).

> ii.	En la etapa dos, la última edición de la ECV se actualiza, generando un conjunto de variables necesarias para la simulación. Por una parte, se asignan las probabilidades a nivel micro extraídas en la etapa uno. Esta etapa también incluye el cálculo de los ingresos individuales en caso de pérdida del empleo o de ser afectado por un expediente de regulación temporal del empleo (ERTE). El resultado de la etapa dos es una nueva versión de la ECV que incluye tanto los datos de ingresos originales como los datos simulados de diferentes escenarios posibles a nivel individual.

> iii.	En la etapa tres se define un modelo con los mecanismos que activan los cambios de rentas a nivel individual i que dan como resultado los parámetros a nivel macro definidos en la etapa uno. Una simulación de Monte Carlo ejecuta el proceso de forma iterativa. En cada iteración de la simulación, se selecciona cuasi-aleatoriamente un conjunto de individuos (de acuerdo con las probabilidades individuales calculadas en la etapa 1) que se verán afectados por las distintas situaciones previstas con efecto sobre las rentas. Se recalculan los ingresos individuales de cada individuo seleccionado. Posteriormente, se recalculan los ingresos a nivel del hogar y se asignan (cuasi-aleatoriamente) las transferencias públicas (ingreso mínimo vital) entre los casos que cumplen los requisitos de elegibilidad. Una vez que se establecen las rentas del hogar simuladas, se calculan los indicadores estándar de distribución de los ingresos (p.e., índice de Gini, riesgo de pobreza, QIR). Este proceso se replica un elevado número de veces, dando lugar a una distribución probabilística de los indicadores sintéticos.

> iv.	La cuarta etapa consiste en el análisis de los resultados agregados y la identificación de la iteración que podría considerarse como “el escenario más plausible”, en virtud de tener el conjunto de resultados que más se asemeja al promedio del conjunto de simulaciones.

La Figura I sintetiza el procedimiento como diagrama de flujo. Cada una de las etapas se define con mayor detalle a continuación.

**Figura 1: Workflow del proceso de simulación**
![Figura 1: Workflow del proceso de simulación](https://github.com/icg-cat/eu-silc_sim/blob/main/Documentation/Screenshot%202021-08-06%20at%2012.49.21.png)

### Etapa 1. 

La EPA española para el año 2020 se utiliza para calcular tanto las tasas de desempleo, como las probabilidades individuales de desempleo, las probabilidades de cobrar una prestación de desempleo y la duración estimada del desempleo.

Las probabilidades observadas de sufrir desempleo se calculan mediante una regresión logística (función de enlace logit), en la que el sexo, la edad, el país de nacimiento y la categoría ocupacional son las variables independientes. Se ha observado que estos atributos sociales están altamente asociados con la situación laboral en este contexto particular, así como a nivel internacional y en particular en la crisis de covid19 (Joyce, Xu, 2020; Adams-Prass, Boneva, Golin et al., 2020)
Los datos para calcular la cobertura, los importes y la duración de los expedientes de regulación temporal de empleo (ERTES y programa equivalente para autónomos) y el ingreso mínimo vital (IMV) se basan en los datos publicados por el Ministerio de Inclusión, Seguridad Social y Migraciones.  

### Etapa 2. 

Los datos de partida en la simulación consisten en la encuesta de condiciones de vida (ECV/EU-SILC) para España, en su edición del 2019. Sobre estos datos, se realizan las siguientes transformaciones: 

*	Se deflactan las variables monetarias
*	Se determina la relación de los individuos con el mercado laboral durante el periodo de referencia. Se consideran como personas activas aquellas que se identifican como activas durante más de 5 meses durante el periodo de referencia de los datos de rentas, y aquellas que, a pesar de no identificar su actividad principal con relación a la actividad ningún mes, mantienen rentas del trabajo durante el periodo de referencia de las rentas.
*	Se asignan las probabilidades individuales de sufrir desempleo y de cobrar una prestación de desempleo (calculadas en la fase 1), para la población activa entre 16 y 64 años. 
*	Se asignan las probabilidades sectoriales de sufrir un expediente de regulación temporal de empleo (calculadas en la fase 1), para la población activa entre 16 y 64 años. 
*	Se asignan aleatoriamente una duración de los episodios de paro y ERTOs, según la distribución empírica observada en la fase 1. 
*	Se calculan las bases de cotización individuales (brutas). 

### Etapa 3. 

En la tercera etapa se aplica la simulación, dando lugar a una transformación de las rentas individuales del trabajo, y posteriormente de las rentas agregadas del hogar. Los mecanismos de transformación de rentas se activan con relación a 4 tipos de sucesos aleatorios dentro del proceso de simulación: 

- **Suceso aleatorio 1**- sufrir desempleo: entre las personas activas, se selecciona un subconjunto de casos que pasan a ser desempleados, durante un tiempo determinado (definido en la etapa 2). En caso de sufrir desempleo, pero no recibir una prestación de desempleo, las rentas del trabajo se recalculan como la base de cotización multiplicada por el número de meses que el individuo permanece ocupado.

- **Suceso aleatorio 2**- disfrutar de una prestación por desempleo: cada país tiene diferentes criterios para determinar la elegibilidad en las prestaciones por desempleo. En España, los criterios se basan en un historial laboral de 24 meses, que no se puede contrastar con los datos originales de la ECV. Alternativamente, se utiliza una asignación cuasialeatoria, basada en las probabilidades condicionales según perfiles sociodemográficos de los individuos, calculados a partir de la EPA 2020.

Para un individuo que ha sido seleccionado para recibir una prestación por desempleo, se recalculan las rentas del trabajo individuales como la base de cotización multiplicada por el número de meses que el individuo permanece ocupado. Las prestaciones por desempleo se calculan como el 70% de la base de cotización durante los primeros 6 meses y el 50% durante los siguientes meses. Se aplican los límites superior e inferior de las prestaciones de desempleo, de acuerdo con la Ley General de la Seguridad Social (8/2015). En caso de que la base de cotización no alcance el límite inferior de la prestación, se asigna un subsidio de desempleo (no contributivo), de acuerdo con la Ley General de la Seguridad Social (8/2015). 

- **Suceso aleatorio 3** - estar sujeto a un expediente de regulación temporal de empleo (ERTE): Solo las personas que están en activo y que no han sido seleccionadas para el paro pueden ser seleccionadas para un ERTE.

En caso de sufrir un expediente de regulación temporal del empleo, las rentas del trabajo se calculan como la base de cotización multiplicada por el número de meses que el individuo permanece ocupado, y la prestación por ERTE se calcula como el 70% de la base de cotización multiplicada por el número de meses de afectación. También se aplican los límites superior e inferior de las prestaciones de desempleo, de acuerdo con la Ley General de la Seguridad Social (8/2015). 

Siempre que una persona sufra una modificación en sus rentas del trabajo de acuerdo con uno de los tres episodios aleatorios descritos hasta el momento, se aplica una rectificación en el cálculo de los impuestos con tal de no grabar unas rentas que se dejan de ingresar. Cabe señalar que el cálculo de las retenciones puede ser muy complejo, teniendo en cuenta que éstas pueden variar en función de múltiples factores administrativos, laborales y personales. Para este ejercicio de simulación, el cálculo se ha simplificado aplicando un criterio uniforme por el cual la corrección resta al componente de impuestos el producto entre la diferencia de rentas tras la simulación por el porcentaje de retención inicialmente aplicado. 

Una vez realizados los cálculos en la simulación de rentas individuales, se procede a recalcular las rentas agregadas a nivel de hogar y se asignan las transferencias a esta escala (el ingreso mínimo vital):

- **Suceso aleatorio 4** – recibir una prestación de ingreso mínimo vital: Una vez recalculadas las rentas a nivel de hogar, se determina para cada unidad si cumple los requisitos de elegibilidad del programa. Entre la población elegible, se hace una selección aleatoria de los hogares beneficiarios, y se recalculan en consecuencia las rentas del hogar y sus ingresos equivalentes. 

Finalmente, a partir de la muestra con las nuevas rentas individuales y del hogar simuladas, se calcula una batería de indicadores sintéticos que dan cuenta de su distribución, incluyendo: el valor medio de las rentas de los hogares, la mediana de los ingresos equivalentes, el índice de Gini, la tasa de riesgo de pobreza moderada (60% de la mediana), y la tasa de riesgo de pobreza extrema (30% de la mediana). 

### Etapa 4. 

El resultado de la etapa anterior es un conjunto de datos con tantas columnas como indicadores estimados, y tantas filas como iteraciones realizadas para la simulación. Este conjunto de datos se analiza con dos fines: 1) testar la calidad del procedimiento, 2) comprender la distribución de los resultados obtenidos. Los tests realizados con estos fines incluyen: verificar la normalidad de los resultados obtenidos, evaluar los intervalos de confianza de las estimaciones y el rango de variabilidad obtenida, contrastar los coeficientes de variación de los resultados, verificar la concordancia entre las tasas proyectadas y las obtenidas después de la ponderación de los resultados. 

**Figura 2: Muestra de resultados 1: simulación para la ECV-España.**
![Figura 2: Muestra de resultados 1: simulación para la ECV-España.](https://github.com/icg-cat/eu-silc_sim/blob/main/Documentation/Screenshot%202021-08-06%20at%2011.55.25.png)


Por último, se calcula para cada iteración de la simulación una medida de distancia respecto a los valores medios del conjunto de simulaciones, y se extrae la referencia donde ésta sea menor. Puesto que los resultados de los indicadores clave tienen una distribución normal, se considera a la iteración más similar al promedio como el escenario más probable o plausible. Este escenario es extraído para realizar análisis a nivel micro.   

**Figura 3: Muestra resultados 2: simulación para EMCV-Área Metropolitana de Barcelona**
![Figura 3: Muestra resultados 2: simulación para EMCV-Área Metropolitana de Barcelona](https://github.com/icg-cat/eu-silc_sim/blob/main/Documentation/Screenshot%202021-09-13%20at%2008.44.58.png)
