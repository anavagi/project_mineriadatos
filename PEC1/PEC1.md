
Minería de datos: PEC1           Ana Valero Giraldez                 Febrero 2025

****
# Ejercicio 1
****

A partir de un juego de datos que te parezca interesante, propón un proyecto completo de minería de datos. La organización de la respuesta tiene que coincidir en las fases típicas del ciclo de vida de un proyecto de minería de datos.  
**No hay que realizar las tareas de la fase, este es un ejercicio teórico**. 

Se espera por lo tanto que se responda de forma argumentada a las siguientes preguntas (Metodología *CRISP-DM*):

1.	**Comprensión del negocio** - ¿Qué necesita el negocio? 
2.	**Comprensión de los datos** - ¿Qué datos tenemos/necesitamos? ¿Están limpios? 
3.	**Preparación de los datos** - ¿Cómo organizamos los datos para el modelado? 
4.	**Modelado** - ¿Qué técnicas de modelado debemos aplicar?
5.	**Evaluación** - ¿Qué modelo cumple mejor con los objetivos del negocio?
6.	**Implementación** - ¿Cómo acceden los interesados a los resultados?

Para cada fase indica cuál es el objetivo de la fase y el producto que se obtendrá. Utiliza ejemplos de qué y cómo podrían ser las tareas. Si hay alguna característica que hace diferente el ciclo de vida de un proyecto de minería respecto a otros proyectos indícalo.

Extensión mínima: 400 palabras


Para este ejercicio vamos a trabajar con datos de una pulsera smartwatch que ha recogido datos de la actividad diaria de varios usuarios.


****
## Definición de la tarea de minería de datos
****

**¿Qué necesita el negocio?**

Este proyecto analiza la relación entre la actividad física y el sueño, determinando si el sedentarismo reduce las horas de descanso o si moverse más favorece un mejor dormir. A través del seguimiento de la actividad diaria y las horas de sueño, se establecerá una correlación entre ambos factores. Esto permitirá comprender el impacto del movimiento en la calidad del descanso.

Los datos se han extraído de la web https://www.kaggle.com/datasets/mohammedarfathr/smartwatch-health-data-uncleaned/data
Estos datos no están limpios, tendremos que tratarlos antes de extraer conclusiones.

****
## Comprensión de los datos
****
**¿Qué datos tenemos/necesitamos? ¿Están limpios?**

Se cargan los datos a estudiar en un nuevo dataframe llamado healthData

```{r}
path = 'unclean_smartwatch_health_data.csv'
healthData <- read.csv(path, row.names=NULL)
```

Realizando el siguiente comando podemos ver qué datos componen el dataset y la cantidad de registros que tenemos.

```{r}
structure = str(healthData)
```

Tenemos **10.000 objetos y 7 variables**.

  - **User.ID**: Identificador único que vincula al usuario con sus datos de actividad y salud en el              smartwatch.
  
  - **Heart.Rate.BPM.**: Mide los latidos por minuto, variando según la actividad: más alto con ejercicio y más bajo en reposo o sueño.
  
  - **Blood.Oxygen.Level**: Indica el porcentaje de oxígeno en la sangre, siendo normal entre 95% y 100%, clave para la función corporal.
  
  - **Step.Count**: Cuenta los pasos dados por el usuario a lo largo del día, ayudando a monitorear la actividad física diaria.
  
  - **Sleep.Duration..hours.**: Mide las horas totales de sueño del usuario, proporcionando datos sobre la cantidad del descanso.
  
  - **Activity.Level**: Clasifica la actividad diaria en sedentario, activo o altamente activo, según el movimiento y ejercicio realizado.
  
  - **Stress.Level**: Evalúa el estrés a través de la variabilidad del ritmo cardíaco, indicando los niveles de tensión física y emocional.
  
  
Vamos a buscar valores nulos o vacíos en las variables.

```{r echo=TRUE, message=FALSE, warning=FALSE}
colSums(is.na(healthData)) #Valores nulos
colSums(healthData=="") #Valores vacíos

```

Obsevamos que hay valores nulos en el dataset y valores vacíos. 
En el analsis de valores nulos podemos ver que tenemos en el **User.ID con 201**, Heart.Rate..BPM. con 400, Blood.Oxygen.Level.... con 300 y **Step.Count con 100**.

Nos interesan los User.ID y Step.Count, aunque el numero de usuarios nulos solo es 2,01% y de pasos 1% lo cual no supondría inconsistencias en los datos ya que no son valores muy elevados.

Estos porcentajes son extraidos de la siguiente manera; Para User.ID = 201 valores nulos/10000 totales * 100 = 2,01% ; Para Step.Count = 100 valores nulos /10000 totales * 100 = 1%

Podemos eliminarlos si queremos con el siguiente comando:

```{r echo=TRUE, message=FALSE, warning=FALSE}

healthDataClear <- na.omit(healthData)

```

En el análisis de valores en blanco, solo hay en los campos **Sleep.Duration..hours. con 150** y Stress.Level con 200, como en el caso anterior el porcentaje es muy pequeño para que suponga una gran inconsistencia.

De todas maneras, como la variable que más nos interesa es las horas de sueño, podemos substituir los valores blancos (y nulos si no los eliminamos), por la mediana.

```{r echo=TRUE, message=FALSE, warning=FALSE}


medianaSleepHours <- median(as.numeric(healthDataClear$Sleep.Duration..hours.), na.rm = TRUE)

print(medianaSleepHours)

healthDataClear$Sleep.Duration..hours.[healthDataClear$Sleep.Duration..hours. == ""] <- medianaSleepHours #Cambiar los valores en blanco por la mediana

healthDataClear$Sleep.Duration..hours.[is.na(healthDataClear$Sleep.Duration..hours.)] <- medianaSleepHours  #Cambiar los valores nulos por la mediana
```

También podemos observar que hay errores de escritura en las variables Activity.Level. Las analizamos y corregimos.

```{r echo=TRUE, message=FALSE, warning=FALSE}
unique(healthDataClear$Activity.Level) #Todos los valores que toma el campo

library(dplyr)

healthDataClear <- healthDataClear %>%
  mutate(Activity.Level = recode(Activity.Level,
                         "Actve" = "Active",
                         "Highly Active" = "Highly_Active",
                         "Seddentary" = "Sedentary"))

unique(healthDataClear$Activity.Level) #Valores después de la correción

```

Podemos verificar también si existen valores duplicados. En la primera comanda nos printa TRUE o FALSE por fila. TRUE = duplicada

En la seguna comanda solo nos muestra las filas duplicadas.

```{r echo=TRUE, message=FALSE, warning=FALSE}

valores_duplicados <- duplicated(healthDataClear)
#print(valores_duplicados) no se muestra por los largos logs

healthDataClear[duplicated(healthDataClear),]

# Eliminar duplicados healthDataClear <- healthDataClear[!duplicated(healthDataClear),]

```

Podemos ver que en este conjunto de datos no existen valores duplicados.

Una vez hemos finalizado este apartado ya tenemos los datos listos limpios para empezar a tratarlos.

****
## Prepración de datos. ¿Como organizamos los datos para el modelado?
****

**Los campos seleccionados para el minado serán los siguientes: User.ID, Step.Count , Sleep.Duration..hours., Activity.Level**

Este apartado consiste en adecuar los datos que vamos a tratar a los objetivos, utilizando métodos de codificación, discretización, normalización y crossing, también se trabajará la reducción de dimensionalidad con tecnicas como PCA, SVC o outliers.

Para empezar vamos a eliminar las variables que no han sido seleccionadas, que son: Heart.Rate..BPM, Blood.Oxygen.Level y  Stress.Level. 

**Codificación campo Activity.Level**

Como podemos ver el campo Activity.Level se presenta como un dato categórico y seria interesante convertir este dato a numérico, siendo la correspondencia: Highly_Active = 5; Active = 3; Sedentary = 0.

```{r echo=TRUE, message=FALSE, warning=FALSE}

healthDataClear$Activity.Level.Numeric = with(healthDataClear, ifelse(Activity.Level %in% c("Highly_Active"), 5,ifelse(Activity.Level %in% c("Active"), 3, 0)))
```

**Discretización campo Step.Count**

De la misma manera, tenemos los Step.Count como datos numéricos, pero nos interesaría una medida más correspondiente para poder hacer una estadística clara. Si tiene < 5.000 = Sedentary = 0;
Si tiene entre 5.000 y 10.000 = Active = 3; Si tiene > 10000 = Highly Active = 5;

Los criterios de rangos están cogidos de artículos de la OMS. 

Realizamos una discretización poniendo los valores de los pasos en rangos, estos rangos se presentarán en valores numéricos simples. 

```{r echo=TRUE, message=FALSE, warning=FALSE}

healthDataClear$Step.Count.Transform = with(healthDataClear, ifelse(Step.Count < 5000, 0, ifelse(Step.Count > 5000 & Step.Count < 10000, 3, 5)))

#healthDataClear["Step.Count.Transform"] <- cut(healthDataClear$Step.Count, breaks = c(0,5000,10000,35000), labels = c(0, 3, 5))

```

El campo Sleep.Duration..hours. está guardado como dato caracteres, pero para tratarlos en futuras estadísticas nos interesa que sean datos numéricos.

```{r echo=TRUE, message=FALSE, warning=FALSE}

healthDataClear$Sleep.Duration..hours. <- as.numeric(healthDataClear$Sleep.Duration..hours.)

summary(healthDataClear[c("Step.Count","Sleep.Duration..hours.","Activity.Level.Numeric")])

```

**Reducción de la dimensionalidad con los outliers**

También vamos a limpiar los datos de los outliers. De esta manera reduciremos la dimensionalidad mediante la reducción de registros que son valores extremos o se desvían de manera significativa. Además, si se aplica un modelado de regresión, como indicamos posteriormente, estos valores límite podrían generar perturbaciones en los resultados.

```{r echo=TRUE, message=FALSE, warning=FALSE}

boxplot(healthDataClear$Sleep.Duration..hours., main = "Boxplot de horas de Sueño", col = "lightblue")

```

Podemos ver que obtenemos algunos datos atípicos, estos se representan como puntos. La mayoría de nuestros datos están en la caja representada en azul. 

**Crossing para crear Activity.total**

Si vamos a trabajar con un modelo de regresión lineal tendremos en cuenta principalmente las variables de nivel de actividad y horas de sueño. 
Para crear la variable de Activity.total, sumaremos los datos convertidos a numéricos de Activity.Level y de Step.Count.

```{r echo=TRUE, message=FALSE, warning=FALSE}

healthDataClear$Activity.total <- healthDataClear$Activity.Level.Numeric + healthDataClear$Step.Count.Transform

```

Finalmente, ya tendremos las variables preparadas para aplicar un modelado.

****
## Modelado ¿Que técnicas de modelado debemos aplicar?
****

Para este conjunto de datos seria interesante utilizar un modelo de regresión para analizar la relación entre la actividad fisica diaria y las horas de sueño.

Por definicion la regresión se utiliza cuando se quiere predecir o entender la relación entre dos o más variables y esto encaja con el modelo de negocio de un smartwatch.

La variable dependiente seria la actividad fisca diaria que incluiría el nivel de actividad y los pasos dados. Y la variable independiente las horas de sueño.

La relación que queremos buscar entre ambas: las personas que duermen más horas son las personas más activas.

Se puede utilizar una regresión lineal simple, buscando unarelacion directa. Si incluimos más datos por ejemplo el estres si que trabajariamos con una regresión lineal múltiple.

```{r echo=TRUE, message=FALSE, warning=FALSE}
#Comando aproximado para obtener una regresión lineal

modelo <- lm(Sleep.Duration..hours. ~ Activity.total, data = healthDataClear)

# Ver el resumen del modelo
summary(modelo)

```


****
## Evaluación ¿Que modelo cumple mejor los objetivos del negocio?
****
Esta fase consiste en ver si el modelo encaja con los datos obtenidos y lo que esperamos como objetivo. Para la evaluación de los datos, necesitaremos aparte del modelo creado, otro para validarlo y otro para evaluarlo. 

Para ello se pueden realizar varias predicciones para ver si los datos que nos proporciona el modelo se clasifica correctamente y que la relación es lineal, el caso de ser exitoso tendremos un modelo de buena calidad.

También verificaremos la proporción de falsos positivos y falsos negativos, en el caso de ser alta tendremos un modelo de baja calidad. 

Estas predicciones consistirían en proporcionar horas de actividad diaria, y ver si las horas de sueño predichas cuadran con los datos establecidos y que consideramos correctos. También esto se podría realizar a la inversa, introducir horas de sueño y que nos prediga cuantas horas de actividad diaria se necesitan para cumplir el descanso.

****
## Implementación ¿Como acceden los interesado a los resultados?
****

Estos resultados pueden ser presentados en el mismo smartwatch o desde la aplicación asociada a este.

****
# Ejercicio 2
****
A partir del juego de datos utilizado en el ejemplo de la PEC, realiza las tareas previas a la generación de un modelo de minería de datos explicadas en los módulos "El proceso de minería de datos" y "Preprocesado de los datos y gestión de características". 

Puedes utilizar de referencia el ejemplo de la PEC, pero procura cambiar el enfoque y analizar los datos en función de las diferentes dimensiones que presentan los datos. Así, no se puede utilizar la combinación de variables utilizada en el ejemplo: "FATALS","DRUNK_DR","VE_TOTAL","VE_FORMS","PVH_INVL","PEDS","PERSONS","PERMVIT","PERNOTMVIT". Se debe analizar cualquier otra combinación que puede incluir (o no) algunas de estas variables con otras nuevas. 

Opcionalmente y valorable se pueden añadir al estudio datos de otros años para realizar comparaciones temporales (https://www.nhtsa.gov/file-downloads?p=nhtsa/downloads/FARS/) o añadir otros hechos a estudiar relacionados, por ejemplo, el consumo de drogas en los accidentes (https://static.nhtsa.gov/nhtsa/downloads/FARS/2020/National/FARS2020NationalCSV.zip)

****
## Descripción del origen del conjunto de datos
****
Se ha seleccionado un conjunto de datos del [National Highway Traffic Safety Administration](https://www.nhtsa.gov/). El sistema de informes de análisis de mortalidad fue creado en los Estados Unidos por la National Highway Traffic Safety Administration para proporcionar una medida global de la seguridad en las carreteras. 

Los datos pertenecen al año 2020. Se trata de un conjunto de registros de accidentes que recogen datos significativos que los describen, conjuntamente con el conjunto de registros de los test de drogas realizados y sus resultados.

El objetivo analítico que tenemos en mente es ver cuantos de estos accidentes mortales están relacionados con la toma de drogas, y en que estado y meses son más propensos.


****
## Análisis exploratorio
****

El primer paso para realizar un análisis exploratorio es cargar el fichero de datos.

```{r}
path = 'accident.CSV'
accidentData <- read.csv(path, row.names=NULL)

# Apartado complementario

path = 'drugs.CSV'
drugsData <- read.csv(path, row.names=NULL)

```

****
### Exploración del conjunto de datos
****

```{r}
structure = str(accidentData)
structure = str(drugsData)

```

Tenemos **35766 registros y 81 variables** en los datos de **accidentes**.

Las variables totales se han comentado en el ejemplo. De este conjunto de datos se utilizaran para el analisis las siguientes variables:

+ **ST_CASE** identificador de accidente
+ **FATAL** muertes
+ **DRUNK_DR** conductores bebidos
+ **STATE** codificación de estado
+ **STATENAME** nombre de estado
+ **MONTH** mes
+ **MONTHNAME** nombre de mes

Tenemos **107141 registros y 9 variables** en los datos de **drogas**. Se utilizaran los siguientes datos:

+ **ST_CASE** identificador de accidente
+ **STATE** codificación de estado
+ **STATENAME** nombre de estado
+ **VEH_NO** numero del vehiculo
+ **PER_NO** numero del persona
+ **DRUGSPEC** valor de si se ha realizado un test de drogas . 1 = Si ; 0= No
+ **DRUGSPECNAME** descripción del tipo de test de drogas
+ **DRUGRES** codificación de la droga tomada.
+ **DRUGRESNAME ** Nombre de la droga o comentario del test de drogas.

****
### Procesamiento de datos y gestión de caracteristicas
****

****
#### Limpieza
****

**Verificación de vacíos o nulos**

Tenemos que limpiar los datos verificando si hay valores vacíos o nulos.

```{r echo=TRUE, message=FALSE, warning=FALSE}
colSums(is.na(accidentData))
colSums(accidentData=="")
```

Verificamos que no hay valores nulos en los datos ni campos con valores en blanco. Encontrarmos en los datos de accidente el campo TWAY_ID2 con 26997 valores en blanco pero no nos interesa.

```{r echo=TRUE, message=FALSE, warning=FALSE}
colSums(is.na(drugsData))
colSums(drugsData=="")
```

Tampoco hay valores nulos o en blanco en los datos de drogas.

En el caso de que hubieramos encontrado datos y que influyeran el campos que vamos a utilizar podemos substituir estos datos por media/mediana/moda en vez de eliminiarlos.

**Verificación de errores tipograficos**

Revisaremos que los datos categoricos que vamos a utilizar no tengan errores de tipográficos

```{r echo=TRUE, message=FALSE, warning=FALSE}
unique(drugsData$STATENAME)

unique(accidentData$STATENAME)
unique(accidentData$MONTHNAME)

```

**Verificación de valores duplicados**

```{r echo=TRUE, message=FALSE, warning=FALSE}

accidentData_duplicados <- duplicated(accidentData)
drugsData_duplicados <- duplicated(drugsData)
#print(accidentData_duplicados) no se muestra por los largos logs
#print(drugsData_duplicados) no se muestra por los largos logs

accidentData[duplicated(accidentData),]
#drugsData[duplicated(drugsData),] no se muestra por los largos logs

```

Al realizar el análisis, podemos ver que hay unos **2,497 registros duplicados**. Estos se pueden eliminar fácilmente del dataset con el siguiente comando. 

```{r echo=TRUE, message=FALSE, warning=FALSE}
# Eliminar duplicados accidentData <- accidentData[!duplicated(accidentData),]
drugsData <- drugsData[!duplicated(drugsData),]

```

****
### Datos en contexto
****

El objetivo de este análisis es ver cuantos de estos accidentes mortales están relacionados con la toma de drogas, y en que estado y meses son más propensos.

Teniendo esto en cuenta y para situarnos en el contexto, vamos a describir los valores en términos generales.Y generar los primeros histogramas.

```{r echo=TRUE, message=FALSE, warning=FALSE}

summary(accidentData[c("FATALS")])

```

Realizando un **resumen de la variable FATALS** podemos comprender que en el conjunto de datos se reporta una muerte como minimo, siendo el accidente más grave con 8 victimas. 
Teniendo esto encuenta, podemos suponer que cada ST_CASE abierto correspondera a un accidente mortal.

Seguidamente, vamos a analizar el número de muertes por estado. 

```{r echo=TRUE, message=FALSE, warning=FALSE}
library(ggplot2)
library(dplyr)

objeto <- 1:56

listname <- character(length(objeto)) 
listfatal <- numeric(length(objeto)) 
listid <- numeric(length(objeto))   

for(i in seq_along(objeto)) {
  elemento <- objeto[i]
  
  # Recogemos los datos que coinciden con el estado
  matching_rows <- accidentData[accidentData$STATE == elemento, ]

  # Recogemos el nombre del estado
  listname[i] <- unique(matching_rows$STATENAME)[1]  
  
  # Recogemos la suma de muertes por estado
  listfatal[i] <- sum(matching_rows$FATALS, na.rm = TRUE)  
  listid[i] <- i
  
}

# Creamos un dataframe accidentDataStateFatals a partir de los datos añadidos en la lista
accidentDataStateFatals <- data.frame(ID_STATE = listid, STATENAME=listname, FATALS = listfatal)

#Eliminamos los datos nulos
accidentDataStateFatals <- na.omit(accidentDataStateFatals)


#Recogemos los datos del dataframe creado accidentDataStateFatals
ggplot(accidentDataStateFatals, aes(x = reorder(STATENAME, FATALS), y = FATALS, fill = STATENAME)) +
  geom_col() +
  coord_flip() + 
  ggtitle("Número de muertes en accidentes por Estado") +
  labs(x = "Estado", y = "Número de muertes") +
  theme_minimal() + 
  theme(legend.position = "none")


max(accidentDataStateFatals$FATALS)

```

Podemos concluir, que los estados más propensos a tener accidentes mortales son Texas, California y Florida. Siendo así los menos propensos District of Columbia, Vermont y Alaska.

**Texas recoge un total de 3874 muertes**


```{r echo=TRUE, message=FALSE, warning=FALSE}
max(accidentDataStateFatals$FATALS)
```

Vamos a indagar dentro de este estado en que meses se han producido más muertes. Generamos el siguiente histograma:

```{r echo=TRUE, message=FALSE, warning=FALSE}
library(ggplot2)
library(dplyr)


estado_seleccionado <- "Texas"  # Cambia esto por el estado que quieras analizar

accidentData_estado <- accidentData %>%
  filter(STATENAME == estado_seleccionado)

ggplot(accidentData_estado, aes(x = factor(MONTH, levels = 1:12), y = FATALS, fill = as.factor(MONTH))) +
  geom_col() +
  ggtitle(paste("Muertes por mes en", estado_seleccionado)) +
  labs(x = "Mes", y = "Número de muertes") +
  theme_minimal() +
  theme(legend.position = "none")


```

Podemos ver que los **meses de octubre y diciembre son los que más presentan accidentes**. Podemos crear la **hipótesis de que estas fechas coinciden con el inicio de las festividades navideñas o con Halloween**, pero no estamos seguros de ello. 

Vamos a crear un histograma para refutar o no nuestra hipótesis.


```{r echo=TRUE, message=FALSE, warning=FALSE}
library(ggplot2)
library(dplyr)

estado_seleccionado <- "Texas"  # Cambia esto por el estado que quieras analizar
mes_seleccionado1 <- "October" 
mes_seleccionado2 <- "December" 

accidentData_estado <- accidentData %>%
  filter(STATENAME == estado_seleccionado & MONTHNAME == mes_seleccionado1)

ggplot(accidentData_estado, aes(x = factor(DAY), y = FATALS, fill = as.factor(DAY))) +
  geom_col() +
  ggtitle(paste("Muertes en", mes_seleccionado1, "en",estado_seleccionado)) +
  labs(x = "Dias", y = "Número de muertes") +
  theme_minimal() +
  theme(legend.position = "none")


accidentData_estado <- accidentData %>%
  filter(STATENAME == estado_seleccionado & MONTHNAME == mes_seleccionado2)

ggplot(accidentData_estado, aes(x = factor(DAY), y = FATALS, fill = as.factor(DAY))) +
  geom_col() +
  ggtitle(paste("Muertes en", mes_seleccionado2, "en",estado_seleccionado)) +
  labs(x = "Dias", y = "Número de muertes") +
  theme_minimal() +
  theme(legend.position = "none")

```

Podemos **refutar nuestra hipótesis en el caso de octubre**, ya que los **días** que se han registrado más accidentes son el **05, 10, 25**.

En caso de Navidades, podemos decir que la operación salida por **Navidades afecta**, ya que el **día 20** es el segundo más accidentado, el primero es el **28**, pudiendo corresponder con la movilización para fin de año.

Seguimos indagando, de esos días festivos en diciembre vamos a ver que accidentes **NO** han sido causados por un **conductor ebrio**.

```{r echo=TRUE, message=FALSE, warning=FALSE}

accidentData_estado <- accidentData %>%
  filter(STATENAME == estado_seleccionado & MONTHNAME == mes_seleccionado1 & DRUNK_DR == 0) %>%
  select(ST_CASE, STATENAME, MONTHNAME, FATALS, DAY, HOUR, DRUNK_DR)

muertes_sobrios <- sum(accidentData_estado$FATALS, na.rm = TRUE)  
print(muertes_sobrios)
```

En el mes de octubre y en Texas, tenemos **303 muertes que no corresponden a accidentes por alcohol**.

****
### Construcción de conjunto de datos final
****
A partir de estos datos analizados nos vamos a centrar en aquellos accidentes que no han sido causados por el alcohol. 
Vamos a **vincular estos datos con los registros de los casos de drogas mediante un inner join**, seleccionando así solos los casos que coinciden con la clave primaria ST_CASE.

```{r echo=TRUE, message=FALSE, warning=FALSE}

accidentDataWithDrugs <- inner_join(accidentData_estado, drugsData, by = "ST_CASE") 

str(accidentDataWithDrugs)

```


****
#### Codificación
****

Como la definición de codificación indica, es el convertir variables clásicas a numéricas. En este caso, para adecuar los datos, hemos convertido el campo **DRUGSPECNAME en un simple valor booleano 0, 1**. Indicando, en el caso de que se haya hecho un test de drogas = 1; y en el caso de que no, un 0.


```{r echo=TRUE, message=FALSE, warning=FALSE}

accidentDataWithDrugs$TESTED <- with(accidentDataWithDrugs, ifelse(DRUGRESNAME %in% c("Test Not Given"), 0, 1))

```

****
#### Normalización 
****
La normalización consiste en ajustar valores numéricos a una escala estándar, y eso es justo lo que necesitamos para tratar el campo DRUGRES.
Este dispone de todos los códigos de drogas registrados, pero solo nos interesa saber si se han encontrado o no estupefacientes. 
Indicaremos un 0 cuando el valor en el registro es 0, es decir, que no se han encontrado drogas o que no se ha realizado ningún test. Si hay otro valor registrado, sabemos que es una codificación del estupefaciente, por lo tanto, valor = 1, indicando que sí se han encontrado drogas.

```{r echo=TRUE, message=FALSE, warning=FALSE}

accidentDataWithDrugs$FINDED <- with(accidentDataWithDrugs, ifelse(DRUGRES %in% c("0", "1"), 0, 1))

```

A partir de los anteriores datos estandarizados podemos realizar los siguientes histogramas:

```{r echo=TRUE, message=FALSE, warning=FALSE}
library(ggplot2)
library(dplyr)

objeto <- 1:31

listname <- character(length(objeto)) 
listfatal <- numeric(length(objeto)) 
listid <- numeric(length(objeto))   

for(i in seq_along(objeto)) {
  elemento <- objeto[i]
  
  # Recogemos los datos que coinciden con el estado
  matching_rows <- accidentDataWithDrugs[accidentDataWithDrugs$DAY == elemento, ]
  
  # Recogemos la suma de muertes por estado
  listfatal[i] <- sum(matching_rows$FINDED[matching_rows$FINDED == 1], na.rm = TRUE)  
  listid[i] <- i
  
}

# Creamos un dataframe accidentDataStateFatals a partir de los datos añadidos en la lista
accidentDataWithDrugsAux <- data.frame(DAY = listid, FINDED = listfatal)

#Eliminamos los datos nulos
accidentDataWithDrugsAux <- na.omit(accidentDataWithDrugsAux)


#Recogemos los datos del dataframe creado accidentDataStateFatals
ggplot(accidentDataWithDrugsAux, aes(x = FINDED , y = factor(DAY, levels = 1:31), fill = FINDED)) +
  geom_col() +
  coord_flip() + 
  ggtitle("Días con drogas encontradas mes Octubre") +
  labs(x = "Test con drogas encontradas", y = "Dias") +
  theme_minimal() + 
  theme(legend.position = "none")

```

Primeramente, en el mes de octubre teníamos como días con más accidentes el 05, 10 y 25. Al presentar esta gráfica, podemos enlazar datos, ya que en los días 10 y 25 ha habido repuntes de tests positivos.

Por curiosidad, vamos a hacer un pequeño hincapié en las horas para ver si la cantidad de tests realizados se enlaza con la cantidad de test positivos.

****
#### Discretización 
****
Como su definición indica, consiste en convertir una variable numérica en intervalos o categorías. En este caso, hemos transformado la variable HOUR en un intervalo leíble para poder analizar. Estos intervalos están separados por franjas horarias, siendo  Madrugada de 0/24 a 6; Mañana de 6 a 11; Mediodía de 11 a 15; Anochecer de 15 a 18 y Noche de 18 a 23.

```{r echo=TRUE, message=FALSE, warning=FALSE}

accidentDataWithDrugs["HOUR_SEG"]<-cut(accidentDataWithDrugs$HOUR, breaks = c(0,6,11,15,17,23), labels = c("Madrugada", "Mañana", "Mediodía", "Anochecer","Noche"))


```

Realizamos los histogramas teniendo en cuenta la transformación de las variables:

```{r echo=TRUE, message=FALSE, warning=FALSE}
library(ggplot2)
library(dplyr)

objeto <- c("Madrugada", "Mañana", "Mediodía", "Anochecer","Noche")

listname <- character(length(objeto)) 
listfatal <- numeric(length(objeto)) 
listid <- numeric(length(objeto))   

for(i in seq_along(objeto)) {
  elemento <- objeto[i]
  # Recogemos los datos que coinciden con el estado
  matching_rows <- accidentDataWithDrugs[accidentDataWithDrugs$HOUR_SEG == elemento, ]

  listname[i] <- elemento
  
  # Recogemos la suma de muertes por estado
  listfatal[i] <- sum(matching_rows$TESTED[matching_rows$TESTED == 1], na.rm = TRUE)  
  listid[i] <- i
  
}

# Creamos un dataframe accidentDataStateFatals a partir de los datos añadidos en la lista
accidentDataWithDrugsAux2 <- data.frame(HOUR_SEG = listname, TESTED = listfatal)

#Eliminamos los datos nulos
accidentDataWithDrugsAux2 <- na.omit(accidentDataWithDrugsAux2)

ggplot(accidentDataWithDrugsAux2, aes(x = TESTED, y = HOUR_SEG, fill = TESTED)) +
  geom_col() +
  coord_flip() + 
  ggtitle("Número de tests de drogas por franjas horarias") +
  labs(x = "Cantidad de test", y = "Franja horaria") +
  theme_minimal() + 
  theme(legend.position = "none")


```

```{r echo=TRUE, message=FALSE, warning=FALSE}
library(ggplot2)
library(dplyr)

objeto <- c("Madrugada", "Mañana", "Mediodía", "Anochecer","Noche")

listname <- character(length(objeto)) 
listfatal <- numeric(length(objeto)) 
listid <- numeric(length(objeto))   

for(i in seq_along(objeto)) {
  elemento <- objeto[i]
  # Recogemos los datos que coinciden con el estado
  matching_rows <- accidentDataWithDrugs[accidentDataWithDrugs$HOUR_SEG == elemento, ]

  listname[i] <- elemento
  
  # Recogemos la suma de muertes por estado
  listfatal[i] <- sum(matching_rows$FINDED[matching_rows$FINDED == 1], na.rm = TRUE)  
  listid[i] <- i
  
}

# Creamos un dataframe accidentDataStateFatals a partir de los datos añadidos en la lista
accidentDataWithDrugsAux2 <- data.frame(HOUR_SEG = listname, FINDED = listfatal)

#Eliminamos los datos nulos
accidentDataWithDrugsAux2 <- na.omit(accidentDataWithDrugsAux2)

ggplot(accidentDataWithDrugsAux2, aes(x = FINDED, y = HOUR_SEG, fill = FINDED)) +
  geom_col() +
  coord_flip() + 
  ggtitle("Número de tests de drogas por franjas horarias") +
  labs(x = "Cantidad de test", y = "Franja horaria") +
  theme_minimal() + 
  theme(legend.position = "none")


```

Con los dos graficos podemos ver que la franja horaria que se han detectado **más test positivos y que se han realizado más tests es por la noche**.


****
#### Proceso PCA 
****

Como la nomenclatura indica, haremos un análisis de componentes principales para reducir la dimensionalidad de los datos; trabajaremos sobre el proceso PCA.


```{r echo=TRUE, message=FALSE, warning=FALSE}

n = c("ST_CASE","STATE","VEH_NO","PER_NO","DRUGSPEC", "DRUGRES")
drugsDataAux= drugsData %>% select(all_of(n))

#pca.acc <- prcomp(drugsDataAux)
pca.acc <- prcomp(drugsDataAux, scale. = TRUE)  # Normaliza antes de PCA

summary(pca.acc)
```

Este método encuentra direcciones de mayor variancia, se puede utilizar sin usar la desviación estándar pero puede distorsionar el análisis de dispersión, especialmente cuando las variables originales tienen escalas muy distintas, como es el caso, y esto llevaría a interpretaciones incorrectas.

Por lo tanto, se ha añadido al comando el valor "scale." para escalar los datos y utilizar esta desviación estándar. 

Los resultados son los siguientes:

```{r}
if (!require('factoextra')) install.packages('factoextra'); library('factoextra')
#Los valores propios corresponden a la cantidad de variación explicada por cada componente principal (PC).
ev= get_eig(pca.acc)
ev
fviz_eig(pca.acc)
```


El gráfico (llamado de codo)  muestra que los primeros componentes principales (PC1 y PC2) explican la mayor parte de la variabilidad en los datos, mientras que los siguientes tienen una contribución menor. 

Aun así, en este análisis se han utilizado componentes con baja varianza, porque contienen información clave sobre variables específicas, como el uso de drogas. 
Aunque PCA suele usarse para reducir dimensiones, y según el gráfico se puede reducir a tres,  en el caso tratado durante este ejercicio se ha priorizado el análisis de datos para sacar conclusiones.
Para mejorar el análisis, se pueden revisar las cargas de cada variable en los PCs o aplicar otras técnicas como clustering o árboles de decisión que también permiten extraer conclusiones más precisas sin perder información relevante.


****
#### Datos correlación 
****

```{R}

objeto <- 1:31

listday <- numeric(length(objeto)) 
listfatal <- numeric(length(objeto))
listdrugs <- numeric(length(objeto)) 

for(i in seq_along(objeto)) {
  elemento <- objeto[i]

  listday[i] <- elemento
  
  listfatal[i] <- sum(accidentDataWithDrugs$FATALS[accidentDataWithDrugs$DAY == elemento & accidentDataWithDrugs$FINDED == 1], na.rm = TRUE)
  
  listdrugs[i] <- sum(accidentDataWithDrugs$FINDED[accidentDataWithDrugs$DAY == elemento & accidentDataWithDrugs$FINDED == 1], na.rm = TRUE) 
  
  
}

accidentDataFatalsDrugs <- data.frame(DAY = listday, FATALS=listfatal, DRUGS = listdrugs)

accidentDataFatalsDrugs <- na.omit(accidentDataFatalsDrugs)



cor.test(x = accidentDataFatalsDrugs$FATALS, y = accidentDataFatalsDrugs$DRUGS, method = "kendall")
ggplot(data = accidentDataFatalsDrugs, aes(x = FATALS, y = log(DRUGS))) + geom_point(color = "gray30") + geom_smooth(color = "firebrick") + theme_bw() +ggtitle("Correlación entre muertes y estupefacientes detectados")

```

Esta gráfica muestra la correlación entre el número de muertes y la cantidad de estupefacientes detectados, con una transformación logarítmica aplicada a la variable DRUGS.

Podemos ver que, a medida que aumentan las muertes, también aumenta la cantidad de drogas detectadas, pero la relación parece curva en lugar de ser completamente lineal. 

Podemos destacar dos componentes en la gráfica: la sombra gris, que indica que un modelo no lineal es más adecuado para describir la relación; la transformación algorítmica sobre la variable DRUGS, que indica que los valores originales posiblemente sean valores un tanto extremos 

En conclusión, existe una fuerte correlación entre muertes y drogas detectadas, pero la relación no es estrictamente lineal y esto indica que, a medida que aumenta el número de muertes, la detección de drogas crece de forma acelerada, pero con cierta saturación en niveles altos.


****
## Implementación de los resultados
****

Los datos estudiados contemplan accidentes de tráfico con víctimas en las redes de autopistas de EE.UU. a lo largo del 2020, con énfasis en la relación entre el consumo de drogas y la mortalidad en los accidentes. Los registros incluyen un identificador único de accidente y variables principales como número de muertos, conductores implicados, tests de drogas realizados y sus resultados, junto con datos temporales y geográficos.  

Los datos se encuentran bien documentados y con un nivel de limpieza aceptable. No se presentan graves problemas de valores nulos o vacíos. Además, la combinación de las bases de datos de accidentes y tests de drogas permite generar nuevas variables relevantes para el estudio.  

Podemos afirmar que, a lo largo del 2020, ocurrieron 35,766 accidentes en los que fallecieron 38,824 personas. Se buscó analizar la relación entre el consumo de drogas y la siniestralidad, y las conclusiones fueron significativas. Se encontró una correlación clara entre los tests de drogas realizados y los resultados positivos, ademas que los analisis realizados sugieren una relación entre el consumo de estupefacientes y la presencia de víctimas en los accidentes.  

Aún así, para establecer una relación directa entre el consumo de drogas por parte del conductor y la mortalidad en el accidente, se requiere un análisis más profundo, posiblemente con modelos más complejos.  

El número más habitual de víctimas mortales por accidente es de una persona. Se ha observado que los conductores ebrios están involucrados en un porcentaje considerable de los accidentes mortales, pero también se detectaron casos de accidentes sin alcohol en los que se encontraron drogas en los test realizados. 

A nivel geográfico, Texas, California y Florida fueron los estados con mayor número de accidentes mortales. Al analizar los datos por meses, se observó un incremento en octubre y diciembre, lo que podría estar relacionado con eventos festivos y mayor movilidad. A nivel horario, se determinó que la franja nocturna es la que tiene más tests de drogas y mayor número de positivos, lo que refuerza la hipótesis de que el consumo de sustancias puede estar vinculado a accidentes ocurridos en este horario.

Además, en los datos, se ha aplicado un análisis de componentes principales (PCA) para reducir la dimensionalidad de los datos, los resultados indicaron que los dos primeros componentes principales explican la mayor parte de la variabilidad en los datos, pero no se ha basado el análisis en estas conclusiones.

Por último, en la gráfica de correlación vemos que la relación entre muertes y estupefacientes detectados no es completamente lineal, mostrando una tendencia de crecimiento acelerado en los casos más graves.

****
# Ejercicio 3
****
A partir del juego de datos utilizado en el ejemplo de la PEC, realiza un análisis exploratorio de datos con el paquete explore() de R y comenta las ventajas e inconvenientes que presenta respecto al análisis realizado en el ejercicio 2.

Puedes utilizar la documentación publicada del paquete explore() en https://github.com/rolkra/explore


```{r echo=TRUE, message=FALSE, warning=FALSE}
if(!require('explore')) install.packages('explore'); library('explore')

explore(accidentData)

```

****
## Análisis Exploratorio del Conjunto de Datos
****

El conjunto de datos analizado con explore() de R contiene 35,766 observaciones y 81 variables, abarca toda la información sobre accidentes y sus factores relacionados presentados en el dataset principal (no se ha realizado un analisis del dataset de drogas). En este analisis automatico se verifica la calidad de los datos y la falta de datos en las variables.

****
### Variables generales
****

La variable **STATE** tiene 51 valores únicos, lo que indica que los datos provienen de todos los estados de EE. UU. Además, **COUNTYNAME** y **CITYNAME** muestran variedad de valores únicos, lo que indica que los accidentes están distribuidos en distintas zonas.


La variable **YEAR** no tiene de variabilidad, ya que todos los registros pertenecen al año 2020. En cambio, las variables **DAY, MONTH, HOUR** ayudan a analizar patrones temporales.

La variable **FATALS** tiene un valor máximo de 8, lo que sugiere que el accidente más letal en la base de datos registró esa cantidad de víctimas.

**DRUNK_DR** muestra una media de 0.27, lo que indica que una pequeña proporción de los accidentes involucran conductores ebrios, aunque no es un factor en el que se centra el análisis.

****
### Datos Inusuales
****

La variable **HOUR** tiene valores de 99, que obviamente son erróneos, ya que la hora máxima válida debería ser 23. Esto podría representar que los datos codificados son incorrectos o hay registros erróneos.

El análisis exploratorio general muestra al detalle el conjunto de datos, mostrando una gráfica para cada variable analizada. Podemos decir, grosso modo, que los datos están limpios pero con ciertas anomalías en variables de tiempo y geolocalización.
Existen datos interesantes como la distribución horaria y en la severidad de los accidentes, además de la posible influencia del alcohol y posteriormente del consumo de drogas. 

****
## Ventajas y desventajas de explore()
****

El paquete explore() presenta estadísticas básicas y un desglose del tipo de variables, la cantidad de valores únicos, mínimos, máximos y medias, además de verificar valores faltantes. 

El uso de esta librería ofrece varias ventajas, ya que automatiza los resúmenes estadísticos y la detección de valores atípicos, y proporciona una visión general rápida sin la necesidad de escribir código previo.
Además, permite manejar grandes volúmenes de datos, ya que facilita la identificación de patrones antes de un análisis más profundo. 

Sin embargo, como desventaja,  el usuario tiene menos control sobre la personalización del análisis y la selección de las métricas. Si se trabaja de manera manual, se pueden diseñar visualizaciones y métricas más adaptadas a los objetivos del estudio, mientras que explore() se generan resultados generales que pueden no ser suficientes para el caso. 

En conclusión, es útil para un primer vistazo general de las métricas, pero no reemplaza al análisis manual en profundidad.

