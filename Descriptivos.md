
# ESTADISTICA BASICA

Dentro del análisis exploratorio un aspecto importante es realizar un buen análisis descriptivo de los datos, sin lo cual, puede llegar a perderse el tiempo. Veamos cómo podemos realizar un análisis descriptivo básico. Usaremos las bases de datos que suministra el programa y que son usadas como ejemplo en toda la literatura asociada a *R*.

## Estadistica Descriptiva

En primer lugar, usaremos el ```data.frame``` llamado ```mtcars```


```R
head(mtcars)
```


<table>
<thead><tr><th></th><th scope=col>mpg</th><th scope=col>cyl</th><th scope=col>disp</th><th scope=col>hp</th><th scope=col>drat</th><th scope=col>wt</th><th scope=col>qsec</th><th scope=col>vs</th><th scope=col>am</th><th scope=col>gear</th><th scope=col>carb</th></tr></thead>
<tbody>
	<tr><th scope=row>Mazda RX4</th><td>21.0 </td><td>6    </td><td>160  </td><td>110  </td><td>3.90 </td><td>2.620</td><td>16.46</td><td>0    </td><td>1    </td><td>4    </td><td>4    </td></tr>
	<tr><th scope=row>Mazda RX4 Wag</th><td>21.0 </td><td>6    </td><td>160  </td><td>110  </td><td>3.90 </td><td>2.875</td><td>17.02</td><td>0    </td><td>1    </td><td>4    </td><td>4    </td></tr>
	<tr><th scope=row>Datsun 710</th><td>22.8 </td><td>4    </td><td>108  </td><td> 93  </td><td>3.85 </td><td>2.320</td><td>18.61</td><td>1    </td><td>1    </td><td>4    </td><td>1    </td></tr>
	<tr><th scope=row>Hornet 4 Drive</th><td>21.4 </td><td>6    </td><td>258  </td><td>110  </td><td>3.08 </td><td>3.215</td><td>19.44</td><td>1    </td><td>0    </td><td>3    </td><td>1    </td></tr>
	<tr><th scope=row>Hornet Sportabout</th><td>18.7 </td><td>8    </td><td>360  </td><td>175  </td><td>3.15 </td><td>3.440</td><td>17.02</td><td>0    </td><td>0    </td><td>3    </td><td>2    </td></tr>
	<tr><th scope=row>Valiant</th><td>18.1 </td><td>6    </td><td>225  </td><td>105  </td><td>2.76 </td><td>3.460</td><td>20.22</td><td>1    </td><td>0    </td><td>3    </td><td>1    </td></tr>
</tbody>
</table>



Delimitamos las variables construyendo un vector de caracteres que contiene el nombre de las variables a emplear


```R
myvars<-c("mpg","hp","wt")
head(mtcars[myvars])
```


<table>
<thead><tr><th></th><th scope=col>mpg</th><th scope=col>hp</th><th scope=col>wt</th></tr></thead>
<tbody>
	<tr><th scope=row>Mazda RX4</th><td>21.0 </td><td>110  </td><td>2.620</td></tr>
	<tr><th scope=row>Mazda RX4 Wag</th><td>21.0 </td><td>110  </td><td>2.875</td></tr>
	<tr><th scope=row>Datsun 710</th><td>22.8 </td><td> 93  </td><td>2.320</td></tr>
	<tr><th scope=row>Hornet 4 Drive</th><td>21.4 </td><td>110  </td><td>3.215</td></tr>
	<tr><th scope=row>Hornet Sportabout</th><td>18.7 </td><td>175  </td><td>3.440</td></tr>
	<tr><th scope=row>Valiant</th><td>18.1 </td><td>105  </td><td>3.460</td></tr>
</tbody>
</table>



Existe multiplicidad de métodos, las opciones más básicas son:

* ```summary()```
* ```apply()```
* ```sapply()```

Usemos la opcion ```summary()```


```R
summary(mtcars[myvars])
```


          mpg              hp              wt       
     Min.   :10.40   Min.   : 52.0   Min.   :1.513  
     1st Qu.:15.43   1st Qu.: 96.5   1st Qu.:2.581  
     Median :19.20   Median :123.0   Median :3.325  
     Mean   :20.09   Mean   :146.7   Mean   :3.217  
     3rd Qu.:22.80   3rd Qu.:180.0   3rd Qu.:3.610  
     Max.   :33.90   Max.   :335.0   Max.   :5.424  


Vamos a crear una manera distinta de visualizar los resultados; con la opción ```function{}``` podemos crear nuestra propia forma de visualizar y, además, seleccionar los estadísticos a mostrar:


```R
mystats <- function(x, na.omit=FALSE){
  if (na.omit)
    x <- x[!is.na(x)]
  m <- mean(x)
  n <- length(x)
  s <- sd(x)
  skew <- sum((x-m)^3/s^3)/n
  kurt <- sum((x-m)^4/s^4)/n - 3
  return(c(tamaño=n, media=m, "desviación estándar"=s, simetría=skew, kurtosis=kurt))
}
```

Usando el comando ```sapply()``` podemos aplicar una función a todas las variables del _data frame_


```R
sapply(mtcars[myvars], mystats)
```


<table>
<thead><tr><th></th><th scope=col>mpg</th><th scope=col>hp</th><th scope=col>wt</th></tr></thead>
<tbody>
	<tr><th scope=row>tamaño</th><td>32.000000  </td><td> 32.0000000</td><td>32.00000000</td></tr>
	<tr><th scope=row>media</th><td>20.090625  </td><td>146.6875000</td><td> 3.21725000</td></tr>
	<tr><th scope=row>desviación estándar</th><td> 6.026948  </td><td> 68.5628685</td><td> 0.97845744</td></tr>
	<tr><th scope=row>simetría</th><td> 0.610655  </td><td>  0.7260237</td><td> 0.42314646</td></tr>
	<tr><th scope=row>kurtosis</th><td>-0.372766  </td><td> -0.1355511</td><td>-0.02271075</td></tr>
</tbody>
</table>



Con el comando ```round()``` es posible mejorar la presentación:


```R
round(sapply(mtcars[myvars], mystats),3)
```


<table>
<thead><tr><th></th><th scope=col>mpg</th><th scope=col>hp</th><th scope=col>wt</th></tr></thead>
<tbody>
	<tr><th scope=row>tamaño</th><td>32.000 </td><td> 32.000</td><td>32.000 </td></tr>
	<tr><th scope=row>media</th><td>20.091 </td><td>146.688</td><td> 3.217 </td></tr>
	<tr><th scope=row>desviación estándar</th><td> 6.027 </td><td> 68.563</td><td> 0.978 </td></tr>
	<tr><th scope=row>simetría</th><td> 0.611 </td><td>  0.726</td><td> 0.423 </td></tr>
	<tr><th scope=row>kurtosis</th><td>-0.373 </td><td> -0.136</td><td>-0.023 </td></tr>
</tbody>
</table>



Veamos otra opción para lo cual requerimos la librería ```Hmisc``` (recordemos, si no está instalada procedemos a instalarla)


```R
library(Hmisc)
myvars <- c("mpg", "hp", "wt")
describe(mtcars[myvars])
```


    mtcars[myvars] 
    
     3  Variables      32  Observations
    --------------------------------------------------------------------------------
    mpg 
           n  missing distinct     Info     Mean      Gmd      .05      .10 
          32        0       25    0.999    20.09    6.796    12.00    14.34 
         .25      .50      .75      .90      .95 
       15.43    19.20    22.80    30.09    31.30 
    
    lowest : 10.4 13.3 14.3 14.7 15.0, highest: 26.0 27.3 30.4 32.4 33.9
    --------------------------------------------------------------------------------
    hp 
           n  missing distinct     Info     Mean      Gmd      .05      .10 
          32        0       22    0.997    146.7    77.04    63.65    66.00 
         .25      .50      .75      .90      .95 
       96.50   123.00   180.00   243.50   253.55 
    
    lowest :  52  62  65  66  91, highest: 215 230 245 264 335
    --------------------------------------------------------------------------------
    wt 
           n  missing distinct     Info     Mean      Gmd      .05      .10 
          32        0       29    0.999    3.217    1.089    1.736    1.956 
         .25      .50      .75      .90      .95 
       2.581    3.325    3.610    4.048    5.293 
    
    lowest : 1.513 1.615 1.835 1.935 2.140, highest: 3.845 4.070 5.250 5.345 5.424
    --------------------------------------------------------------------------------


Y más opciones de descriptivos:


```R
library(pastecs)
stat.desc(mtcars[myvars])
```


<table>
<thead><tr><th></th><th scope=col>mpg</th><th scope=col>hp</th><th scope=col>wt</th></tr></thead>
<tbody>
	<tr><th scope=row>nbr.val</th><td> 32.0000000 </td><td>  32.0000000</td><td> 32.0000000 </td></tr>
	<tr><th scope=row>nbr.null</th><td>  0.0000000 </td><td>   0.0000000</td><td>  0.0000000 </td></tr>
	<tr><th scope=row>nbr.na</th><td>  0.0000000 </td><td>   0.0000000</td><td>  0.0000000 </td></tr>
	<tr><th scope=row>min</th><td> 10.4000000 </td><td>  52.0000000</td><td>  1.5130000 </td></tr>
	<tr><th scope=row>max</th><td> 33.9000000 </td><td> 335.0000000</td><td>  5.4240000 </td></tr>
	<tr><th scope=row>range</th><td> 23.5000000 </td><td> 283.0000000</td><td>  3.9110000 </td></tr>
	<tr><th scope=row>sum</th><td>642.9000000 </td><td>4694.0000000</td><td>102.9520000 </td></tr>
	<tr><th scope=row>median</th><td> 19.2000000 </td><td> 123.0000000</td><td>  3.3250000 </td></tr>
	<tr><th scope=row>mean</th><td> 20.0906250 </td><td> 146.6875000</td><td>  3.2172500 </td></tr>
	<tr><th scope=row>SE.mean</th><td>  1.0654240 </td><td>  12.1203173</td><td>  0.1729685 </td></tr>
	<tr><th scope=row>CI.mean.0.95</th><td>  2.1729465 </td><td>  24.7195501</td><td>  0.3527715 </td></tr>
	<tr><th scope=row>var</th><td> 36.3241028 </td><td>4700.8669355</td><td>  0.9573790 </td></tr>
	<tr><th scope=row>std.dev</th><td>  6.0269481 </td><td>  68.5628685</td><td>  0.9784574 </td></tr>
	<tr><th scope=row>coef.var</th><td>  0.2999881 </td><td>   0.4674077</td><td>  0.3041285 </td></tr>
</tbody>
</table>




```R
library(psych)
describe(mtcars[myvars])
```


<table>
<thead><tr><th></th><th scope=col>vars</th><th scope=col>n</th><th scope=col>mean</th><th scope=col>sd</th><th scope=col>median</th><th scope=col>trimmed</th><th scope=col>mad</th><th scope=col>min</th><th scope=col>max</th><th scope=col>range</th><th scope=col>skew</th><th scope=col>kurtosis</th><th scope=col>se</th></tr></thead>
<tbody>
	<tr><th scope=row>mpg</th><td>1          </td><td>32         </td><td> 20.09062  </td><td> 6.0269481 </td><td> 19.200    </td><td> 19.696154 </td><td> 5.4114900 </td><td>10.400     </td><td> 33.900    </td><td> 23.500    </td><td>0.6106550  </td><td>-0.37276603</td><td> 1.0654240 </td></tr>
	<tr><th scope=row>hp</th><td>2          </td><td>32         </td><td>146.68750  </td><td>68.5628685 </td><td>123.000    </td><td>141.192308 </td><td>77.0952000 </td><td>52.000     </td><td>335.000    </td><td>283.000    </td><td>0.7260237  </td><td>-0.13555112</td><td>12.1203173 </td></tr>
	<tr><th scope=row>wt</th><td>3          </td><td>32         </td><td>  3.21725  </td><td> 0.9784574 </td><td>  3.325    </td><td>  3.152692 </td><td> 0.7672455 </td><td> 1.513     </td><td>  5.424    </td><td>  3.911    </td><td>0.4231465  </td><td>-0.02271075</td><td> 0.1729685 </td></tr>
</tbody>
</table>



También podemos generar cálculos por grupos y comparar; por ejemplo, usemos el tipo de transmisión para comparar la media:


```R
aggregate(mtcars[myvars], by=list(am=mtcars$am), mean)
```


<table>
<thead><tr><th scope=col>am</th><th scope=col>mpg</th><th scope=col>hp</th><th scope=col>wt</th></tr></thead>
<tbody>
	<tr><td>0       </td><td>17.14737</td><td>160.2632</td><td>3.768895</td></tr>
	<tr><td>1       </td><td>24.39231</td><td>126.8462</td><td>2.411000</td></tr>
</tbody>
</table>



Podemos usar la opción ```by()``` para generar otra visualización de resultados:


```R
dstats <- function(x)sapply(x, mystats)
myvars <- c("mpg", "hp", "wt")
by(mtcars[myvars], mtcars$am, dstats)
```


    mtcars$am: 0
                                mpg           hp         wt
    tamaño              19.00000000  19.00000000 19.0000000
    media               17.14736842 160.26315789  3.7688947
    desviación estándar  3.83396639  53.90819573  0.7774001
    simetría             0.01395038  -0.01422519  0.9759294
    kurtosis            -0.80317826  -1.20969733  0.1415676
    ------------------------------------------------------------ 
    mtcars$am: 1
                                mpg          hp         wt
    tamaño              13.00000000  13.0000000 13.0000000
    media               24.39230769 126.8461538  2.4110000
    desviación estándar  6.16650381  84.0623243  0.6169816
    simetría             0.05256118   1.3598859  0.2103128
    kurtosis            -1.45535200   0.5634635 -1.1737358


## Frecuencias  y Tablas de Contingencia

Las Tablas de Contingencia nos sirven para analizar frecuencias. En **R** podemos encontrar varias formas que nos permiten construirlas. Vamos a mirar los métodos más básicos.

Para el ejercicio, emplearemos algunos data frame propios del programa y empleados con frecuencia en la literatura. Usaremos el conjunto de datos llamado ```Arthritis```. Para conocer más detalles sobre los datos podemos usar ```help(Arthritis)```


```R
library(vcd)
head(Arthritis)
```


<table>
<thead><tr><th scope=col>ID</th><th scope=col>Treatment</th><th scope=col>Sex</th><th scope=col>Age</th><th scope=col>Improved</th></tr></thead>
<tbody>
	<tr><td>57     </td><td>Treated</td><td>Male   </td><td>27     </td><td>Some   </td></tr>
	<tr><td>46     </td><td>Treated</td><td>Male   </td><td>29     </td><td>None   </td></tr>
	<tr><td>77     </td><td>Treated</td><td>Male   </td><td>30     </td><td>None   </td></tr>
	<tr><td>17     </td><td>Treated</td><td>Male   </td><td>32     </td><td>Marked </td></tr>
	<tr><td>36     </td><td>Treated</td><td>Male   </td><td>46     </td><td>Marked </td></tr>
	<tr><td>23     </td><td>Treated</td><td>Male   </td><td>58     </td><td>Marked </td></tr>
</tbody>
</table>



Para empezar, usaremos la opción ```table()```.


```R
mytable <- with(Arthritis, table(Improved))
mytable
```


    Improved
      None   Some Marked 
        42     14     28 


Como vemos, se trata de una tabla de frecuencias absolutas. Con la opción ```prop.table()``` podemos construir una tabla de frecuencias relativas:


```R
prop.table(mytable)
```


    Improved
         None      Some    Marked 
    0.5000000 0.1666667 0.3333333 


O multiplicarla por $100$ si se desea que el resultado esté en porcentajes: 


```R
prop.table(mytable)*100
```


    Improved
        None     Some   Marked 
    50.00000 16.66667 33.33333 


Podemos, también, construir una tabla de contingencias para dos variables:


```R
mytable <- xtabs(~ Treatment+Improved, data=Arthritis)
mytable
```


             Improved
    Treatment None Some Marked
      Placebo   29    7      7
      Treated   13    7     21


Calcular los valores marginales


```R
margin.table(mytable, 1)
```


    Treatment
    Placebo Treated 
         43      41 


O sus proporciones


```R
prop.table(mytable, 1)
```


             Improved
    Treatment      None      Some    Marked
      Placebo 0.6744186 0.1627907 0.1627907
      Treated 0.3170732 0.1707317 0.5121951


Observemos que, en ambos casos, el argumento ```(...,1)``` indica que la operación debe hacerse usando sólo la variable 1; que en el caso del ejemplo corresponde a los Tratamientos. Si queremos calcular los valores marginales para la variable de mejoras, entonces cambiamos ```1``` por ```2```.

Si lo deseamos, podemos añadir los valores marginales a la tabla (tanto para frecuencias absolutas como relativas):


```R
addmargins(mytable)
```


<table>
<thead><tr><th></th><th scope=col>None</th><th scope=col>Some</th><th scope=col>Marked</th><th scope=col>Sum</th></tr></thead>
<tbody>
	<tr><th scope=row>Placebo</th><td>29</td><td> 7</td><td> 7</td><td>43</td></tr>
	<tr><th scope=row>Treated</th><td>13</td><td> 7</td><td>21</td><td>41</td></tr>
	<tr><th scope=row>Sum</th><td>42</td><td>14</td><td>28</td><td>84</td></tr>
</tbody>
</table>




```R
addmargins(prop.table(mytable))
```


<table>
<thead><tr><th></th><th scope=col>None</th><th scope=col>Some</th><th scope=col>Marked</th><th scope=col>Sum</th></tr></thead>
<tbody>
	<tr><th scope=row>Placebo</th><td>0.3452381 </td><td>0.08333333</td><td>0.08333333</td><td>0.5119048 </td></tr>
	<tr><th scope=row>Treated</th><td>0.1547619 </td><td>0.08333333</td><td>0.25000000</td><td>0.4880952 </td></tr>
	<tr><th scope=row>Sum</th><td>0.5000000 </td><td>0.16666667</td><td>0.33333333</td><td>1.0000000 </td></tr>
</tbody>
</table>



O limitar los valores marginales a una sola variable:


```R
addmargins(prop.table(mytable, 1), 2)
```


<table>
<thead><tr><th></th><th scope=col>None</th><th scope=col>Some</th><th scope=col>Marked</th><th scope=col>Sum</th></tr></thead>
<tbody>
	<tr><th scope=row>Placebo</th><td>0.6744186</td><td>0.1627907</td><td>0.1627907</td><td>1        </td></tr>
	<tr><th scope=row>Treated</th><td>0.3170732</td><td>0.1707317</td><td>0.5121951</td><td>1        </td></tr>
</tbody>
</table>



Con la opción ```CrossTable``` de la libraría ```gmodels``` podemos construir una tabla de valores cruzados más completa:


```R
library(gmodels)
CrossTable(Arthritis$Treatment, Arthritis$Improved)
```

    
     
       Cell Contents
    |-------------------------|
    |                       N |
    | Chi-square contribution |
    |           N / Row Total |
    |           N / Col Total |
    |         N / Table Total |
    |-------------------------|
    
     
    Total Observations in Table:  84 
    
     
                        | Arthritis$Improved 
    Arthritis$Treatment |      None |      Some |    Marked | Row Total | 
    --------------------|-----------|-----------|-----------|-----------|
                Placebo |        29 |         7 |         7 |        43 | 
                        |     2.616 |     0.004 |     3.752 |           | 
                        |     0.674 |     0.163 |     0.163 |     0.512 | 
                        |     0.690 |     0.500 |     0.250 |           | 
                        |     0.345 |     0.083 |     0.083 |           | 
    --------------------|-----------|-----------|-----------|-----------|
                Treated |        13 |         7 |        21 |        41 | 
                        |     2.744 |     0.004 |     3.935 |           | 
                        |     0.317 |     0.171 |     0.512 |     0.488 | 
                        |     0.310 |     0.500 |     0.750 |           | 
                        |     0.155 |     0.083 |     0.250 |           | 
    --------------------|-----------|-----------|-----------|-----------|
           Column Total |        42 |        14 |        28 |        84 | 
                        |     0.500 |     0.167 |     0.333 |           | 
    --------------------|-----------|-----------|-----------|-----------|
    
     
    

Veamos, ahora, la opción para tres variables:


```R
mytable <- xtabs(~ Treatment+Improved+Sex, data=Arthritis)
mytable
```


    , , Sex = Female
    
             Improved
    Treatment None Some Marked
      Placebo   19    7      6
      Treated    6    5     16
    
    , , Sex = Male
    
             Improved
    Treatment None Some Marked
      Placebo   10    0      1
      Treated    7    2      5
    


La anterior presentación no es fácil de leer para el usuario nuevo, entonces, es posible construir una tabla de frecuencias de más fácil lectura:


```R
ftable(mytable)
```


                       Sex Female Male
    Treatment Improved                
    Placebo   None             19   10
              Some              7    0
              Marked            6    1
    Treated   None              6    7
              Some              5    2
              Marked           16    5


O calcular los valores marginales de la tabla:


```R
margin.table(mytable, 1);margin.table(mytable, 2);margin.table(mytable, 3)
```


    Treatment
    Placebo Treated 
         43      41 



    Improved
      None   Some Marked 
        42     14     28 



    Sex
    Female   Male 
        59     25 


El proceso para dos variables también es aplicable a tres variables:


```R
ftable(prop.table(mytable, c(1, 2)))
```


                       Sex    Female      Male
    Treatment Improved                        
    Placebo   None         0.6551724 0.3448276
              Some         1.0000000 0.0000000
              Marked       0.8571429 0.1428571
    Treated   None         0.4615385 0.5384615
              Some         0.7142857 0.2857143
              Marked       0.7619048 0.2380952



```R
ftable(addmargins(prop.table(mytable, c(1, 2)), 3))
```


                       Sex    Female      Male       Sum
    Treatment Improved                                  
    Placebo   None         0.6551724 0.3448276 1.0000000
              Some         1.0000000 0.0000000 1.0000000
              Marked       0.8571429 0.1428571 1.0000000
    Treated   None         0.4615385 0.5384615 1.0000000
              Some         0.7142857 0.2857143 1.0000000
              Marked       0.7619048 0.2380952 1.0000000



```R
ftable(addmargins(prop.table(mytable, c(1, 2)), 3)) * 100
```


                       Sex    Female      Male       Sum
    Treatment Improved                                  
    Placebo   None          65.51724  34.48276 100.00000
              Some         100.00000   0.00000 100.00000
              Marked        85.71429  14.28571 100.00000
    Treated   None          46.15385  53.84615 100.00000
              Some          71.42857  28.57143 100.00000
              Marked        76.19048  23.80952 100.00000


### Ejercicio
_Implementemos el comando ```round()``` para construir de nuevo las tablas_

## Pruebas de Independencia

Las pruebas de independencia nos permiten determinar si una variable categorica de respuesta está relacionada o no con una variable categorica. En el ejemplo de ```Arthritis``` se desea averiguar si las mejoras observadas dependen del tipo de tratamiento implementado.


```R
library(vcd)
mytable <- xtabs(~Treatment+Improved, data=Arthritis)
mytable
```


             Improved
    Treatment None Some Marked
      Placebo   29    7      7
      Treated   13    7     21


A las frecuencias observadas en la tabla le aplicamos una prueba de $\chi^{2}$ para determinar la independencia. Esto sugiere que la hipótesis a probar es: $H_{0}: \text{son independientes}$


```R
chisq.test(mytable)
```


    
    	Pearson's Chi-squared test
    
    data:  mytable
    X-squared = 13.055, df = 2, p-value = 0.001463
    


Ahora, supongamos que deseamos realizar la misma prueba considerando la variable ```sex```


```R
mytable <- xtabs(~Improved+Sex, data=Arthritis)
chisq.test(mytable)
```

    Warning message in chisq.test(mytable):
    "Chi-squared approximation may be incorrect"


    
    	Pearson's Chi-squared test
    
    data:  mytable
    X-squared = 4.8407, df = 2, p-value = 0.08889
    


Aquí observamos una respuesta bastante particular: ```"Chi-squared approximation may be incorrect"```. En realidad, no se trata de un error, lo que ocurre es que algunos de los valores esperados podrían ser muy pequeños por lo que la aproximación de los p-valor mediante chi cuadrado podría no ser la correcta. En este caso, es recomendable hacer vel cálculo usando el test de valores exactos de Fisher:


```R
mytable <- xtabs(~Treatment+Sex, data=Arthritis)
fisher.test(mytable)
```


    
    	Fisher's Exact Test for Count Data
    
    data:  mytable
    p-value = 0.4763
    alternative hypothesis: true odds ratio is not equal to 1
    95 percent confidence interval:
     0.5320442 4.3286798
    sample estimates:
    odds ratio 
      1.500984 
    


Si se desea probar más de dos categorías se emplea el test de ```Cochran-Mantel-Haenszel```


```R
mytable <- xtabs(~Treatment+Improved+Sex, data=Arthritis)
mantelhaen.test(mytable)
```


    
    	Cochran-Mantel-Haenszel test
    
    data:  mytable
    Cochran-Mantel-Haenszel M^2 = 14.632, df = 2, p-value = 0.0006647
    


## Correlaciones

El coeficiente correlación nos permite identificar el grado de asociación lineal entre dos variables. Vamos a tomar una de las bases de datos del programa para calcular y probar algunas correlaciones. En primer lugar, calcularemos la covarianza con la opción ```cov```


```R
states<- state.x77[,1:6]
cov(states)
```


<table>
<thead><tr><th></th><th scope=col>Population</th><th scope=col>Income</th><th scope=col>Illiteracy</th><th scope=col>Life Exp</th><th scope=col>Murder</th><th scope=col>HS Grad</th></tr></thead>
<tbody>
	<tr><th scope=row>Population</th><td>19931683.7588</td><td>571229.7796  </td><td> 292.8679592 </td><td>-407.8424612 </td><td>5663.523714  </td><td>-3551.509551 </td></tr>
	<tr><th scope=row>Income</th><td>  571229.7796</td><td>377573.3061  </td><td>-163.7020408 </td><td> 280.6631837 </td><td>-521.894286  </td><td> 3076.768980 </td></tr>
	<tr><th scope=row>Illiteracy</th><td>     292.8680</td><td>  -163.7020  </td><td>   0.3715306 </td><td>  -0.4815122 </td><td>   1.581776  </td><td>   -3.235469 </td></tr>
	<tr><th scope=row>Life Exp</th><td>    -407.8425</td><td>   280.6632  </td><td>  -0.4815122 </td><td>   1.8020204 </td><td>  -3.869480  </td><td>    6.312685 </td></tr>
	<tr><th scope=row>Murder</th><td>    5663.5237</td><td>  -521.8943  </td><td>   1.5817755 </td><td>  -3.8694804 </td><td>  13.627465  </td><td>  -14.549616 </td></tr>
	<tr><th scope=row>HS Grad</th><td>   -3551.5096</td><td>  3076.7690  </td><td>  -3.2354694 </td><td>   6.3126849 </td><td> -14.549616  </td><td>   65.237894 </td></tr>
</tbody>
</table>



Hay varias formas de calcular las correlaciones, por defecto, el programa calcula el coeficiente de correlación de Pearson:


```R
cor(states)
```


<table>
<thead><tr><th></th><th scope=col>Population</th><th scope=col>Income</th><th scope=col>Illiteracy</th><th scope=col>Life Exp</th><th scope=col>Murder</th><th scope=col>HS Grad</th></tr></thead>
<tbody>
	<tr><th scope=row>Population</th><td> 1.00000000</td><td> 0.2082276 </td><td> 0.1076224 </td><td>-0.06805195</td><td> 0.3436428 </td><td>-0.09848975</td></tr>
	<tr><th scope=row>Income</th><td> 0.20822756</td><td> 1.0000000 </td><td>-0.4370752 </td><td> 0.34025534</td><td>-0.2300776 </td><td> 0.61993232</td></tr>
	<tr><th scope=row>Illiteracy</th><td> 0.10762237</td><td>-0.4370752 </td><td> 1.0000000 </td><td>-0.58847793</td><td> 0.7029752 </td><td>-0.65718861</td></tr>
	<tr><th scope=row>Life Exp</th><td>-0.06805195</td><td> 0.3402553 </td><td>-0.5884779 </td><td> 1.00000000</td><td>-0.7808458 </td><td> 0.58221620</td></tr>
	<tr><th scope=row>Murder</th><td> 0.34364275</td><td>-0.2300776 </td><td> 0.7029752 </td><td>-0.78084575</td><td> 1.0000000 </td><td>-0.48797102</td></tr>
	<tr><th scope=row>HS Grad</th><td>-0.09848975</td><td> 0.6199323 </td><td>-0.6571886 </td><td> 0.58221620</td><td>-0.4879710 </td><td> 1.00000000</td></tr>
</tbody>
</table>



pero se puede solicitar el cálculo de otro coeficiente, por ejemplo, Spearman:


```R
cor(states, method="spearman")
```


<table>
<thead><tr><th></th><th scope=col>Population</th><th scope=col>Income</th><th scope=col>Illiteracy</th><th scope=col>Life Exp</th><th scope=col>Murder</th><th scope=col>HS Grad</th></tr></thead>
<tbody>
	<tr><th scope=row>Population</th><td> 1.0000000</td><td> 0.1246098</td><td> 0.3130496</td><td>-0.1040171</td><td> 0.3457401</td><td>-0.3833649</td></tr>
	<tr><th scope=row>Income</th><td> 0.1246098</td><td> 1.0000000</td><td>-0.3145948</td><td> 0.3241050</td><td>-0.2174623</td><td> 0.5104809</td></tr>
	<tr><th scope=row>Illiteracy</th><td> 0.3130496</td><td>-0.3145948</td><td> 1.0000000</td><td>-0.5553735</td><td> 0.6723592</td><td>-0.6545396</td></tr>
	<tr><th scope=row>Life Exp</th><td>-0.1040171</td><td> 0.3241050</td><td>-0.5553735</td><td> 1.0000000</td><td>-0.7802406</td><td> 0.5239410</td></tr>
	<tr><th scope=row>Murder</th><td> 0.3457401</td><td>-0.2174623</td><td> 0.6723592</td><td>-0.7802406</td><td> 1.0000000</td><td>-0.4367330</td></tr>
	<tr><th scope=row>HS Grad</th><td>-0.3833649</td><td> 0.5104809</td><td>-0.6545396</td><td> 0.5239410</td><td>-0.4367330</td><td> 1.0000000</td></tr>
</tbody>
</table>



Con la opción ```args``` es posible detallar los métodos empleados por el programa para calcular los coeficientes de correlación:


```R
args(cor)
```


<pre class=language-r><code>function (x, y = NULL, use = "everything", method = c("pearson", 
<span style=white-space:pre-wrap>    "kendall", "spearman")) </span>
NULL</code></pre>


También, se puede calcular la correlación para parejas específicas de variables:


```R
x <- states[,c("Population", "Income", "Illiteracy", "HS Grad")]
y <- states[,c("Life Exp", "Murder")]
cor(x,y)
```


<table>
<thead><tr><th></th><th scope=col>Life Exp</th><th scope=col>Murder</th></tr></thead>
<tbody>
	<tr><th scope=row>Population</th><td>-0.06805195</td><td> 0.3436428 </td></tr>
	<tr><th scope=row>Income</th><td> 0.34025534</td><td>-0.2300776 </td></tr>
	<tr><th scope=row>Illiteracy</th><td>-0.58847793</td><td> 0.7029752 </td></tr>
	<tr><th scope=row>HS Grad</th><td> 0.58221620</td><td>-0.4879710 </td></tr>
</tbody>
</table>



En la librería ```ggm``` encontramos la opción para calcular el coeficiente de correlación parcial:


```R
colnames(states)
```


<ol class=list-inline>
	<li>'Population'</li>
	<li>'Income'</li>
	<li>'Illiteracy'</li>
	<li>'Life Exp'</li>
	<li>'Murder'</li>
	<li>'HS Grad'</li>
</ol>




```R
library(ggm)
pcor(c(1,5,2,3,6), cov(states))
```


0.346272371372947


O hacer una prueba de hipótesis para determinar la significancia de la correlación:


```R
cor.test(states[,3], states[,5])
```


    
    	Pearson's product-moment correlation
    
    data:  states[, 3] and states[, 5]
    t = 6.8479, df = 48, p-value = 1.258e-08
    alternative hypothesis: true correlation is not equal to 0
    95 percent confidence interval:
     0.5279280 0.8207295
    sample estimates:
          cor 
    0.7029752 
    


También, podemos aplicar una prueba de correlación para una matriz de datos, obteniendo una matriz de correlaciones y una matriz de valores p de significancia:


```R
library(psych)
corr.test(states, use="complete")
```


    Call:corr.test(x = states, use = "complete")
    Correlation matrix 
               Population Income Illiteracy Life Exp Murder HS Grad
    Population       1.00   0.21       0.11    -0.07   0.34   -0.10
    Income           0.21   1.00      -0.44     0.34  -0.23    0.62
    Illiteracy       0.11  -0.44       1.00    -0.59   0.70   -0.66
    Life Exp        -0.07   0.34      -0.59     1.00  -0.78    0.58
    Murder           0.34  -0.23       0.70    -0.78   1.00   -0.49
    HS Grad         -0.10   0.62      -0.66     0.58  -0.49    1.00
    Sample Size 
    [1] 50
    Probability values (Entries above the diagonal are adjusted for multiple tests.) 
               Population Income Illiteracy Life Exp Murder HS Grad
    Population       0.00   0.59       1.00      1.0   0.10       1
    Income           0.15   0.00       0.01      0.1   0.54       0
    Illiteracy       0.46   0.00       0.00      0.0   0.00       0
    Life Exp         0.64   0.02       0.00      0.0   0.00       0
    Murder           0.01   0.11       0.00      0.0   0.00       0
    HS Grad          0.50   0.00       0.00      0.0   0.00       0
    
     To see confidence intervals of the correlations, print with the short=FALSE option

