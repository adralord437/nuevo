**********************           Tarea de Econometria                        *****
******************* Efectos del COVID19 sobre los Ingresos Fiscales **************
*******************          Angel David Rodriguez Alvarado 20151022511 **************
*******************          Alexis Daniel Herrera Aguilar  20161001378 **************



//Cargando los datos
clear all
use 
"C:\Users\user\Desktop\Econometria - Efecto del COVID-19\data2.dta"

import excel "E:\Data final .xlsm", sheet("Sheet1") firstrow

tsset Año
*** Inciso A " Especifique El Modelo Teorico " 

Modelo:
Ingresos fiscales = B0 + B1*log(PIB_Real) + B2*GINI + e   ; Para toda t

Donde : 

* Variable Independiente : Ingreso Fiscales Como Proporcion Del PIB 
	
* Variables Dependientes :  

**  log(PIB_Real) =  Pib Real De Honduras en Log
**           Gini = Coefiente De Gini Para Honduras  
**             e  = Perturbacionaes aleatorias 
**             B0 = Constante
**             β1 = Parámetro del PIB real 
**             β2 = Parámetro del Coefiente De Gini
 
Inciso B     " Especique el Tipo , Fuente , Horizonte  ***

** Tipo : Regresion de Minimos Cuadrados Ordinarios (OLS) 
 **Fuente : Los ingresos fiscales fueron extraídos de la base de datos de la OCDE  "Organización para la Cooperación y el Desarrollo Económicos" 
            PIB Real fue extraído de los datos del BCH Banco Central De Honduras" 
			El Coeficente de Gini fue extraido de la Base de Datos del Banco Mundial.
 **Horizonte : Horizonte de datos del tipo " Serie De Tiempo "

 Inciso C  " Estudie La Estacionariedad de cada vaiable de cada variable del modelo" 

//Redefiniendo variables
rename var10 GINI
gen l_pib = log(pib_real)
gen t_Y = if_deflactados/pib_real // Ingresos Fiscales como proporcion del PIB

//Visualizando las series
line l_pib Año, title(Pib_Real Log) // PIB Real
line t_y Año, title(Ingresos Fiscales (%PIB))  //Ingresos Fiscales
line GINI Año, title(Coeficiente de GINI Honduras)

// Correlogramas

** PIB Real
corrgram l_pib, lags(17)
ac l_pib, lags(17)
pac l_pib, lags(6)

** Ingresos Fiscales
corrgram t_Y, lags(17)
ac t_Y, lags(17) title(Correlograma Total Ingresos Fiscales %PIB)
pac t_Y, lags(6) title(Correlograma Parcial Ingresos Fiscales %PIB)
**
corrgram GINI, lags(17)
ac GINI, lags(17) title(Correlograma Total Coeficiente de GINI)
pac GINI, lags(6) title(Correlograma Parcial Coeficiente de GINI)

inciso D  " Estudie el tipo de tendencia " 

// Prueba de Raiz Unitaria para las series

** Evaluando el numero de rezagos optimos para cada variable

varsoc l_pib, exog(Año) //1 lag
varsoc t_Y, exog(Año) //1 lag
varsoc GINI, exog(Año) //2 lags

**Prueba General de Dickey Fuller - PIB Real
dfuller l_pib, lags(1) trend reg // Regresion con tendencia y constante
dfuller l_pib, lags(1) reg // Regresion sin tendencia pero con constante
dfuller l_pib, lags(1) noconstant reg // Regresion sin tendecia y sin constante

** Conclusion : El PIB real no tiene raiz unitaria. Serie estacionaria en tendencia


**Prueba General de Dickey Fuller - Ingresos Fiscales
dfuller t_Y, lags(1) trend reg // Regresion con tendencia y constante
dfuller t_Y, lags(1) reg // Regresion sin tendencia pero con constante
dfuller t_Y, lags(1) noconstant reg // Regresion sin tendecia y sin constante

* Conclusion : Los ingresos fiscales tienen tienen raiz unitaria y por ende, es estacionaria en diferencias.


**Prueba General de Dickey Fuller - Coeficiente de GINI
dfuller GINI, lags(2) trend reg // Regresion con tendencia y constante

* conclucion : El coeficeinte de Gini no tiene raiz unitaria. Estacionaria en tendencia

Inciso E "  //Verificando por Cointegracion Engel - Granger " 

// Estacionarizando las series. Aplicando detrend y diferencias.

* PIB Real
gen trend = _n
reg l_pib trend
predict pib_detr, resid
line pib_detr Año, title(PIB_detrended)
corrgram pib_detr  //Correlograma cae abruptamente, signo de estacionariedad

* Ingresos Fiscales
gen t_Y_detr = d1.t_Y // Sacando primera diferencias por tener raiz unitaria
line t_Y_detr Año, title(Ingresos Fiscales %PIB)
*GINI
reg GINI trend
predict GINI_detr, resid
predict pibt, resid
line GINI_detr Año,title(GINI_detrended)
corrgram GINI_detr   //Correlograma cae abruptamente, signo de estacionariedad

reg t_Y_detr l_pib GINI 
predict residual, res
line residual Año, title(Residuos vs Tiempo)
twoway (scatter residual L.residual) (lfit residual L.residual), title(Residuos vs. Residuos con rezago)
dfgls residual, maxlag(1) // No se rechaza la hipotesis nula

** Ho: No hay Cointegracion
** H1: Si hay Cointegracion

ssc install egranger
egranger l_pib t_Y  // No hay cointegracion


*Inciso f.) En el modelo estimado se puede hacer inferencia estandar?

// Verificando si se cumplen supuestos para inferencia estandar
reg t_Y_st pib_detr GINI_detr
predict resid2, resid

** Prueba Breusch-Godfrey
* H0: Homocedasticidad; Ausencia autocorrelacion
* H1: Heterocedasticidad; Presencia Autocorrelacion

estat bgodfrey  // No hay presencia de autocorrelacion
hettest            // Hay presencia de heterocedasticidad

** Verificando Normalidad de los errores
qnorm resid2        // Hay presencia de Normalidad
sum resid2, detail  
histogram resid2

** Dado que hay heterocedasticidad se estima el modelo con matriz robusta de errores
reg t_Y_st pib_detr GINI_detr, robust

* Inciso H " Estime el Modelo De correccion De errores " 

** Regresion de largo plazo 
reg t_Y_detr l_pib GINI
predict residual, res
varsoc residual, exog(Año)
dfgls residual, maxlag(0) * los criterios de informacion nos dicen que el maximo de rezagos es 0

***Diferencias de una serie
gen dl_pib=d.l_pib
gen dGINI=d.GINI
gen lres=l.residual * resagamos un periodo los residuos de la regrecion a largo plazo 
gen dres=d.lres

** Regresion De Corto Plazo 
reg t_Y_detr dl_pib dGINI lres 

I - )  Interprete el impacto del crecimiento de la economía en  “ H” 
Basándonos en el inciso  “H”   podemos decir que: 
Si el coefiente de GINI  aumenta en 1 unidad adicional, la recaudación tributaria como proporción del PIB disminuyen en 0.0008828 puntos. 
Si el pib real aumenta en 1 %  adicional, la recaudación tributaria como proporción del PIB  se incrementaran en 0.2739702  puntos.



