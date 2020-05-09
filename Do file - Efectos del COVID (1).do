**********************           Tarea de Econometria                        *****
******************* Efectos del COVID19 sobre los Ingresos Fiscales **************
*******************          Angel David Rodriguez Alvarado 20151022511 **************
*******************          Alexis Daniel Herrera Aguilar  20161001378 **************



//Cargando los datos
clear all
use "C:\Users\user\Desktop\Econometria - Efecto del COVID-19\data2.dta"
tsset Año

/* Modelo:
Ingresos fiscales = B0 + B1*log(PIB_Real) + B2*GINI + e   ; Para toda t
*/

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

// Prueba de Raiz Unitaria para las series

** Evaluando el numero de rezagos optimos para cada variable

varsoc l_pib, exog(Año) //1 lag
varsoc t_Y, exog(Año) //1 lag
varsoc GINI, exog(Año) //2 lags

**Prueba General de Dickey Fuller - PIB Real
dfuller l_pib, lags(1) trend reg // Regresion con tendencia y constante
dfuller l_pib, lags(1) reg // Regresion sin tendencia pero con constante
dfuller l_pib, lags(1) noconstant reg // Regresion sin tendecia y sin constante

**Prueba General de Dickey Fuller - Ingresos Fiscales
dfuller t_Y, lags(1) trend reg // Regresion con tendencia y constante
dfuller t_Y, lags(1) reg // Regresion sin tendencia pero con constante
dfuller t_Y, lags(1) noconstant reg // Regresion sin tendecia y sin constante

**Prueba General de Dickey Fuller - Coeficiente de GINI
dfuller GINI, lags(2) trend reg // Regresion con tendencia y constante

// Verificando si PIB Real e Ingresos Fiscales son estacionarias en tendencia

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
line GINI_detr Año,title(GINI_detrended)
corrgram GINI_detr   //Correlograma cae abruptamente, signo de estacionariedad

//Verificando por Cointegracion Engel - Granger

reg t_Y_detr l_pib GINI
predict residual, res
line residual Año, title(Residuos vs Tiempo)
twoway (scatter residual L.residual) (lfit residual L.residual), title(Residuos vs. Residuos con rezago)
dfgls residual, maxlag(1) // No se rechaza la hipotesis nula

** Ho: No hay Cointegracion
** H1: Si hay Cointegracion

ssc install egranger
egranger l_pib t_Y  // No hay cointegracion

