**********************           Tarea de Econometria                        *****
******************* Efectos del COVID19 sobre los Ingresos Fiscales **************
*******************          Angel David Rodriguez Alvarado         **************
*******************                  20151022511                    **************



//Cargando los datos
use "C:\Users\user\Desktop\Econometria - Efecto del COVID-19\data.dta"
tsset Año
/* Modelo:
Ingresos fiscales = B0 + B1*log(PIB_Real) + e   ; Para toda t
*/

//Redefiniendo variables
gen l_pib = log(pib_real)
gen t_y = if_deflactados/pib_real // Ingresos Fiscales como proporcion del PIB
//Visualizando las series
line l_pib Año, title(Pib_Real Log) // PIB Real
line t_y Año, title(Ingresos Fiscales (%PIB))  //Ingresos Fiscales

// Correlogramas
** PIB Real
corrgram l_pib, lags(17)
ac l_pib, lags(17)
pac l_pib, lags(6)

** Ingresos Fiscales
corrgram t_Y, lags(17)
ac t_Y, lags(17)
pac t_Y, lags(6)

// Prueba de Raiz Unitaria para las series

** Evaluando el numero de rezagos optimos para cada variable

varsoc l_pib, exog(Año) //1 lag
varsoc t_Y, exog(Año) //1 lag

**Prueba General de Dickey Fuller - PIB Real
dfuller l_pib, lags(1) trend reg // Regresion con tendencia y constante
dfuller l_pib, lags(1) reg // Regresion sin tendencia pero con constante
dfuller l_pib, lags(1) noconstant reg // Regresion sin tendecia y sin constante

**Prueba General de Dickey Fuller - Ingresos Fiscales
dfuller t_Y, lags(1) trend reg // Regresion con tendencia y constante
dfuller t_Y, lags(1) reg // Regresion sin tendencia pero con constante
dfuller t_Y, lags(1) noconstant reg // Regresion sin tendecia y sin constante
