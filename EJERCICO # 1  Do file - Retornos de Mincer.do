************** Tarea Domiciliaria 2 de Econometria ********************
****Elaborado Por : 
************** Angel David Rodriguez Alvarado 20151022511
***************Alexis Daniel Herrera AGUILAR  20161001378 
**************           
// Cargando los datos
 use "C:\Users\User\Desktop\Data - Hogar0512.dta", clear

 // ***  INCISO A  "Especificar El Modelo Poblacional" ***
 
 /*Ln Ingresos = B0 + B1*Educacion + B2*Experiencia + B3*Experiencia^2 + e

 Donde:
 Ln Ingresos = Logaritmo natural de Salario mensual
 Educacion = Años de educacion formal
 Experiencia = Edad - Años de estudio - Edad inicio educacion.
 Experiencia^2 = Años de Experiencia Laboral al Cuadrado.
       e         = Perturbación Aleatoria
	   
 *** Inciso B  " Especifique el tipo, fuente y el horizonte de datos que utilizaría para el 
 estimar el modelo poblacional propuesto.
	
	Tipo: Regresion por Minimos cuadrados ordinarios
	Fuente: Encuesta permanente de Hogares 2012. INE
	Horizonte: 2012, datos de corte tranversal
	

 * "Inciso C "  Estudie los datos perdidos y los datos atípicos para cada variable.
*/
*Datos Perdidos
ssc install nmissing.pkg

nmissing ysmop //  ** El Ingreso Del salario mensual  Tiene 27,148 Valores Perdidos **
nmissing anosest //  ** Lo Años de Estudio Tiene 7,030 Valores Prohibidos**
nmissing EDAD // ** La Edad No tiene Valores Prohibidos **
 
**

//Evaluando la distribucion muestral de los regresores
qnorm ysmop  // No concluyente
qnorm anosest  // Distribuida normalmente
qnorm EDAD     // No concluyente

//Evaluando normalidad formalmente con el criterio de asimetria
sum ysmop, detail // Asimetria muy significativa. No normalmente distribuida
sum EDAD, detail //Asimetria baja. Normalmente distribuida

* Datos atipicos
grubbs anosest // Efectuamos la prueba de Grubbs. No hay valores atipicos
grubbs EDAD    // No hay valores atipicos


** Inciso D "Obtenga adecuadamente el modelo muestral" **

// Estimando el modelo por OLS

* Redefiniendo variables

gen a = EDAD - anosest - 6   //Generando variable experiencia

gen l_ingresos = log(ysmop)  //Logaritmizando variable ingresos
gen l_ingresos_sinatipicos = log(ysmop) if ysmop  <= 30000  // Eliminando valores atipicos
gen educ = anosest            // Generando variable educacion
gen exper = a if a >= 0      //tomando solo años de experiencia positivos 
gen exper_sqr = exper*exper   // Generando regresos experiencia al cuadrado


// Regresion
regress l_ingresos educ exper exper_sqr              // Regresion con valores atipicos
regress l_ingresos_sinatipicos educ exper exper_sqr // Regresion sin valores atipicos

// Evaluando si existe Heterocedasticidad
hettest  // Rechazamos la hipotesis nula concluyendo que si hay heterocedascticidad

**Inciso E " Estudie la significancia e  Interprete el retorno De Mincer ""

** Hipotesis De Significancia ***
** HO : Si P>|t| es menor a 0.05 .... la variable es estadisticamente significativa ***
** H1 : Si P>|t|es mayor a 0.05......la variable no es estadisticamente significativa ***

*Años de Escolaridad de la persona* = 0.1265812<0.05 = Los Años de Escolaridad de la persona son estadisticamente significativos 
*Años de Experiencia Laboral* = 0.486834 < 0.05  = Los Años de Experiencia Laboral son estadisticamnete significativos 
*Años de Experiencia Laboral al Cuadrado  = -0.0007553 < 0.05 = Los Años de Experiencia Laboral al Cuadrado son estadisticamente significativos 


***Retorno De Mincer***

**ysmop = 6.59262 + 0.1315429Educacion  + 0.533899Experiencia - 0.0006605Experiencia^2+ e


*** Si nuestra población no presenta años de estudio ni años de experienca laboral, 
*** el ingreso séra aproximadamente de 6.59262

*** Si los años de estudio aumentan en 1 unidad adicional, los ingresos se incrementaran en 0.1315429 %
*** Si la experiencia laboral aumenta en 1 unidad adicional, los ingresos se incrementaran en 0.533899% 
**Si la experiencia laboral  al cuadrado aumenta en 1 unidad adicional, los ingresos dismiran en  0.06605%

*** Modelo mediante Corrección de Heckman (Heckit)

gen Y=.
replace Y=0 if ysmop<30000 
replace Y=1 if ysmos!=.

tab Y

*** Visualizar Criterio de Dummy

br Y ysmos ysmop

**** Estimación del modelo de Corrección de Heckman (Modelo Heckit)

heckman ln_ingresos educ exper exper_sqr if (exper >=0), select(Y = Rama3c)

*** Guardar el modelo mediante Heckit

estimate store Heckman





















                                                                                        
