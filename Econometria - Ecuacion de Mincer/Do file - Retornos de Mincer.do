************** Tarea Domiciliaria 2 de Econometria ********************
************** Angel David Rodriguez Alvarado**************************
**************           20151022511         **************************

// Cargando los datos
 use "C:\Users\User\Desktop\Data - Hogar0512.dta", clear

 // Especificando el modelo a estimar
 
 /*Ln Ingresos = B0 + B1*Educacion + B2*Experiencia + B3*Experiencia^2 + e

 Donde:
 Ln Ingresos = Logaritmo natural de Salario mensual
 Educacion = Años de educacion formal
 Experiencia = Edad - Años de estudio - Edad inicio educacion.    

 
 b) Especifique el tipo, fuente y el horizonte de datos que utilizaría para el
    estimar el modelo poblacional propuesto.
	
	Tipo: Regresion por Minimos cuadrados ordinarios
	Fuente: Encuesta permanente de Hogares 2012. INE
	Horizonte: 2012, datos de corte tranversal
	

c) Estudie los datos perdidos y los datos atípicos para cada variable.
*/
*Datos Perdidos
ssc install nmissing.pkg

nmissing ysmop // Valores perdidos salario mensual
nmissing anosest // Valores perdidos nivel educativo
nmissing EDAD // Valores perdidos Edad
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





















                                                                                        