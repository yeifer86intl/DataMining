#La función kmeans viene por defecto en la libreria: stat.

set.seed(12345)

#Invocamos la Base de datos Iris.
data(iris)
datos=iris

#Verificamos la composición del set de datos.
#En este caso hay 5 variables: 4 numericas (Sepal y Petal) y 1 categorica determinada como factor (Species).
str(datos)


# En este caso la variable objetivo, nuestra y, que es la variable: Species.
datos$Species

#Las primeras 50 son: Setosas, las 50 siguientes son: Versicolor y, las ultimas 50,
#son: virginica y, estas conforman las 3 CLASES a pronósticar.


# Gráfico de dispersión de las clases según las variables predictoras.
pairs(datos[-5],col=datos$Species)


# Verificamos la correlación entre las variables NUMERICAS existentes.
correlacion=cor(datos[-5])
correlacion

#Con esto podemos observar que combinación de variables van a portar mayor información a la
# formación de los grupos.


#-----------------------------------------------------------------------------


#Aplicación del Método: K MEANS.

#El objetivo del Kmeans o K medias es formar grupos homogeneos, distintos entre si.
#En este caso, nuestro k=3, formaremos 3 grupos, porque ya conocemos que hay
# 3 distintos tipos de iris, sin embargo, el n, dependerá del problema.

grupos=kmeans(datos[-5],3) 

# La función kmeans recibe dos parametros: datos (solo las variables numericas) y k (número de grupos a formar).

grupos

#El número 3: corresponde a las Setosas, el número 2 a las Versicolor y el número 1 a las Virginicas. 


#Con la siguiente sentencia se obtiene el grupo al cual pertenecen los registros o filas
#del set de datos según la función: kmeans .

grupos$cluster

#Como se puede apreciar se han formado 3 clusters o grupos.

#Grupo 3, corresponden a: Setosas.
#Grupo 1, corresponden a: Versicolor.
#Grupo 2: corresponden a: Virginica.

#-----------------------------------------

#¡ Importante ! : 
# Como es un método NO supervisado, no necesita una variable objetivo (y) para operar.

#-----------------------------------------


#-----------------------------------------------------------------------------

#Evaluación de Resultados

#Asignamos a la variable: clases, los datos numericos como factor del set de datos.

clases=factor(datos[,5]) 


#Asignamos a la variable: predicho, las clases predichas por el kmedias. 

predicho= NULL 


# La función: mode (moda), contará la cantidad de registros clasificados como: 
# setosa, versicolor o virginica en cada llamada y, retornará la mayor cantidad
# de ocurrencias vinculadas a la clase que se le asigne.


mode=function (x){
  ux=unique(x)
  ux[which.max(tabulate(match(x,ux)))]
}

#Segmentamos el clustering vector de 50 en 50, hasta llegar a 150, es decir, 3 grupos de 50,
#porque, al inicio, habian 50 flores de cada tipo: Setosas, Versicolor y Virginica.

#-----------------------------------------

#¡ Importante ! : 
# Evidentemente el algoritmo tiene errores en clasificar entre Versicolor y Virginica, por lo cual, se necesita
#conocer la precisión y el error asociado al modelo.

#Setosas
#[1] 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3

#Versicolor
# 1 1 2 1
#[55] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1

#Virginicas
#2 1 2 2 2 2 1 2
#[109] 2 2 2 2 2 1 1 2 2 2 2 1 2 1 2 1 2 2 1 1 2 2 2 2 2 1 2 2 2 2 1 2 2 2 1 2 2 2 1 2 2 1


#-----------------------------------------


#Así pues, las primeras 50 eran: Setosas, las 50 siguientes eran: Versicolor y, las ultimas 50,
#eran: virginica y, estas conforman las 3 CLASES a pronósticar.

predicho [grupos$cluster==mode(grupos$cluster[1:50])]="setosa"         #Grupo 3
predicho [grupos$cluster==mode(grupos$cluster[50:100])]="versicolor"
predicho [grupos$cluster==mode(grupos$cluster[100:150])]="virginica"

#-----------------------------------------

#¡ Importante ! : 
# Verificar que el orden de los grupos son los que se evaluaron al inicio.

#-----------------------------------------

# Se transforma la variable: predicho, a factor.
predicho=factor(predicho)


#Matriz de confusión

MC=table(clases, predicho)
MC


precision=(sum(diag(MC)))/sum(MC)
precision

error=1-precision
error


# En este caso sabiamos cuantos grupos podiamos formar, sin embargo, para otras
# aplicaciones donde no se sepa correctamente cuántos grupos formar es necesario
# realizar el test que indicaremos en otra entrega.




barplot(t(MC), col=2:4, 
        legend.text=c("Cluster 3","Cluster 1","Cluster 2"),
        main="Presición del Método según las clases reales")

barplot(MC, col=2:4, 
        names.arg = c("Cluster 3","Cluster 1","Cluster 2"),
        legend.text=c("Setosa","Versicolor","Virginica"),
        main="Asertividad del método según las clases predichas")

seeds=c(51,88,99,139,675,25,5613,123,32,346,3,42,678,48902, 8485, 543, 78,72,13,65124,876,91384,6247,9348,12,345,789,29,248,7)

InerciaIC=rep(0,30)
for (s in 1:30) {
  set.seed(seeds[s])
  grups=kmeans(datos[-5],3)
  InerciaIC[s]=grups$tot.withinss
}

plot(InerciaIC, col="blue",type="b", main="Inercia total intra clase (nstar=1)")

summary(as.factor(InerciaIC))


seeds=c(51,88,99,139,675,25,5613,123,32,346,3,42,678,48902, 8485, 543, 78,72,13,65124,876,91384,6247,9348,12,345,789,29,248,7)

InerciaIC=rep(0,30)
for (s in 1:30) {
  set.seed(seeds[s])
  grups=kmeans(datos[-5],3, nstart=10)
  InerciaIC[s]=grups$tot.withinss
}
# con nstar=10 se obtiene una solución optima

plot(InerciaIC, col="blue",type="b", main="Inercia total intra clase (nstar=1)")

c42= combn(1:4,2) # combinaciones de 4 en 2
c43= combn(1:4,3) # combinaciones de 4 en 3
c44= combn(1:4,4) # combinaciones de 4 en 4
c44
variables=list(c42 [,1], c42 [,2], c42 [,3], c42 [,4],c42 [,5], c42 [,6],
               c43 [,1], c43 [,2], c43 [,3], c43 [,4], 
               c44 [,1])
# todas las combinaciones de 2,3 y 4 variables


##Selección de variables predictoras

InerciaIC=rep(0,11)
PrecisGlob=rep(0,11)

for (v in 1:11){
  grups=kmeans(datos[,variables[[v]] ],3, nstart=10)
  InerciaIC[v]=grups$betweens/grups$totss
  predicho= NULL 
  predicho [grups$cluster==mode(grups$cluster[1:50])]="setosa"
  predicho [grups$cluster==mode(grups$cluster[50:100])]="versicolor"
  predicho [grups$cluster==mode(grups$cluster[100:150])]="virginica"
  predicho=factor(predicho)
  MC= table (clases, predicho)
  PrecisGlob[v]= (sum(diag(MC)))/sum(MC)
}

for (v in 1:11) {
  cat(v,":", variables [[v]], "(", names(datos)[variables[[v]] ] , ")", "\n")
}

par(mfrow=c(1,2))
plot (PrecisGlob, col="blue", type="b", main="Precisión global del método")
plot (InerciaIC, col="blue", type="b", main="Inercia total intra-clases")

#Codo de Jambu

#Ejemplo del Codo de Jambu

InerciaIC=rep(0,30)
for (k in 1:30) {
  grups=kmeans(datos[-5],k,nstart=10)
  InerciaIC[k]=grups$tot.withinss
}

plot (InerciaIC, col="blue", type="b")
points(x=3, y=InerciaIC[3],col="red", pch=19)

grupos$betweenss
grupos$withinss
grupos$tot.withinss
grupos$ifault
