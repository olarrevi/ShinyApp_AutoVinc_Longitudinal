# ShinyApp_AutoVinc_Longitudinal

Este repositorio contiene una aplicación de Shiny diseñada para la vinculación de bases de datos en formato .sav, siguiendo las etiquetas (labels) correspondientes de las variables. Es especialmente útil para crear un diccionario de variables en estudios longitudinales, como el Panel de Políticas Públicas de la Fundación.
Funcionalidades

- Carga de Datos: Permite cargar dos archivos en formato .sav (archivos SPSS) para su procesamiento.
  + Diccionario: .Sav de referencia donde la otra df se vinculará
  + DF nuevo: DF que se vinculará al DF matriz.
  
- Vinculación de Bases de Datos: La app vincula las bases de datos utilizando las etiquetas de las variables como referencia, lo que facilita la comparación y vinculación usando el método "levenshtein distance".

- Interfaz Interactiva: La app ofrece una interfaz fácil de usar con tablas interactivas y opciones de visualización, mejorando la experiencia del usuario.

## Tecnologías Utilizadas

- Shiny: Framework para construir aplicaciones web interactivas en R.

- R Packages:
  + haven: Para la lectura de archivos .sav.
  + dplyr, tidyr: Para manipulación de datos.
  + fuzzyjoin: Para unir bases de datos utilizando coincidencias aproximadas.
  + DT: Para tablas interactivas en la interfaz.
  + ggplot2: Para visualización de datos.

## Instalación y Ejecución

- Clonar el repositorio:

`git clone https://github.com/usuario/ShinyApp_AutoVinc_Longitudinal.git`

- Restaurar el entorno de R con renv:

`renv::restore()`

- Ejecutar la aplicación Shiny:

`shiny::runApp("app.R")`

## Uso

- Subir las Bases de Datos: Seleccionar dos archivos .sav para cargarlos en la aplicación.
- Vinculación de Variables: La aplicación mostrará las variables de ambas bases de datos y realizará la vinculación utilizando las etiquetas correspondientes.
- Exploración y Exportación: Una vez vinculadas, se puede explorar el diccionario generado y exportarlo para su posterior uso.

## Aplicaciones Prácticas

- Estudios Longitudinales: Es ideal para estudios que requieren la comparación de variables a lo largo del tiempo, como encuestas periódicas o paneles de investigación.
- Gestión de Datos en Políticas Públicas: Facilita la creación de diccionarios de variables y la unificación de datos provenientes de distintas fuentes en el ámbito de la gestión pública.

## Contribuciones

Las contribuciones son bienvenidas. Por favor, abre un issue o envía un pull request para sugerencias y mejoras.
Contacto

Para preguntas o sugerencias, puedes contactar con Oriol Larrea i Vivés a través del correo electrónico: oriollarrea111@gmail.com.
