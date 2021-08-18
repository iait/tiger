# Compilador Tiger

La carpeta *src/* contiene los fuentes SML y el runtime en C del compilador tiger y la carpeta *test/* contiene programas escritos en lenguaje tiger para pruebas.

Ejecutando *make* en el directorio actual se generan dos ejecutables en esta misma carpeta: el compilador **tiger** y un ejecutable para pruebas **tiger-test**.

Al ejecutar *tiger-test* se prueba hasta la etapa de traducción a código intermedio todos los programas tiger *testN.tig* de la carpeta *test/*. Para los programas tiger que tengan errores *tiger-test* verifica que el mensaje de error sea el esperado.

Para compilar un programa tiger utilizar el ejecutable *tiger* pasándole como argumento el archivo fuente que queremos compilar. Esto genera un assembler con extensión *.s* y un ejecutable sin extensión en la misma carpeta que el fuente con el mismo nombre. Por ejemplo, al ejecutar:

`$ ./tiger test/example01.tig`

Se genera el assembler *test/example01.s* y se lo enlaza con el runtime generando el ejecutable *test/example01*. Los programas *exampleN.tig* de la carpeta *test/* son programas simples para probar los diferentes aspectos del compilador como asignación de registros, volcado de variables a memoria cuando los registros no son suficientes, llamadas a funciones de librería, manejo de caller y callee-save, funciones anidadas, variables escapadas, manejo de static link, convención de llamada a función, etc.

Luego se puede ejecutar el programa tiger compilado con

`$ test/example01`

y verificar el resultado con

`$ echo $?`

En este ejemplo el resultado deberá ser 45.

### Parámetros del compilador
- **-arbol**: imprime el árbol de sintaxis abstracta del programa (antes del cálculo de escapes),
- **-escapes**: imprime el AST luego del cálculo de escapes,
- **-ir**: imprime el código intermedio generado (antes de la etapa de canonización),
- **-canon**: imprime el código intermedio canonizado,
- **-inter**: ejecuta el intérprete de código intermedio (no muy probado),
- **-alloc**: imprime todo lo relacionado con la asignación de registros, como ser control-flow graph, grafo de interferencias y pasos detallados para el coloreo.

