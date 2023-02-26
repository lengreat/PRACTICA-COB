      ******************************************************************
      * AUTHOR: LEONARDO VILLAFUERTE
      * DATE: 17/02/23
      * PURPOSE: PRACTICA TALLER
      * TECTONICS: COBC
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. MENU01.
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
      *-----------------------
       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
      *-----------------------
       WORKING-STORAGE SECTION.
      *-----------------------
       01  FILLER                        PIC X(30) VALUE "INICIO WORKING
      -    "MENU01".
       01  WS-VARIABLES.
           05  WS-CEDULA                 PIC 9(10).
           05  WS-COD-PRODUCT            PIC X(10).
           05  WS-OPCION                 PIC 9.
    
       01  SW-CONTINUAR                  PIC X     VALUE 'X'.
           88 SW-SI-CONTINUA                       VALUE 'S'.
           88 SW-NO-CONTINUA                       VALUE 'N'.
      
      *+---------------------------------------------------------------+
      *                           TABLAS
      *+---------------------------------------------------------------+

       01  TB-FACTURA.
           05 TB-DATOS OCCURS 10 TIMES INDEXED BY TB-INDICE.
              10 TB-DATOS-CLIENTE.
                 15 WK-NOMBRE            PIC X(30).
                 15 WK-APELLIDO          PIC X(30).
                 15 WK-ID-TIPO           PIC X.
                 15 WK-ID                PIC 9(10).
                 15 WK-NACIMIENTO.
                    20 WK-DIA            PIC 99.
                    20 FILLER            PIC X VALUE '/'.
                    20 WK-MES            PIC 99.
                    20 FILLER            PIC X VALUE '/'.
                    20 WK-ANIO           PIC 9(4).
                 15 WK-DIRECCION         PIC X(60).
                 15 WK-TELEFONO          PIC 9(10).
              10 TB-DATOS-FACTURA.
                 15 WK-FACTURAID         PIC 9(18).
                 15 WK-TASA-IVA          PIC 9(2)V99.
                 15 WK-VALORT-FACTURA    PIC 9(10)V99.
                 15 WK-CODIGO-PRODUCTO   PIC X(10).
                 15 WK-CANTIDAD          PIC 9(8).
                 15 WK-PRECIO-UNITARIO   PIC 9(10)V99.
                 15 WK-VALORTOTAL-CXP    PIC S9(15)V99 COMP-3.
                 15 WK-DESC              PIC 9(5)V99.
                 15 WK-VALORIVA          PIC 9(5)V99.

       01  FILLER                        PIC X(30) VALUE "FINAL WORKING
      -    "MENU01".

       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       COMIENZO-MENU01.
           PERFORM 10000-INICIO
           PERFORM 20000-PROCESO
           PERFORM 30000-FIN
           .

       10000-INICIO.
           INITIALIZE WS-VARIABLES
                      TB-FACTURA
                      SW-CONTINUAR
                      TB-INDICE
                      REPLACING NUMERIC BY ZEROES ALPHANUMERIC BY SPACES
           .
       20000-PROCESO.
           PERFORM UNTIL WS-OPCION=3
               DISPLAY "1.- INGRESO DE CLIENTES"
               DISPLAY "2.- CONSULTA DE CLIENTES"
               DISPLAY "3.- SALIR"
               DISPLAY "DIGITE UNA OPCION"
               ACCEPT WS-OPCION
               PERFORM 20100-INGRESO-OPCION
           END-PERFORM
           .
       20100-INGRESO-OPCION.
           EVALUATE WS-OPCION
             WHEN 1 PERFORM  20200-INGRESO-CLIENTES
             WHEN 2 PERFORM  20300-BUSQUEDA-CLIENTES
           END-EVALUATE
           .


       20200-INGRESO-CLIENTES.
           SET SW-SI-CONTINUA TO TRUE
           SET TB-INDICE TO 0
           INITIALIZE TB-FACTURA
           PERFORM 20210-INGRESO-DATOS UNTIL SW-NO-CONTINUA
           .

       20210-INGRESO-DATOS.
           SET TB-INDICE UP BY 1
           DISPLAY "INGRESE NOMBRE"
           ACCEPT WK-NOMBRE(TB-INDICE)
           DISPLAY "INGRESE APELLIDO"
           ACCEPT WK-APELLIDO(TB-INDICE)
           DISPLAY "INGRESE TIPO ID: C O P"
           ACCEPT WK-ID-TIPO(TB-INDICE)
           DISPLAY "INGRESE ID DE 10 DIGITOS"
           ACCEPT WK-ID(TB-INDICE)
           DISPLAY "INGRESE FECHA DE NACIMIENTO DD/MM/AAAA"
           ACCEPT WK-NACIMIENTO(TB-INDICE)
           DISPLAY "INGRESE DIRECCION"
           ACCEPT WK-DIRECCION(TB-INDICE)
           DISPLAY "INGRESE TELEFONO 10 DIGITOS"
           ACCEPT WK-TELEFONO(TB-INDICE)
           DISPLAY "INGRESE NUMERO DE FACTURA"
           ACCEPT WK-FACTURAID(TB-INDICE)
           DISPLAY "INGRESE TASA DE IVA %"
           ACCEPT WK-TASA-IVA(TB-INDICE)
           DISPLAY "INGRESE CODIGO PRODUCTO 10 ALFANUMERICOS"
           ACCEPT WK-CODIGO-PRODUCTO(TB-INDICE)
           DISPLAY "INGRESE CANTIDAD HASTA 8 DIGITOS"
           ACCEPT WK-CANTIDAD(TB-INDICE)
           DISPLAY "INGRESE PRECIO UNITARIO $"
           ACCEPT WK-PRECIO-UNITARIO(TB-INDICE)
           DISPLAY "INGRESE DESCUENTO HASTA 5 DIGITOS"
           ACCEPT WK-DESC(TB-INDICE)
           DISPLAY "DESEA CONTINUAR? Y/N"
           ACCEPT SW-CONTINUAR
           COMPUTE WK-VALORTOTAL-CXP(TB-INDICE)=
                    WK-CANTIDAD(TB-INDICE)
                    * WK-PRECIO-UNITARIO(TB-INDICE)
           COMPUTE WK-VALORIVA(TB-INDICE)=
                    WK-VALORTOTAL-CXP(TB-INDICE)
                    * (WK-TASA-IVA(TB-INDICE)/100)
           COMPUTE WK-VALORT-FACTURA(TB-INDICE)=
                    WK-CANTIDAD(TB-INDICE)
                    * WK-PRECIO-UNITARIO(TB-INDICE)
           IF (TB-INDICE=10)
               DISPLAY "YA NO HAY MAS DATA QUE LLENAR"
               MOVE 'N' TO SW-CONTINUAR
           END-IF
           .
       20300-BUSQUEDA-CLIENTES.
           DISPLAY "BUSQUEDA DE CLIENTES"
           DISPLAY "INGRESE NUMERO DE CEDULA"
           ACCEPT WS-CEDULA
           DISPLAY "INGRESE CODIGO DE PRODUCTO"
           ACCEPT WS-COD-PRODUCT
           SET TB-INDICE TO 1
           SEARCH TB-DATOS AT END
                  DISPLAY "NO SE ENCONTRO LA FACTURA"
                  WHEN WK-ID(TB-INDICE)=WS-CEDULA AND
                       WK-CODIGO-PRODUCTO(TB-INDICE)=WS-COD-PRODUCT
                       PERFORM 20400-DISPLAY-FACTURA
           END-SEARCH
           .
       20400-DISPLAY-FACTURA.
           DISPLAY "SE ENCONTRO LA FACTURA: "
           DISPLAY '*******************************'
           DISPLAY "** NOMBRE: " WK-NOMBRE(TB-INDICE)
           DISPLAY "** APELLIDO: " WK-APELLIDO(TB-INDICE)
           DISPLAY "** TIPO DE ID: "WK-ID-TIPO(TB-INDICE)
           DISPLAY "** ID: " WK-ID(TB-INDICE)
           DISPLAY "** NACIMIENTO: " WK-NACIMIENTO(TB-INDICE)
           DISPLAY "** DIRECCION: " WK-DIRECCION(TB-INDICE)
           DISPLAY "** TELEFONO: " WK-TELEFONO(TB-INDICE)
           DISPLAY "** FACTURAID: " WK-FACTURAID(TB-INDICE)
           DISPLAY "** TASA-IVA: " WK-TASA-IVA(TB-INDICE)
           DISPLAY "** CODIGO-PRODUCTO:"  WK-CODIGO-PRODUCTO(TB-INDICE)
           DISPLAY "** CANTIDAD: " WK-CANTIDAD(TB-INDICE)
           DISPLAY "** PRECIO-UNITARIO: " WK-PRECIO-UNITARIO(TB-INDICE)
           DISPLAY "** DESCUENTO: " WK-DESC(TB-INDICE)
           DISPLAY "** VALOR IVA: " WK-VALORIVA(TB-INDICE)
           DISPLAY "** TOTAL VALOR: " WK-VALORTOTAL-CXP(TB-INDICE)
           DISPLAY "** VALOR FACTURA: " WK-VALORT-FACTURA(TB-INDICE)
           DISPLAY'*******************************'
           .
       30000-FIN.
            STOP RUN.
      ** add other procedures here
       END PROGRAM MENU01.
