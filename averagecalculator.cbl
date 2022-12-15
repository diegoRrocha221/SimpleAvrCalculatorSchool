      ******************************************************************
      * Author:Diego R. Rocha
      * Date:18/05/2016
      * Purpose:Realizar o calculo de media aritmetica simples
      * para resultado de notas
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CalculadoraMediaSimples.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       01 NOME-ALUNO PIC X(30).
       01 MATERIA PIC X(20).
       01 NOTAS PIC 99V99 OCCURS 4.
       01 MEDIA PIC 99V99.
       01 RESULTADO PIC X(10).
       01 I PIC 99.
       01 CONTINUAR PIC X.

       PROCEDURE DIVISION.
       MEDIA-ALUNO.

       DISPLAY "Entre com o nome do aluno: "
       ACCEPT NOME-ALUNO

       DISPLAY "Entre com a matéria: "
       ACCEPT MATERIA

       PERFORM VARYING I FROM 1 BY 1
        UNTIL I > 4
        DISPLAY "Entre com a nota " I ": "
        ACCEPT NOTAS(I)
        IF NOTAS(I) < 0 THEN
          DISPLAY "Nota inválida! Por favor entre com uma nota válida"
          GO TO MEDIA-ALUNO
        ELSE IF NOTAS(I) > 10 THEN
        DISPLAY "Nota inválida! Por favor entre com uma nota válida"
        GO TO MEDIA-ALUNO
        END-IF
       END-PERFORM

       MOVE ZERO TO MEDIA

       PERFORM VARYING I FROM 1 BY 1
        UNTIL I > 4
        ADD NOTAS(I) TO MEDIA
       END-PERFORM

       DIVIDE MEDIA BY 4 GIVING MEDIA

       IF MEDIA >= 7 THEN
        MOVE "Aprovado" TO RESULTADO
       ELSE
        MOVE "Reprovado" TO RESULTADO
       END-IF

       DISPLAY NOME-ALUNO " foi " RESULTADO " em " MATERIA

       DISPLAY "Deseja continuar (S/N)? "
       ACCEPT CONTINUAR

       IF CONTINUAR = "S" THEN
         GO TO MEDIA-ALUNO
       ELSE
         STOP RUN
       END-IF.
