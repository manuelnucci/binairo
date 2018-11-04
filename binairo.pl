:- dynamic tablero/1.

% genero_lista_solucion([I,J,C|L]):- retract(tablero(I,J,C)),
% genero_lista_solucion(L).
% genero_lista_solucion([]).

%###############################################
%################GUI temporal###################
%###############################################

siguiente(X, Y):- X < 5, Y is X+1.
siguiente(X, Y):- X1 is X+1, siguiente(X1, Y).

imprimir(I, I, _).
imprimir(I, N, M):- I < N,
                    write('                             '),
                    imprimir_fila(I, 0, M),
                    I1 is I+1,
                    imprimir(I1, N, M).

imprimir_fila(_, M, M):- nl.
imprimir_fila(I, J, M):- J < M, (tablero(I, J, C), write(C); write('.')), J1 is J+1, imprimir_fila(I, J1, M).

%###############################################
%#############Logica del programa###############
%###############################################

% Predicado que es llamado desde la GUI
resuelve(Nivel, tablero(I,J,C)):- game(Nivel, N, M), ciclo(N, M, 0), tablero(I,J,C).

% Procedimiento para determinar si el tablero esta completo
completo(0, 0, _).
completo(I, 0, M):- tablero(I, 0, _), I1 is I-1, completo(I1, M, M).
completo(I, J, M):- tablero(I, J, _), J1 is J-1, completo(I, J1, M).

% Si se ha aplicado fuerza bruta se debe verificar la validez del tablero
verifica_validez(_).

% Ciclo por el que se itera cada vez que se ha aplicado una regla y ha
% desembocado en al menos un cambio en el tablero
ciclo(N, M, FB):- N1 is N-1, M1 is M-1, not(completo(N1, M1, M1)), aplicar_reglas(N, M, FB).
ciclo(_, _, 0). % No se aplico fuerza bruta, el ciclo puede terminar
ciclo(_, _, 1):- verifica_validez(sarasa). % Se aplico fuerza bruta, se debe verificar validez del tablero

siguiente_celda(N, M, N, M, _, _, 1):- fail. % Fuera de la matriz
siguiente_celda(N, M, I, M, I1, 0, 0):- I < N, I1 is I+1. % Ultima columna
siguiente_celda(N, M, N, J, N, J1, 0):- J < M, J1 is J+1. % Ultima fila
siguiente_celda(N, M, I, J, I, J1, 0):- I < N, J < M, J1 is J+1. % Dentro de la matriz

% Aplicacion de cada una de las reglas de resolucion del juego
aplicar_reglas(N, M, FB):- regla1(0, 0, N, M, Cambio), Cambio == 1, ciclo(N, M, FB).
aplicar_reglas(N, M, FB):- regla2(Cambio), Cambio == 1, ciclo(N, M, FB).
aplicar_reglas(N, M, FB):- regla3(Cambio), Cambio == 1, ciclo(N, M, FB).
aplicar_reglas(N, M, FB):- regla4(Cambio), Cambio == 1, ciclo(N, M, FB).
aplicar_reglas(N, M, FB):- regla5(Cambio), Cambio == 1, ciclo(N, M, FB).
aplicar_reglas(N, M, _):- fuerza_bruta(), ciclo(N, M, 1).

regla1(I, J, N, M, C):- N1 is N-1,
                        M1 is M-1,
                        siguiente_celda(N1, M1, I, J, I1, J1, T),
                        elige_caso(I1, J1, N, M, C, T).
regla1(_, _, _, _, _). % Se llega a esta clausula cuando siguiente_celda da falso

elige_caso(_, _, _, _, _, 1).
elige_caso(I, J, N1, M1, C, 0):- tablero(I, J, _),
                                 celda_color(I, J, N1, M1, C),
                                 regla1(I, J, N1, M1, C).
elige_caso(I, J, C, N1, M1, 0):- celda_vacia(), regla1(I, J, N1, M1, C).

celda_color(I, J, I, J, _). % Ultima celda (N1, M1)
celda_color(I, J, I, M1, _):- J =:= M1-1. % (N1, M1-1)
celda_color(I, J, N1, J, _):- I =:= N1-1. % (N1-1, M1)
celda_color(I, J, N1, M1, _):- I =:= N1-1, J =:= M1-1. % (N1-1, M1-1)
% Ultima columna
celda_color(I, J, _, J, C):- miro_abajo(I, J, C).
% Penultima columna
celda_color(I, J, _, M1, C):- J =:= M1-1, miro_abajo(I, J, C).
% ULtima fila
celda_color(I, J, I, _, C):- miro_derecha(I, J, C).
% Penultima fila
celda_color(I, J, N1, _, C):- I =:= N1-1, miro_derecha(I, J, C).
% Celda que no es un caso especial
celda_color(I, J, _, _, C):- miro_abajo(I, J, C), miro_derecha(I, J, C).

miro_abajo(I, J, C):- tablero(I, J, Color),
                      I1 is I+1,
                      tablero(I1, J, Color),
                      I2 is I1+1,
                      not(tablero(I2, J, _)),
                      C is 1,
                      (Color == x, assert(tablero(I2, J, o)); assert(tablero(I2, J, x))),
                      imprimir(0, 8, 8), read(X).

miro_derecha(I, J, C):- tablero(I, J, Color),
                        I1 is I+1,
                        tablero(I1, J, Color),
                        I2 is I1+1,
                        not(tablero(I2, J, _)),
                        C is 1,
                        (Color == x, assert(tablero(I2, J, o)); assert(tablero(I2, J, x))),
                        imprimir(0, 8, 8), read(X).





regla2(1).
regla3(1).
regla4(1).
regla5(1).
fuerza_bruta().

%###############################################
%############Niveles del juego##################
%###############################################

game(1, 6, 6).

game(2, 8, 8):- assert(tablero(0,1,o)),
                assert(tablero(0,6,x)),
                assert(tablero(1,7,o)),
                assert(tablero(2,0,x)),
                assert(tablero(2,4,x)),
                assert(tablero(3,0,x)),
                assert(tablero(3,3,x)),
                assert(tablero(3,6,x)),
                assert(tablero(4,6,x)),
                assert(tablero(5,1,o)),
                assert(tablero(5,7,o)),
                assert(tablero(6,2,x)),
                assert(tablero(7,4,o)),
                assert(tablero(7,7,o)).

game(3, 10, 10).

game(4, 14, 14).

game(5, 20, 20).

% tablero(0,1,o).
% tablero(0,6,x).
% tablero(1,7,o).
% tablero(2,0,x).
% tablero(2,4,x).
% tablero(3,0,x).
% tablero(3,3,x).
% tablero(3,6,x).
% tablero(4,6,x).
% tablero(5,1,o).
% tablero(5,7,o).
% tablero(6,2,x).
% tablero(7,4,o).
% tablero(7,7,o).
