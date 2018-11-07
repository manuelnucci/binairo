:- dynamic tablero/1.

%###############################################
%################GUI temporal###################
%###############################################

siguiente(X, Y):- X < 5, Y is X+1.
siguiente(X, Y):- X1 is X+1, siguiente(X1, Y).

imprimir(I, N, _):- I > N. % Termine de imprimir el tablero
imprimir(I, N, M):- write('                             '),
                    imprimir_fila(I, 0, M),
                    I1 is I+1,
                    imprimir(I1, N, M).

imprimir_fila(_, J, M):- J > M, nl.
imprimir_fila(I, J, M):- (tablero(I, J, C), write(' '), write(C), write(' '); write(' . ')),
                         J1 is J+1,
                         imprimir_fila(I, J1, M).

depurar(Texto, N, M):- write(Texto), nl, imprimir(0, N, M), read(X), X \== q.

%###############################################
%#############Logica del programa###############
%###############################################

% Predicado que es llamado desde la GUI
resuelve(Nivel, L):- retractall(tablero(I, J, C)), % Eliminamos de la memoria cualquier contenido dinámico que
                                                                % hubiese quedado
                     game(Nivel, N, M),
                     % depurar('Tablero inicial', N, M),
                     ciclo(N, M, 0),
                     findall((I,J,C), tablero(I,J,C), L).
                     % lista_solucion(0, N, L).

% Procedimiento que genera la lista con todas las celdas de la matriz
% lista_solucion(I, N, [I,J,C|L]):- I < N, tablero(I,J,C),
% lista_solucion(L).
% lista_solucion([]).

% Procedimiento para determinar si el tablero esta completo
completo(0, 0, _).
completo(I, 0, M):- tablero(I, 0, _), I1 is I-1, completo(I1, M, M).
completo(I, J, M):- tablero(I, J, _), J1 is J-1, completo(I, J1, M).

% Si se ha aplicado fuerza bruta se debe verificar la validez del tablero
verifica_validez(_).

% Ciclo por el que se itera cada vez que se ha aplicado una regla y ha
% desembocado en al menos un cambio en el tablero
ciclo(N, M, FB):- not(completo(N, M, M)), aplicar_reglas(N, M, FB).
ciclo(_, _, 0). % No se aplico fuerza bruta, el ciclo puede terminar
ciclo(_, _, 1):- verifica_validez(sarasa). % Se aplico fuerza bruta, se debe verificar validez del tablero

siguiente_celda(N, M, N, M, _, _):- fail. % Fuera de la matriz
siguiente_celda(N, M, I, M, I1, 0):- I < N, I1 is I+1. % Ultima columna
siguiente_celda(N, M, N, J, N, J1):- J < M, J1 is J+1. % Ultima fila
siguiente_celda(N, M, I, J, I, J1):- I < N, J < M, J1 is J+1. % Dentro de la matriz

% Aplicacion de cada una de las reglas de resolucion del juego
aplicar_reglas(N, M, FB):- reglas1y2(0, -1, N, M, Cambio), Cambio == 1, ciclo(N, M, FB).
aplicar_reglas(N, M, FB):- regla3_fila(0, N, Cambio), Cambio == 1, ciclo(N, M, FB).
aplicar_reglas(N, M, FB):- regla3_columna(0, M, Cambio), Cambio == 1, ciclo(N, M, FB).
% aplicar_reglas(N, M, FB):- regla4(Cambio), Cambio == 1, ciclo(N, M,
% FB).
% aplicar_reglas(N, M, FB):- regla5(Cambio), Cambio == 1, ciclo(N, M,
% FB).
% aplicar_reglas(N, M, _):- fuerza_bruta(), ciclo(N, M, 1).

%###############################################
%##############  Reglas 1 y 2  #################
%###############################################

reglas1y2(I, J, N, M, Cambio):- siguiente_celda(N, M, I, J, I1, J1),
                                elige_caso(I1, J1, N, M, Cambio),
                                !, reglas1y2(I1, J1, N, M, Cambio).
reglas1y2(_, _, _, _, _). % Se llega a esta clausula cuando siguiente_celda da falso

elige_caso(I, J, N, M, Cambio):- (tablero(I, J, _), celda_color(I, J, N, M, Cambio); celda_vacia(I, J, N, M, Cambio)).
elige_caso(_, _, _, _, _).

celda_color(I, J, I, J, _). % Ultima celda (N, M)
celda_color(I, J, I, M, _):- J =:= M-1. % (N, M-1)
celda_color(I, J, N, J, _):- I =:= N-1. % (N-1, M)
celda_color(I, J, N, M, _):- I =:= N-1, J =:= M-1. % (N-1, M-1)
% Ultima columna
celda_color(I, J, _, J, Cambio):- miro_abajo_color(I, J, Cambio).
% Penultima columna
celda_color(I, J, _, M, Cambio):- J =:= M-1, miro_abajo_color(I, J, Cambio).
% Ultima fila
celda_color(I, J, I, _, Cambio):- miro_derecha_color(I, J, Cambio).
% Penultima fila
celda_color(I, J, N, _, Cambio):- I =:= N-1, miro_derecha_color(I, J, Cambio).
% Celda que no es un caso especial
celda_color(I, J, _, _, Cambio):- miro_derecha_color(I, J, Cambio), miro_abajo_color(I, J, Cambio).

miro_abajo_color(I, J, Cambio):- tablero(I, J, Color), % Regla 1
                                 I1 is I+1,
                                 tablero(I1, J, Color),
                                 I2 is I1+1,
                                 not(tablero(I2, J, _)),
                                 Cambio is 1,
                                 (Color == x, assert(tablero(I2, J, o)); assert(tablero(I2, J, x))),
                                 depurar('Regla 1', 7, 7).
miro_abajo_color(I, J, Cambio):- tablero(I, J, Color), % Regla 2
                                 I2 is I+2,
                                 tablero(I2, J, Color),
                                 I1 is I+1,
                                 not(tablero(I1, J, _)),
                                 Cambio is 1,
                                 (Color == x, assert(tablero(I1, J, o)); assert(tablero(I1, J, x))),
                                 depurar('Regla 2', 7, 7).
miro_abajo_color(_, _, _).

miro_derecha_color(I, J, Cambio):- tablero(I, J, Color), % Regla 1
                                   J1 is J+1,
                                   tablero(I, J1, Color),
                                   J2 is J1+1,
                                   not(tablero(I, J2, _)),
                                   Cambio is 1,
                                   (Color == x, assert(tablero(I, J2, o)); assert(tablero(I, J2, x))),
                                   depurar('Regla 1', 7, 7).
miro_derecha_color(I, J, Cambio):- tablero(I, J, Color), % Regla 2
                                   J2 is J+2,
                                   tablero(I, J2, Color),
                                   J1 is J+1,
                                   not(tablero(I, J1, _)),
                                   Cambio is 1,
                                   (Color == x, assert(tablero(I, J1, o)); assert(tablero(I, J1, x))),
                                   depurar('Regla 2', 7, 7).
miro_derecha_color(_, _, _).

celda_vacia(I, J, I, J, _). % Ultima celda (N, M)
celda_vacia(I, J, I, M, _):- J =:= M-1. % (N, M-1)
celda_vacia(I, J, N, J, _):- I =:= N-1. % (N-1, M)
celda_vacia(I, J, N, M, _):- I =:= N-1, J =:= M-1. % (N-1, M-1)
% Ultima columna
celda_vacia(I, J, N, J, Cambio):- miro_abajo_vacia(I, J, N, J, Cambio).
% Penultima columna
celda_vacia(I, J, N, M, Cambio):- J =:= M-1, miro_abajo_vacia(I, J, N, M, Cambio).
% Ultima fila
celda_vacia(I, J, I, M, Cambio):- miro_derecha_vacia(I, J, I, M, Cambio).
% Penultima fila
celda_vacia(I, J, N, M, Cambio):- I =:= N-1, miro_derecha_vacia(I, J, N, M, Cambio).
% Celda que no es un caso especial
celda_vacia(I, J, N, M, Cambio):- miro_derecha_vacia(I, J, N, M, Cambio), miro_abajo_vacia(I, J, N, M, Cambio).

miro_abajo_vacia(I, J, N, M, Cambio):- not(tablero(I, J, _)), % Debo asegurarme que en el caso que sea una celda no especial
                                                                % siga estando vacia luego de haber pasado por miro_abajo_vacia
                                       I1 is I+1,
                                       tablero(I1, J, Color),
                                       I2 is I1+1,
                                       tablero(I2, J, Color),
                                       Cambio is 1,
                                       (Color == x, assert(tablero(I, J, o)); assert(tablero(I, J, x))),
                                       depurar('Regla 1', N, M),
                                       celda_color(I, J, N, M, Cambio). % La celda tiene un color ahora, llamo a celda_color
miro_abajo_vacia(_, _, _, _, _).

miro_derecha_vacia(I, J, N, M, Cambio):- J1 is J+1,
                                         tablero(I, J1, Color),
                                         J2 is J1+1,
                                         tablero(I, J2, Color),
                                         Cambio is 1,
                                         (Color == x, assert(tablero(I, J, o)); assert(tablero(I, J, x))),
                                         depurar('Regla 1', N, M),
                                         celda_color(I, J, N, M, Cambio).
miro_derecha_vacia(_, _, _, _, _).

%###############################################
%################   Regla 3  ###################
%###############################################

depurarF_regla3(I, X, O):- write('Fila '), depurar_regla3(I, X, O).
depurarC_regla3(I, X, O):- write('Columna '), depurar_regla3(I, X, O).
depurar_regla3(I, X, O):- write(I), nl, write('Negros: '), write(X), nl, write('Blancas: '), write(O), nl.

regla3_fila(I, N, _):- I > N, !. % Caso base
regla3_fila(I, N, Cambio):- analizar_fila(I, 0, N, X, O),
                            depurarF_regla3(I, X, O),
                            N1 is N+1,
                            (not(N1 is X+O), ((X is N1/2, completa_fila(I, 0, N, o));
                                             (O is N1/2, completa_fila(I, 0, N, x)))),
                            depurar('Regla 3', N, N),
                            Cambio is 1.
regla3_fila(I, N, Cambio):- I1 is I+1, I1 =< N, regla3_fila(I1, N, Cambio).

analizar_fila(_, J, N, 0, 0):- J > N. % Caso base
analizar_fila(I, J, N, X, O):- J1 is J+1,
                               analizar_fila(I, J1, N, X1, O1),
                               ((tablero(I, J, C), ((C == x, X is X1+1, O is O1); (X is X1, O is O1+1)));
                               (X is X1, O is O1)), !.

completa_fila(_, J, N, _):- J =:= N+1. % Caso base, termine la fila
completa_fila(I, J, N, S):- J1 is J+1,
                            (tablero(I, J, _); assert(tablero(I, J, S))),
                            completa_fila(I, J1, N, S).

regla3_columna(J, M, _):- J > M, !. % Caso base
regla3_columna(J, M, Cambio):- analizar_columna(0, J, M, X, O),
                               depurarC_regla3(J, X, O),
                               M1 is M+1,
                               (not(M1 is X+O), ((X is M1/2, completa_columna(0, J, M, o));
                                                (O is M1/2, completa_columna(0, J, M, x)))),
                               depurar('Regla 3', M, M),
                               Cambio is 1.
regla3_columna(J, M, Cambio):- J1 is J+1, J1 =< M, regla3_columna(J1, M, Cambio).

analizar_columna(I, _, M, 0, 0):- I > M. % Caso base
analizar_columna(I, J, M, X, O):- I1 is I+1,
                                  analizar_columna(I1, J, M, X1, O1),
                                  ((tablero(I, J, C), ((C == x, X is X1+1, O is O1); (X is X1, O is O1+1)));
                                  (X is X1, O is O1)), !.

completa_columna(I, _, M, _):- I =:= M+1. % Caso base, termine la columna
completa_columna(I, J, N, S):- I1 is I+1,
                               (tablero(I, J, _); assert(tablero(I, J, S))),
                               completa_columna(I1, J, N, S).

%################################################
%##################   Regla 4  ##################
%################################################

regla4(1).
regla5(1).
fuerza_bruta().

%################################################
%#############  Niveles del juego  ##############
%################################################

% Matriz de 6x6
game(1, 5, 5).

% Matriz de 8x8
game(2, 7, 7):- assert(tablero(0,1,o)),
                assert(tablero(0,3,x)),
                assert(tablero(0,6,x)),
                assert(tablero(0,7,x)),
                assert(tablero(1,2,x)),
                assert(tablero(1,7,o)),
                assert(tablero(2,0,x)),
                assert(tablero(2,2,x)),
                assert(tablero(2,4,x)),
                assert(tablero(3,0,x)),
                assert(tablero(3,3,x)),
                assert(tablero(3,5,x)),
                assert(tablero(3,6,x)),
                assert(tablero(4,2,o)),
                assert(tablero(4,3,o)),
                assert(tablero(4,6,x)),
                assert(tablero(5,1,o)),
                assert(tablero(5,2,x)),
                assert(tablero(5,7,o)),
                assert(tablero(6,1,x)),
                assert(tablero(6,2,x)),
                assert(tablero(6,5,o)),
                assert(tablero(6,6,o)),
                assert(tablero(7,4,o)),
                assert(tablero(7,7,o)).

% Matriz de 10x10
game(3, 9, 9).

% Matriz de 14x14
game(4, 13, 13).

% Matriz de 20x20
game(5, 19, 19).

game(6, 7, 7):- assert(tablero(0,1,o)),
                assert(tablero(0,3,x)),
                assert(tablero(0,6,x)),
                assert(tablero(1,1,o)),
                assert(tablero(1,7,o)),
                assert(tablero(1,7,o)),
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
