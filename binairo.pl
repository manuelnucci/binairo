﻿:- dynamic tablero/1.

%###############################################
%#############   GUI temporal   ################
%###############################################

siguiente(X, Y):- X < 5, Y is X+1.
siguiente(X, Y):- X1 is X+1, siguiente(X1, Y).

depurar(_,_,_).
depurar2(Texto, N, M):- write(Texto), nl, imprimir(0, N, M), read(X), X \== q.

imprimir(I, N, _):- I > N. % Termine de imprimir el tablero
imprimir(I, N, M):- write('                             '),
                    imprimir_fila(I, 0, M),
                    I1 is I+1,
                    imprimir(I1, N, M).

imprimir_fila(_, J, M):- J > M, nl.
imprimir_fila(I, J, M):- (tablero(I, J, C), write(' '), write(C), write(' '); write(' . ')),
                         J1 is J+1,
                         imprimir_fila(I, J1, M).

%###############################################
%##########   Logica del programa   ############
%###############################################

% Predicado que es llamado desde la GUI
resuelve(Nivel, [N, M, CI |L]):- retractall(tablero(I, J, C)), % Eliminamos de la memoria cualquier contenido dinamico que
                                                   % hubiese quedado
                     game(Nivel, N, M, CI), % CI = Celdas Iniciales
                     % depurar2('Tablero inicial', N, M),
                     ciclo(N, M, 0),
                     % depurar2('Tablero final', N, M),
                     findall((I,J,C), tablero(I,J,C), L), !.
                     % lista_solucion(0, N, L).

% Procedimiento que genera la lista con todas las celdas de la matriz
% lista_solucion(I, N, [I,J,C|L]):- I < N, tablero(I,J,C),
% lista_solucion(L).
% lista_solucion([]).

% Procedimiento para determinar si el tablero esta completo
completo(0, 0, _).
completo(I, 0, M):- tablero(I, 0, _), I1 is I-1, completo(I1, M, M).
completo(I, J, M):- tablero(I, J, _), J1 is J-1, completo(I, J1, M), !.

% Si se ha aplicado fuerza bruta se debe verificar la validez del tablero
% verifica_validez se fija solamente que toda fila y columna sea
% diferente a las demás, NO controla cantidad de colores por fila o
% columna
verifica_validez(N):- arma_filas(0, N, F), distintas(F), arma_columnas(0, N, C), distintas(C).

arma_filas(I, N, []):- I > N.
arma_filas(I, N, [F|Filas]):- arma_fila(I, 0, N, F), I1 is I+1, arma_filas(I1, N, Filas).

arma_fila(_, J, N, []):- J > N.
arma_fila(I, J, N, [C|Colores]) :- tablero(I, J, C), J1 is J+1, arma_fila(I, J1, N, Colores).

arma_columnas(J, M, []):- J > M.
arma_columnas(J, M, [C|Columnas]):- arma_columna(0, J, M, C), J1 is J+1, arma_columnas(J1, M, Columnas).

arma_columna(I, _, M, []):- I > M.
arma_columna(I, J, M, [C|Colores]) :- tablero(I, J, C), I1 is I+1, arma_columna(I1, J, M, Colores).

distintas([_]).
distintas(LL):- select(L, LL, L1), distinto_de_todos(L, L1), distintas(L1).

distinto_de_todos(_, []).
distinto_de_todos(L, [Cab|Cola]):- not(distinto(L, Cab)), distinto_de_todos(L, Cola).

distinto([Cab], [Cab]).
distinto([Cab|Cola1], [Cab|Cola2]):- distinto(Cola1, Cola2).

% Ciclo por el que se itera cada vez que se ha aplicado una regla y ha
% desembocado en al menos un cambio en el tablero
ciclo(N, M, FB):- not(completo(N, M, M)), aplicar_reglas(N, M, FB).
ciclo(N, _, 0):- verifica_validez(N). % No se aplico fuerza bruta, el ciclo puede terminar
                 % write('Se verifica la validez del tablero'), nl.
ciclo(N, _, 1):- verifica_validez(N). % Se aplico fuerza bruta, se debe verificar validez del tablero
                 % write('Se verifica la validez del tablero habiendose usado fuerza bruta'), nl.

siguiente_celda(N, M, N, M, _, _):- fail. % Fuera de la matriz
siguiente_celda(N, M, I, M, I1, 0):- I < N, I1 is I+1. % Ultima columna
siguiente_celda(N, M, N, J, N, J1):- J < M, J1 is J+1. % Ultima fila
siguiente_celda(N, M, I, J, I, J1):- I < N, J < M, J1 is J+1. % Dentro de la matriz

% Aplicacion de cada una de las reglas de resolucion del juego
aplicar_reglas(N, M, FB):- reglas1y2(0, -1, N, M, Cambio), Cambio == 1, ciclo(N, M, FB).
aplicar_reglas(N, M, FB):- regla3_fila(0, N, Cambio), Cambio == 1, ciclo(N, M, FB).
aplicar_reglas(N, M, FB):- regla3_columna(0, M, Cambio), Cambio == 1, ciclo(N, M, FB).
aplicar_reglas(N, M, FB):- regla4_fila(0, N, Cambio), Cambio == 1, ciclo(N, M, FB).
aplicar_reglas(N, M, FB):- regla4_columna(0, M, Cambio), Cambio == 1, ciclo(N, M, FB).
aplicar_reglas(N, M, FB):- regla5_fila(N, Cambio), Cambio == 1, ciclo(N, M, FB).
aplicar_reglas(N, M, FB):- regla5_columna(M, Cambio), Cambio == 1, ciclo(N, M, FB).
% aplicar_reglas(N, M, _):- fuerza_bruta(), ciclo(N, M, 1).

%###############################################
%##############  Reglas 1 y 2  #################
%###############################################

reglas1y2(I, J, N, M, Cambio):- siguiente_celda(N, M, I, J, I1, J1),
                                elige_caso(I1, J1, N, M, Cambio),
                                reglas1y2(I1, J1, N, M, Cambio), !.
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
                                 (Color == x, assertz(tablero(I2, J, o)); assertz(tablero(I2, J, x))),
                                 depurar('Regla 1', 19, 19).
miro_abajo_color(I, J, Cambio):- tablero(I, J, Color), % Regla 2
                                 I2 is I+2,
                                 tablero(I2, J, Color),
                                 I1 is I+1,
                                 not(tablero(I1, J, _)),
                                 Cambio is 1,
                                 (Color == x, assertz(tablero(I1, J, o)); assertz(tablero(I1, J, x))),
                                 depurar('Regla 2', 19, 19).
miro_abajo_color(_, _, _).

miro_derecha_color(I, J, Cambio):- tablero(I, J, Color), % Regla 1
                                   J1 is J+1,
                                   tablero(I, J1, Color),
                                   J2 is J1+1,
                                   not(tablero(I, J2, _)),
                                   Cambio is 1,
                                   (Color == x, assertz(tablero(I, J2, o)); assertz(tablero(I, J2, x))),
                                   depurar('Regla 1', 19, 19).
miro_derecha_color(I, J, Cambio):- tablero(I, J, Color), % Regla 2
                                   J2 is J+2,
                                   tablero(I, J2, Color),
                                   J1 is J+1,
                                   not(tablero(I, J1, _)),
                                   Cambio is 1,
                                   (Color == x, assertz(tablero(I, J1, o)); assertz(tablero(I, J1, x))),
                                   depurar('Regla 2', 19, 19).
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
                                       (Color == x, assertz(tablero(I, J, o)); assertz(tablero(I, J, x))),
                                       depurar('Regla 1', N, M),
                                       celda_color(I, J, N, M, Cambio). % La celda tiene un color ahora, llamo a celda_color
miro_abajo_vacia(_, _, _, _, _).

miro_derecha_vacia(I, J, N, M, Cambio):- J1 is J+1,
                                         tablero(I, J1, Color),
                                         J2 is J1+1,
                                         tablero(I, J2, Color),
                                         Cambio is 1,
                                         (Color == x, assertz(tablero(I, J, o)); assertz(tablero(I, J, x))),
                                         depurar('Regla 1', N, M),
                                         celda_color(I, J, N, M, Cambio).
miro_derecha_vacia(_, _, _, _, _).

%###############################################
%################   Regla 3  ###################
%###############################################

%depurarF_regla3(I, X, O):- write('Fila '), %depurar_regla3(I, X, O).
%depurarC_regla3(I, X, O):- write('Columna '), %depurar_regla3(I, X, O).
%depurar_regla3(I, X, O):- write(I), nl, write('Negros: '), write(X), nl, write('Blancas: '), write(O), nl.

regla3_fila(I, N, _):- I > N. % Caso base
regla3_fila(I, N, Cambio):- analizar_fila(I, 0, N, X, O),
                            % %depurarF_regla3(I, X, O),
                            N1 is N+1,
                            not(N1 is X+O),
                            ((X is N1/2, Color = o); (O is N1/2, Color = x)),
                            completa_fila(I, 0, N, Color),
                            depurar('Regla 3 Fila', N, N),
                            Cambio is 1.
regla3_fila(I, N, Cambio):- I1 is I+1, regla3_fila(I1, N, Cambio), !.

analizar_fila(_, J, N, 0, 0):- J > N. % Caso base
analizar_fila(I, J, N, X, O):- J1 is J+1,
                               analizar_fila(I, J1, N, X1, O1),
                               ((tablero(I, J, C), ((C == x, X is X1+1, O is O1); (X is X1, O is O1+1)));
                               (X is X1, O is O1)), !.

completa_fila(_, J, N, _):- J > N. % Caso base, termine la fila
completa_fila(I, J, N, S):- J1 is J+1,
                            (tablero(I, J, _); assertz(tablero(I, J, S))),
                            completa_fila(I, J1, N, S).

regla3_columna(J, M, _):- J > M. % Caso base
regla3_columna(J, M, Cambio):- analizar_columna(0, J, M, X, O),
                               % %depurarC_regla3(J, X, O),
                               M1 is M+1,
                               not(M1 is X+O),
                               ((X is M1/2, Color = o); (O is M1/2, Color = x)),
                               completa_columna(0, J, M, Color),
                               depurar('Regla 3 Columna', M, M),
                               Cambio is 1.
regla3_columna(J, M, Cambio):- J1 is J+1, regla3_columna(J1, M, Cambio), !.

analizar_columna(I, _, M, 0, 0):- I > M. % Caso base
analizar_columna(I, J, M, X, O):- I1 is I+1,
                                  analizar_columna(I1, J, M, X1, O1),
                                  ((tablero(I, J, C), ((C == x, X is X1+1, O is O1); (X is X1, O is O1+1)));
                                  (X is X1, O is O1)), !.

completa_columna(I, _, M, _):- I > M. % Caso base, termine la columna
completa_columna(I, J, M, S):- I1 is I+1,
                               (tablero(I, J, _); assertz(tablero(I, J, S))),
                               completa_columna(I1, J, M, S).

%################################################
%#################   Regla 4   ##################
%################################################

%%###############################################
%###############   Regla 4 Fila   ###############
%################################################

regla4_fila(I, N, _):- I > N. % Caso base
regla4_fila(I, N, Cambio):- analizar_fila(I, 0, N, X, O),
                            Y is ((N+1)/2)-1, % Mitad de la fila menos 1
                            ((X =:= Y, A is (N+1)/2-O, A >= 2, C = o, R = A); % Nos aseguramos que "falten 2 o mas de B"
                                                                              % A = Blancos que faltan
                            (O =:= Y, B is (N+1)/2-X, B >= 2, C = x, R = B)), % B = Negros que faltan
                            armar_lista_posiciones_fila(I, 0, N, L),
                            adfpc(I, N, L, C, Cambio, R),
                            depurar('Regla 4 Fila', N, N).
regla4_fila(I, N, Cambio):- I1 is I+1, regla4_fila(I1, N, Cambio), !.

% Recorremos las columnas
armar_lista_posiciones_fila(_, J, N, []):- J > N.
armar_lista_posiciones_fila(I, J, N, [J|L1]):- tablero(I, J, _),
                                               J1 is J+1,
                                               armar_lista_posiciones_fila(I, J1, N, L1), !.
armar_lista_posiciones_fila(I, J, N, L1):- J1 is J+1,
                                           armar_lista_posiciones_fila(I, J1, N, L1), !.

completar_fila(_, J, N, _, _):- J > N.
completar_fila(I, J, N, L, C):- (tablero(I, J, _); (not(member(J, L)), assertz(tablero(I, J, C))); true),
                                J1 is J+1,
                                completar_fila(I, J1, N, L, C).

% analizar_distancias_fila_primer_caso
adfpc(I, N, [0|Cola], Color, Cambio, R):- adfem(I, N, [0|Cola], Color, Cambio, R).
adfpc(I, N, [X|_], Color, Cambio, _):- X == 1, % Segundo elemento con uno libre a la izquierda
                                       not(tablero(I, 2, _)),
                                       tablero(I, X, Color), % Importa el color
                                       completar_fila(I, 1, N, [0, 2], Color),
                                       Cambio is 1.
adfpc(I, N, [X|_], Color, Cambio, _):- X >= 2, % Tercer elemento con dos o mas libres a su izquierda
                                       tablero(I, X, Color), % Importa el color
                                       X1 is X-1,
                                       X2 is X-2,
                                       completar_fila(I, 0, N, [X2, X1], Color),
                                       Cambio is 1.
adfpc(I, N, [X|_], Color, Cambio, R):- X == 3, % Cuarto elemento con 3 libres a la izquierda,
                                               % no importa el color
                                       R >= 3,
                                       completar_fila(I, 0, N, [0, 1, 2], Color),
                                       Cambio is 1.
adfpc(I, N, [X|_], Color, Cambio, _):- X == 4, % Quinto elemento con 4 libres a la izquierda,
                                               % no importa el color
                                       completar_fila(I, 0, N, [1, 2], Color),
                                       Cambio is 1.
adfpc(I, N, [X|_], Color, Cambio, _):- X == 5, % Sexto elemento con 5 libres a la izquierda,
                                               % no importa el color
                                       completar_fila(I, 0, N, [2], Color),
                                       (Color == x, Opuesto = o; Opuesto = x),
                                       assertz(tablero(I, 2, Opuesto)),
                                       Cambio is 1.
% No avanzamos en la lista, seguimos en el mismo elemento para poder
% comparar las diferencias con el de la derecha
adfpc(I, N, L, Color, Cambio, R):- adfem(I, N, L, Color, Cambio, R).

% analizar_distancias_fila_entre_medio
adfem(I, N, [X], Color, Cambio, R):- adfuc(I, N, [X], Color, Cambio, R).
adfem(I, N, [X, Y|_], Color, Cambio, _):- X >= 1, % Me aseguro que puedo mirar una celda a la izquierda
                                          2 =:= Y-X, % Espacio libre a la derecha
                                          X1 is X-1,
                                          not(tablero(I, X1, _)), % Espacio libre a la izquierda
                                          tablero(I, X, Color), % Importa el color
                                          X2 is X+1,
                                          completar_fila(I, 0, N, [X1, X2], Color),
                                          Cambio is 1.
adfem(I, N, [X, Y|_], Color, Cambio, _):- D is Y-X,
                                          D > 2, % 2 o mas espacios libres a la derecha
                                          % LA COMPARACION DEBE SER POR >, NO POR >=
                                          tablero(I, X, Color), % Importa el color
                                          X1 is X+1,
                                          X2 is X+2,
                                          completar_fila(I, 0, N, [X1, X2], Color),
                                          Cambio is 1.
adfem(I, N, [X, _|_], Color, Cambio, _):- X >= 2, % Me aseguro que puedo mirar dos celdas a la izquierda
                                          X1 is X-1, % Miro 2 lugares a la izquierda, sin aprovechar la lista
                                          not(tablero(I, X1, _)),
                                          X2 is X-2,
                                          not(tablero(I, X2, _)),
                                          tablero(I, X, Color), % Importa el color
                                          completar_fila(I, 0, N, [X2, X1], Color),
                                          Cambio is 1.
adfem(I, N, [X, Y|_], Color, Cambio, R):- 4 =:= Y-X, % 3 espacios libres
                                          R >= 3,
                                          X1 is X+1, X2 is X+2, X3 is X+3,
                                          completar_fila(I, 0, N, [X1, X2, X3] , Color),
                                          Cambio is 1.
adfem(I, N, [X, Y|_], Color, Cambio, _):- 5 =:= Y-X, % 4 espacios libres
                                          X2 is X+2, X3 is X+3,
                                          completar_fila(I, 0, N, [X2, X3] , Color),
                                          Cambio is 1.
adfem(I, N, [X, Y|_], Color, Cambio, _):- 6 =:= Y-X, % 5 espacios libres
                                          X3 is X+3,
                                          completar_fila(I, 0, N, [X3] , Color),
                                          (Color == x, Opuesto = o; Opuesto = x),
                                          assertz(tablero(I, X3, Opuesto)),
                                          Cambio is 1.
adfem(I, N, [_, Y|Cola], Color, Cambio, R):- adfem(I, N, [Y|Cola], Color, Cambio, R).

% analizar_distancias_fila_ultimo_caso
adfuc(_, N, [N], _, _, _):- fail. % No hay espacios libres despues del ultimo elemento
adfuc(I, N, [X], Color, Cambio, _):- 1 is N-X, % 1 espacio libre a la derecha
                                     X1 is X-1,
                                     not(tablero(I, X1, _)),
                                     tablero(I, X, Color), % Importa el color
                                     completar_fila(I, 0, N, [X1, N], Color),
                                     Cambio is 1.
adfuc(I, N, [X], Color, Cambio, _):- D is N-X,
                                     D >= 2, % Ultimo elemento con dos o mas libres a su derecha
                                     % LA COMPARACION DEBE SER POR >=
                                     tablero(I, X, Color), % Importa el color
                                     X1 is X+1,
                                     completar_fila(I, 0, N, [X1, N], Color),
                                     Cambio is 1.
adfuc(I, N, [X], Color, Cambio, R):- 3 =:= N-X, % Ultimo elemento con 3 libres a la derecha,
                                                % no importa el color
                                     R >= 3,
                                     X1 is X+1,
                                     X2 is X+2,
                                     completar_fila(I, 0, N, [X1, X2, N], Color),
                                     Cambio is 1.
adfuc(I, N, [X], Color, Cambio, _):- 4 =:= N-X, % Ultimo elemento con 4 libres a la derecha,
                                                % no importa el color
                                     X2 is X+2,
                                     X3 is X+3,
                                     completar_fila(I, 0, N, [X2, X3], Color),
                                     Cambio is 1.
adfuc(I, N, [X], Color, Cambio, _):- 5 =:= N-X, % Ultimo elemento con 5 libres a la derecha,
                                                % no importa el color
                                     X3 is X+3,
                                     completar_fila(I, 0, N, [X3], Color),
                                     (Color == x, Opuesto = o; Opuesto = x),
                                     assertz(tablero(I, X3, Opuesto)),
                                     Cambio is 1.

%################################################
%##############  Regla 4 Columna  ###############
%################################################

regla4_columna(J, M, _):- J > M. % Caso base
regla4_columna(J, M, Cambio):- analizar_columna(0, J, M, X, O),
                               Y is ((M+1)/2)-1, % Mitad de la columna menos 1
                               ((X =:= Y, A is (M+1)/2-O, A >= 2, C = o, R = A); % Nos aseguramos que "falten 2 o mas de B"
                                                                                 % A = Blancos que faltan
                               (O =:= Y, B is (M+1)/2-X, B >= 2, C = x, R = B)), % B = Negros que faltan
                               armar_lista_posiciones_columna(0, J, M, L),
                               adcpc(J, M, L, C, Cambio, R),
                               depurar('Regla 4 Columna', M, M).
regla4_columna(J, M, Cambio):- J1 is J+1, regla4_columna(J1, M, Cambio), !.

% Recorremos las filas
armar_lista_posiciones_columna(I, _, M, []):- I > M.
armar_lista_posiciones_columna(I, J, M, [I|L1]):- tablero(I, J, _),
                                                  I1 is I+1,
                                                  armar_lista_posiciones_columna(I1, J, M, L1), !.
armar_lista_posiciones_columna(I, J, M, L1):- I1 is I+1,
                                              armar_lista_posiciones_columna(I1, J, M, L1), !.

completar_columna(I, _, M, _, _):- I > M.
completar_columna(I, J, M, L, C):- (tablero(I, J, _); (not(member(I, L)), assertz(tablero(I, J, C))); true),
                                   I1 is I+1,
                                   completar_columna(I1, J, M, L, C).

% analizar_distancias_columna_primer_caso
adcpc(J, M, [0|Cola], Color, Cambio, R):- adcem(J, M, [0|Cola], Color, Cambio, R).
adcpc(J, M, [X|_], Color, Cambio, _):- X == 1, % Segundo elemento con uno libre arriba
                                       not(tablero(2, J, _)),
                                       tablero(X, J, Color), % Importa el color
                                       completar_columna(1, J, M, [0, 2], Color),
                                       Cambio is 1.
adcpc(J, M, [X|_], Color, Cambio, _):- X >= 2, % Tercer elemento con dos o mas libres arriba
                                       tablero(X, J, Color), % Importa el color
                                       X1 is X-1,
                                       X2 is X-2,
                                       completar_columna(0, J, M, [X2, X1], Color),
                                       Cambio is 1.
adcpc(J, M, [X|_], Color, Cambio, R):- X == 3, % Cuarto elemento con 3 libres arriba,
                                               % no importa el color
                                       R >= 3,
                                       completar_columna(0, J, M, [0, 1, 2], Color),
                                       Cambio is 1.
adcpc(J, M, [X|_], Color, Cambio, _):- X == 4, % Quinto elemento con 4 libres arriba,
                                               % no importa el color
                                       completar_columna(0, J, M, [1, 2], Color),
                                       Cambio is 1.
adcpc(J, M, [X|_], Color, Cambio, _):- X == 5, % Sexto elemento con 5 libres arriba,
                                               % no importa el color
                                       completar_columna(0, J, M, [2], Color),
                                       (Color == x, Opuesto = o; Opuesto = x),
                                       assertz(tablero(2, J, Opuesto)),
                                       Cambio is 1.
% No avanzamos en la lista, seguimos en el mismo elemento para poder
% comparar las diferencias con el de abajo
adcpc(J, M, L, Color, Cambio, R):- adcem(J, M, L, Color, Cambio, R).

% analizar_distancias_columna_entre_medio
adcem(J, M, [X], Color, Cambio, R):- adcuc(J, M, [X], Color, Cambio, R).
adcem(J, M, [X, Y|_], Color, Cambio, _):- X >= 1, % Me aseguro que puedo mirar una celda arriba
                                          2 =:= Y-X, % Espacio libre abajo
                                          X1 is X-1,
                                          not(tablero(X1, J, _)), % Espacio libre abajo
                                          tablero(X, J, Color), % Importa el color
                                          X2 is X+1,
                                          completar_columna(0, J, M, [X1, X2], Color),
                                          Cambio is 1.
adcem(J, M, [X, Y|_], Color, Cambio, _):- D is Y-X,
                                          D > 2, % 2 o mas espacios libres abajo
                                          % LA COMPARACION DEBE SER POR >, NO POR >=
                                          tablero(X, J, Color), % Importa el color
                                          X1 is X+1,
                                          X2 is X+2,
                                          completar_columna(0, J, M, [X1, X2], Color),
                                          Cambio is 1.
adcem(J, M, [X, _|_], Color, Cambio, _):- X >= 2, % Me aseguro que puedo mirar dos celdas arriba
                                          X1 is X-1, % Miro 2 lugares arriba, sin aprovechar la lista
                                          not(tablero(X1, J, _)),
                                          X2 is X-2,
                                          not(tablero(X2, J, _)),
                                          tablero(X, J, Color), % Importa el color
                                          completar_columna(0, J, M, [X2, X1], Color),
                                          Cambio is 1.
adcem(J, M, [X, Y|_], Color, Cambio, R):- 4 =:= Y-X, % 3 espacios libres
                                          R >= 3,
                                          X1 is X+1, X2 is X+2, X3 is X+3,
                                          completar_columna(0, J, M, [X1, X2, X3] , Color),
                                          Cambio is 1.
adcem(J, M, [X, Y|_], Color, Cambio, _):- 5 =:= Y-X, % 4 espacios libres
                                          X2 is X+2, X3 is X+3,
                                          completar_columna(0, J, M, [X2, X3] , Color),
                                          Cambio is 1.
adcem(J, M, [X, Y|_], Color, Cambio, _):- 6 =:= Y-X, % 5 espacios libres
                                          X3 is X+3,
                                          completar_columna(0, J, M, [X3] , Color),
                                          (Color == x, Opuesto = o; Opuesto = x),
                                          assertz(tablero(X3, J, Opuesto)),
                                          Cambio is 1.
adcem(J, M, [_, Y|Cola], Color, Cambio, R):- adcem(J, M, [Y|Cola], Color, Cambio, R).

% analizar_distancias_columna_ultimo_caso
adcuc(_, M, [M], _, _, _):- fail. % No hay espacios libres despues del ultimo elemento
adcuc(J, M, [X], Color, Cambio, _):- 1 is M-X, % 1 espacio libre abajo
                                     X1 is X-1,
                                     not(tablero(X1, J, _)),
                                     tablero(X, J, Color), % Importa el color
                                     completar_columna(0, J, M, [X1, M], Color),
                                     Cambio is 1.
adcuc(J, M, [X], Color, Cambio, _):- D is M-X,
                                     D >= 2, % Ultimo elemento con dos o mas libres abajo
                                     % LA COMPARACION DEBE SER POR >=
                                     tablero(X, J, Color), % Importa el color
                                     X1 is X+1,
                                     completar_columna(0, J, M, [X1, M], Color),
                                     Cambio is 1.
adcuc(J, M, [X], Color, Cambio, R):- 3 =:= M-X, % Ultimo elemento con 3 libres abajo,
                                                % no importa el color
                                     R >= 3,
                                     X1 is X+1,
                                     X2 is X+2,
                                     completar_columna(0, J, M, [X1, X2, M], Color),
                                     Cambio is 1.
adcuc(J, M, [X], Color, Cambio, _):- 4 =:= M-X, % Ultimo elemento con 4 libres abajo,
                                                % no importa el color
                                     X2 is X+2,
                                     X3 is X+3,
                                     completar_columna(0, J, M, [X2, X3], Color),
                                     Cambio is 1.
adcuc(J, M, [X], Color, Cambio, _):- 5 =:= M-X, % Ultimo elemento con 5 libres abajo,
                                                % no importa el color
                                     X3 is X+3,
                                     completar_columna(0, J, M, [X3], Color),
                                     (Color == x, Opuesto = o; Opuesto = x),
                                     assertz(tablero(X3, J, Opuesto)),
                                     Cambio is 1.

%################################################
%#################   Regla 5   ##################
%################################################

regla5_fila(N, Cambio):- filas_comparables(0, N, FCL, FL), % FCL = Filas Casi Llenas, FL = Filas Llenas (N-2)
                         comparacion(FCL, FL, Cambio), (Cambio == 1, depurar('Regla 5 Fila', N, N); true), !.

filas_comparables(I, N, [], []):- I > N. % Caso base
filas_comparables(I, N, FCL, FL):- I1 is I+1,
                                   filas_comparables(I1, N, FCLR, FLR), % FCL Recursivo, FL Recursivo
                                   analizar_fila(I, 0, N, X, O),
                                   ((N-1 =:= X+O, armar_fila(I, 0, N, F1), FCL = [F1|FCLR], FL = FLR); % Fila Casi Llena (N-2)
                                   (N+1 =:= X+O, armar_fila(I, 0, N, F2), FCL = FCLR, FL = [F2|FLR]); % Fila Llena
                                   (FCL = FCLR, FL = FLR)).

armar_fila(_, J, N, []):- J > N.
armar_fila(I, J, N, [t(I, J, C)|F]):- J1 is J+1,
                                      armar_fila(I, J1, N, F),
                                      (tablero(I, J, C); C = v). % v = vacio

comparacion([], _, _). % No hay mas filas casi llenas por comparar
comparacion([FCL|FSCL], FL, Cambio):- comparacion_1_N(FCL, FL, Cambio); comparacion(FSCL, FL, Cambio).

comparacion_1_N(_, [], _):- fail. % No hay mas filas llenas con las que comparar
comparacion_1_N(FCL, [FL|_], Cambio):- similares(FCL, FL), completar_con_opuesto(FCL, FL), Cambio is 1.
comparacion_1_N(FCL, [_|FSL], Cambio):- comparacion_1_N(FCL, FSL, Cambio).

similares([], []).
similares([t(_, _, v)|Cola1], [_|Cola2]):- similares(Cola1, Cola2).
similares([t(_, _, C)|Cola1], [t(_, _, C)|Cola2]):- similares(Cola1, Cola2).

completar_con_opuesto([], []).
completar_con_opuesto([t(I, J, v)|Cola1], [t(_, _, Color)|Cola2]):- ((Color == x, C = o); C = x),
                                                                    assertz(tablero(I, J, C)),
                                                                    completar_con_opuesto(Cola1, Cola2).
completar_con_opuesto([_|Cola1], [_|Cola2]):- completar_con_opuesto(Cola1, Cola2).

regla5_columna(M, Cambio):- columnas_comparables(0, M, CCL, CL), % CCL = Columnas Casi Llenas, CL = Columnas Llenas (N-2)
                            comparacion(CCL, CL, Cambio), (Cambio == 1, depurar('Regla 5 Columna', M, M); true), !.

columnas_comparables(J, M, [], []):- J > M. % Caso base
columnas_comparables(J, M, CCL, CL):- J1 is J+1,
                                      columnas_comparables(J1, M, CCLR, CLR), % CCL Recursivo, CL Recursivo
                                      analizar_columna(0, J, M, X, O),
                                      ((M-1 =:= X+O, armar_columna(0, J, M, C1), CCL = [C1|CCLR], CL = CLR);%Columna Casi Llena (N-2)
                                      (M+1 =:= X+O, armar_columna(0, J, M, C2), CCL = CCLR, CL = [C2|CLR]); % Columna Llena
                                      (CCL = CCLR, CL = CLR)).

armar_columna(I, _, M, []):- I > M.
armar_columna(I, J, M, [t(I, J, C)|Col]):- I1 is I+1,
                                           armar_columna(I1, J, M, Col),
                                           (tablero(I, J, C); C = v). % v = vacio

%################################################
%##############   Fuerza Bruta   ################
%################################################

fuerza_bruta().

%################################################
%############   Niveles del juego   #############
%################################################

% Matriz de 6x6 Hard: id =  9,575,098
game(1, 5, 5, 8):- assertz(tablero(0,0,o)),
                   assertz(tablero(0,4,o)),
                   assertz(tablero(1,4,o)),
                   assertz(tablero(2,0,x)),
                   assertz(tablero(2,2,x)),
                   assertz(tablero(3,2,x)),
                   assertz(tablero(4,1,o)),
                   assertz(tablero(4,4,o)).

% Matriz de 8x8 Hard: id = 3,409,866
game(2, 7, 7, 14):- assertz(tablero(0,2,o)),
                    assertz(tablero(0,3,x)),
                    assertz(tablero(1,0,o)),
                    assertz(tablero(1,2,o)),
                    assertz(tablero(1,6,o)),
                    assertz(tablero(2,5,o)),
                    assertz(tablero(3,3,o)),
                    assertz(tablero(3,7,x)),
                    assertz(tablero(6,1,x)),
                    assertz(tablero(6,4,o)),
                    assertz(tablero(6,6,x)),
                    assertz(tablero(7,2,o)),
                    assertz(tablero(7,6,x)),
                    assertz(tablero(7,7,x)).

% Matriz de 10x10 Hard: id = 3,924,233
game(3, 9, 9, 22):- assertz(tablero(0,6,o)),
                    assertz(tablero(0,7,x)),
                    assertz(tablero(1,4,x)),
                    assertz(tablero(1,5,x)),
                    assertz(tablero(2,2,x)),
                    assertz(tablero(3,7,o)),
                    assertz(tablero(4,1,o)),
                    assertz(tablero(4,4,x)),
                    assertz(tablero(5,0,o)),
                    assertz(tablero(5,4,x)),
                    assertz(tablero(5,8,o)),
                    assertz(tablero(6,1,o)),
                    assertz(tablero(6,9,o)),
                    assertz(tablero(7,2,x)),
                    assertz(tablero(7,5,x)),
                    assertz(tablero(8,0,o)),
                    assertz(tablero(8,4,o)),
                    assertz(tablero(8,7,o)),
                    assertz(tablero(8,9,o)),
                    assertz(tablero(9,2,x)),
                    assertz(tablero(9,3,o)),
                    assertz(tablero(9,8,x)).

% Matriz de 14x14 Hard: id = 2,333,305
game(4, 13, 13, 47):- assertz(tablero(0,2,o)),
                      assertz(tablero(0,5,x)),
                      assertz(tablero(0,7,o)),
                      assertz(tablero(0,13,o)),
                      assertz(tablero(1,6,x)),
                      assertz(tablero(1,11,x)),
                      assertz(tablero(2,1,x)),
                      assertz(tablero(2,5,x)),
                      assertz(tablero(2,7,x)),
                      assertz(tablero(2,8,x)),
                      assertz(tablero(3,0,x)),
                      assertz(tablero(3,1,x)),
                      assertz(tablero(3,13,o)),
                      assertz(tablero(4,4,x)),
                      assertz(tablero(4,11,x)),
                      assertz(tablero(4,13,o)),
                      assertz(tablero(5,9,x)),
                      assertz(tablero(6,2,x)),
                      assertz(tablero(6,4,x)),
                      assertz(tablero(6,7,o)),
                      assertz(tablero(6,10,o)),
                      assertz(tablero(7,1,x)),
                      assertz(tablero(7,5,x)),
                      assertz(tablero(7,6,o)),
                      assertz(tablero(8,4,o)),
                      assertz(tablero(8,8,x)),
                      assertz(tablero(8,12,o)),
                      assertz(tablero(9,0,x)),
                      assertz(tablero(9,3,x)),
                      assertz(tablero(9,6,o)),
                      assertz(tablero(9,9,o)),
                      assertz(tablero(9,10,o)),
                      assertz(tablero(10,1,o)),
                      assertz(tablero(10,5,x)),
                      assertz(tablero(10,7,o)),
                      assertz(tablero(10,13,x)),
                      assertz(tablero(11,0,x)),
                      assertz(tablero(11,2,x)),
                      assertz(tablero(11,3,o)),
                      assertz(tablero(11,8,x)),
                      assertz(tablero(11,12,o)),
                      assertz(tablero(12,3,o)),
                      assertz(tablero(12,12,o)),
                      assertz(tablero(13,4,x)),
                      assertz(tablero(13,8,x)),
                      assertz(tablero(13,10,o)),
                      assertz(tablero(13,11,o)).

% Matriz de 14x14 Hard: id = 177,179
game(5, 13, 13, 40):- assertz(tablero(0,10,x)),
                      assertz(tablero(0,12,x)),
                      assertz(tablero(1,2,o)),
                      assertz(tablero(1,3,x)),
                      assertz(tablero(1,5,x)),
                      assertz(tablero(1,8,o)),
                      assertz(tablero(1,9,o)),
                      assertz(tablero(1,11,o)),
                      assertz(tablero(2,2,o)),
                      assertz(tablero(2,4,o)),
                      assertz(tablero(3,1,x)),
                      assertz(tablero(3,6,x)),
                      assertz(tablero(3,9,x)),
                      assertz(tablero(4,0,x)),
                      assertz(tablero(4,4,x)),
                      assertz(tablero(4,11,o)),
                      assertz(tablero(5,3,o)),
                      assertz(tablero(6,6,o)),
                      assertz(tablero(6,10,x)),
                      assertz(tablero(6,12,x)),
                      assertz(tablero(7,6,o)),
                      assertz(tablero(7,9,o)),
                      assertz(tablero(8,0,x)),
                      assertz(tablero(8,7,x)),
                      assertz(tablero(8,10,o)),
                      assertz(tablero(8,13,x)),
                      assertz(tablero(9,4,x)),
                      assertz(tablero(9,11,x)),
                      assertz(tablero(10,5,o)),
                      assertz(tablero(10,11,x)),
                      assertz(tablero(11,1,x)),
                      assertz(tablero(11,4,o)),
                      assertz(tablero(11,5,o)),
                      assertz(tablero(12,0,o)),
                      assertz(tablero(12,1,o)),
                      assertz(tablero(12,3,o)),
                      assertz(tablero(12,6,x)),
                      assertz(tablero(12,8,o)),
                      assertz(tablero(13,7,o)),
                      assertz(tablero(13,12,x)).

% Matriz de 20x20 Hard: id = 9,651,507
game(6, 19, 19, 94):- assertz(tablero(0,0,x)),
                      assertz(tablero(0,3,o)),
                      assertz(tablero(0,9,o)),
                      assertz(tablero(0,11,x)),
                      assertz(tablero(0,14,o)),
                      assertz(tablero(0,15,x)),
                      assertz(tablero(0,19,x)),
                      assertz(tablero(1,3,o)),
                      assertz(tablero(1,5,o)),
                      assertz(tablero(1,7,x)),
                      assertz(tablero(1,9,x)),
                      assertz(tablero(1,12,x)),
                      assertz(tablero(1,13,x)),
                      assertz(tablero(1,17,x)),
                      assertz(tablero(1,18,x)),
                      assertz(tablero(2,0,o)),
                      assertz(tablero(2,4,x)),
                      assertz(tablero(2,13,o)),
                      assertz(tablero(3,0,o)),
                      assertz(tablero(3,10,o)),
                      assertz(tablero(4,2,x)),
                      assertz(tablero(4,3,x)),
                      assertz(tablero(4,5,o)),
                      assertz(tablero(4,7,x)),
                      assertz(tablero(4,13,x)),
                      assertz(tablero(4,15,x)),
                      assertz(tablero(4,16,x)),
                      assertz(tablero(5,7,x)),
                      assertz(tablero(5,17,o)),
                      assertz(tablero(6,5,o)),
                      assertz(tablero(6,10,o)),
                      assertz(tablero(6,11,x)),
                      assertz(tablero(6,14,o)),
                      assertz(tablero(7,4,x)),
                      assertz(tablero(7,10,o)),
                      assertz(tablero(7,11,x)),
                      assertz(tablero(7,17,x)),
                      assertz(tablero(8,15,o)),
                      assertz(tablero(8,19,o)),
                      assertz(tablero(9,4,x)),
                      assertz(tablero(9,5,x)),
                      assertz(tablero(9,9,x)),
                      assertz(tablero(9,12,x)),
                      assertz(tablero(9,16,x)),
                      assertz(tablero(9,17,o)),
                      assertz(tablero(9,19,o)),
                      assertz(tablero(10,2,x)),
                      assertz(tablero(10,3,x)),
                      assertz(tablero(10,6,o)),
                      assertz(tablero(10,8,o)),
                      assertz(tablero(10,10,o)),
                      assertz(tablero(10,13,o)),
                      assertz(tablero(11,2,o)),
                      assertz(tablero(11,13,o)),
                      assertz(tablero(11,18,o)),
                      assertz(tablero(12,3,x)),
                      assertz(tablero(12,6,o)),
                      assertz(tablero(12,10,o)),
                      assertz(tablero(12,12,o)),
                      assertz(tablero(12,16,o)),
                      assertz(tablero(12,19,x)),
                      assertz(tablero(13,2,o)),
                      assertz(tablero(13,9,o)),
                      assertz(tablero(14,7,o)),
                      assertz(tablero(14,10,x)),
                      assertz(tablero(14,13,o)),
                      assertz(tablero(14,14,o)),
                      assertz(tablero(14,16,o)),
                      assertz(tablero(14,17,o)),
                      assertz(tablero(15,1,x)),
                      assertz(tablero(15,8,x)),
                      assertz(tablero(15,10,x)),
                      assertz(tablero(15,11,x)),
                      assertz(tablero(15,13,o)),
                      assertz(tablero(15,17,o)),
                      assertz(tablero(16,3,o)),
                      assertz(tablero(16,6,o)),
                      assertz(tablero(16,11,x)),
                      assertz(tablero(16,15,o)),
                      assertz(tablero(17,0,o)),
                      assertz(tablero(17,3,o)),
                      assertz(tablero(17,4,o)),
                      assertz(tablero(17,9,o)),
                      assertz(tablero(17,15,o)),
                      assertz(tablero(18,0,o)),
                      assertz(tablero(18,6,x)),
                      assertz(tablero(18,9,x)),
                      assertz(tablero(18,18,x)),
                      assertz(tablero(18,19,x)),
                      assertz(tablero(19,6,x)),
                      assertz(tablero(19,11,o)),
                      assertz(tablero(19,12,o)),
                      assertz(tablero(19,16,x)),
                      assertz(tablero(19,19,x)).

game(7, 29, 29, 219):- assertz(tablero(0,0,x)),
                       assertz(tablero(0,6,x)),
                       assertz(tablero(0,8,x)),
                       assertz(tablero(0,10,x)),
                       assertz(tablero(0,13,x)),
                       assertz(tablero(0,15,x)),
                       assertz(tablero(0,23,x)),
                       assertz(tablero(0,28,x)),
                       assertz(tablero(0,29,x)),
                       assertz(tablero(1,0,x)),
                       assertz(tablero(1,1,x)),
                       assertz(tablero(1,3,x)),
                       assertz(tablero(1,11,x)),
                       assertz(tablero(1,18,o)),
                       assertz(tablero(1,19,o)),
                       assertz(tablero(1,25,x)),
                       assertz(tablero(2,1,x)),
                       assertz(tablero(2,7,o)),
                       assertz(tablero(2,9,o)),
                       assertz(tablero(2,13,o)),
                       assertz(tablero(2,16,x)),
                       assertz(tablero(2,18,x)),
                       assertz(tablero(2,22,x)),
                       assertz(tablero(2,23,x)),
                       assertz(tablero(2,28,o)),
                       assertz(tablero(3,2,o)),
                       assertz(tablero(3,7,o)),
                       assertz(tablero(3,12,x)),
                       assertz(tablero(3,20,o)),
                       assertz(tablero(3,27,x)),
                       assertz(tablero(4,5,o)),
                       assertz(tablero(4,9,o)),
                       assertz(tablero(4,11,o)),
                       assertz(tablero(4,14,x)),
                       assertz(tablero(4,15,x)),
                       assertz(tablero(4,25,x)),
                       assertz(tablero(4,26,o)),
                       assertz(tablero(4,28,o)),
                       assertz(tablero(5,15,x)),
                       assertz(tablero(5,19,x)),
                       assertz(tablero(5,20,x)),
                       assertz(tablero(5,23,o)),
                       assertz(tablero(6,0,o)),
                       assertz(tablero(6,1,x)),
                       assertz(tablero(6,3,x)),
                       assertz(tablero(6,8,x)),
                       assertz(tablero(6,10,x)),
                       assertz(tablero(6,18,x)),
                       assertz(tablero(6,21,o)),
                       assertz(tablero(6,25,o)),
                       assertz(tablero(6,26,o)),
                       assertz(tablero(7,4,x)),
                       assertz(tablero(7,6,x)),
                       assertz(tablero(7,15,x)),
                       assertz(tablero(7,16,x)),
                       assertz(tablero(7,23,o)),
                       assertz(tablero(7,27,x)),
                       assertz(tablero(8,1,o)),
                       assertz(tablero(8,3,o)),
                       assertz(tablero(8,9,o)),
                       assertz(tablero(8,13,o)),
                       assertz(tablero(8,17,o)),
                       assertz(tablero(8,21,o)),
                       assertz(tablero(8,26,o)),
                       assertz(tablero(9,0,x)),
                       assertz(tablero(9,2,o)),
                       assertz(tablero(9,3,o)),
                       assertz(tablero(9,6,x)),
                       assertz(tablero(9,8,x)),
                       assertz(tablero(9,10,x)),
                       assertz(tablero(9,21,o)),
                       assertz(tablero(9,23,o)),
                       assertz(tablero(10,5,o)),
                       assertz(tablero(10,13,x)),
                       assertz(tablero(10,14,x)),
                       assertz(tablero(10,16,o)),
                       assertz(tablero(10,19,x)),
                       assertz(tablero(10,26,o)),
                       assertz(tablero(10,29,o)),
                       assertz(tablero(11,0,x)),
                       assertz(tablero(11,2,o)),
                       assertz(tablero(11,3,x)),
                       assertz(tablero(11,8,x)),
                       assertz(tablero(11,10,x)),
                       assertz(tablero(11,11,x)),
                       assertz(tablero(11,15,o)),
                       assertz(tablero(11,26,o)),
                       assertz(tablero(11,27,x)),
                       assertz(tablero(11,29,o)),
                       assertz(tablero(12,1,o)),
                       assertz(tablero(12,13,o)),
                       assertz(tablero(12,14,o)),
                       assertz(tablero(12,17,o)),
                       assertz(tablero(12,21,x)),
                       assertz(tablero(12,23,x)),
                       assertz(tablero(12,27,x)),
                       assertz(tablero(13,1,x)),
                       assertz(tablero(13,3,o)),
                       assertz(tablero(13,6,x)),
                       assertz(tablero(13,7,x)),
                       assertz(tablero(13,11,x)),
                       assertz(tablero(13,12,x)),
                       assertz(tablero(13,19,x)),
                       assertz(tablero(13,29,x)),
                       assertz(tablero(14,10,o)),
                       assertz(tablero(14,14,o)),
                       assertz(tablero(14,16,x)),
                       assertz(tablero(14,23,o)),
                       assertz(tablero(14,25,o)),
                       assertz(tablero(15,5,o)),
                       assertz(tablero(15,9,x)),
                       assertz(tablero(15,12,x)),
                       assertz(tablero(15,13,x)),
                       assertz(tablero(15,19,x)),
                       assertz(tablero(15,21,x)),
                       assertz(tablero(15,24,x)),
                       assertz(tablero(15,26,x)),
                       assertz(tablero(15,27,x)),
                       assertz(tablero(15,29,x)),
                       assertz(tablero(16,0,x)),
                       assertz(tablero(16,1,x)),
                       assertz(tablero(16,4,x)),
                       assertz(tablero(16,5,o)),
                       assertz(tablero(16,8,o)),
                       assertz(tablero(16,21,x)),
                       assertz(tablero(17,2,o)),
                       assertz(tablero(17,11,x)),
                       assertz(tablero(17,13,x)),
                       assertz(tablero(17,19,x)),
                       assertz(tablero(17,20,x)),
                       assertz(tablero(17,23,o)),
                       assertz(tablero(17,27,x)),
                       assertz(tablero(18,4,o)),
                       assertz(tablero(18,5,o)),
                       assertz(tablero(18,7,o)),
                       assertz(tablero(18,10,o)),
                       assertz(tablero(18,14,o)),
                       assertz(tablero(18,17,o)),
                       assertz(tablero(18,18,o)),
                       assertz(tablero(18,23,x)),
                       assertz(tablero(19,2,o)),
                       assertz(tablero(19,6,x)),
                       assertz(tablero(19,11,x)),
                       assertz(tablero(19,15,x)),
                       assertz(tablero(19,22,x)),
                       assertz(tablero(19,25,x)),
                       assertz(tablero(19,28,o)),
                       assertz(tablero(20,3,o)),
                       assertz(tablero(20,14,x)),
                       assertz(tablero(20,18,o)),
                       assertz(tablero(20,26,x)),
                       assertz(tablero(20,28,o)),
                       assertz(tablero(21,0,x)),
                       assertz(tablero(21,4,x)),
                       assertz(tablero(21,8,o)),
                       assertz(tablero(21,11,o)),
                       assertz(tablero(21,13,x)),
                       assertz(tablero(21,14,x)),
                       assertz(tablero(21,22,o)),
                       assertz(tablero(22,2,o)),
                       assertz(tablero(22,6,o)),
                       assertz(tablero(22,9,x)),
                       assertz(tablero(22,11,o)),
                       assertz(tablero(22,16,o)),
                       assertz(tablero(22,17,o)),
                       assertz(tablero(22,19,x)),
                       assertz(tablero(22,25,o)),
                       assertz(tablero(22,27,o)),
                       assertz(tablero(23,2,o)),
                       assertz(tablero(23,3,o)),
                       assertz(tablero(23,8,o)),
                       assertz(tablero(23,10,o)),
                       assertz(tablero(23,13,x)),
                       assertz(tablero(23,14,o)),
                       assertz(tablero(23,22,o)),
                       assertz(tablero(23,24,x)),
                       assertz(tablero(23,25,o)),
                       assertz(tablero(23,29,o)),
                       assertz(tablero(24,7,x)),
                       assertz(tablero(24,16,o)),
                       assertz(tablero(24,17,x)),
                       assertz(tablero(24,21,x)),
                       assertz(tablero(25,1,o)),
                       assertz(tablero(25,4,o)),
                       assertz(tablero(25,17,x)),
                       assertz(tablero(25,18,x)),
                       assertz(tablero(25,24,o)),
                       assertz(tablero(25,27,o)),
                       assertz(tablero(25,28,o)),
                       assertz(tablero(26,5,x)),
                       assertz(tablero(26,11,x)),
                       assertz(tablero(26,19,x)),
                       assertz(tablero(26,20,x)),
                       assertz(tablero(26,25,x)),
                       assertz(tablero(27,0,o)),
                       assertz(tablero(27,3,o)),
                       assertz(tablero(27,9,o)),
                       assertz(tablero(27,11,x)),
                       assertz(tablero(27,22,x)),
                       assertz(tablero(27,24,x)),
                       assertz(tablero(27,28,o)),
                       assertz(tablero(27,29,o)),
                       assertz(tablero(28,1,x)),
                       assertz(tablero(28,4,x)),
                       assertz(tablero(28,7,x)),
                       assertz(tablero(28,12,o)),
                       assertz(tablero(28,16,o)),
                       assertz(tablero(28,25,o)),
                       assertz(tablero(28,26,o)),
                       assertz(tablero(29,0,o)),
                       assertz(tablero(29,4,x)),
                       assertz(tablero(29,9,o)),
                       assertz(tablero(29,10,x)),
                       assertz(tablero(29,13,x)),
                       assertz(tablero(29,14,x)),
                       assertz(tablero(29,17,o)),
                       assertz(tablero(29,21,x)),
                       assertz(tablero(29,22,x)),
                       assertz(tablero(29,28,o)).
