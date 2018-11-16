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

depurar(Texto, N, M):- write(Texto), nl, imprimir(0, N, M). % read(X), X \== q.

%###############################################
%#############Logica del programa###############
%###############################################

% Predicado que es llamado desde la GUI
resuelve(Nivel, L):- retractall(tablero(I, J, C)), % Eliminamos de la memoria cualquier contenido dinamico que
                                                   % hubiese quedado
                     game(Nivel, N, M),
                     % depurar('Tablero inicial', N, M),
                     ciclo(N, M, 0),
                     depurar('Tablero final', N, M),
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
ciclo(_, _, 0):- write('Default'), nl. % No se aplico fuerza bruta, el ciclo puede terminar
ciclo(_, _, 1):- verifica_validez(sarasa). % Se aplico fuerza bruta, se debe verificar validez del tablero

siguiente_celda(N, M, N, M, _, _):- fail. % Fuera de la matriz
siguiente_celda(N, M, I, M, I1, 0):- I < N, I1 is I+1. % Ultima columna
siguiente_celda(N, M, N, J, N, J1):- J < M, J1 is J+1. % Ultima fila
siguiente_celda(N, M, I, J, I, J1):- I < N, J < M, J1 is J+1. % Dentro de la matriz

% Aplicacion de cada una de las reglas de resolucion del juego
aplicar_reglas(N, M, FB):- reglas1y2(0, -1, N, M, Cambio), Cambio == 1, ciclo(N, M, FB).
aplicar_reglas(N, M, FB):- regla3_fila(0, N, Cambio), Cambio == 1, ciclo(N, M, FB).
aplicar_reglas(N, M, FB):- regla3_columna(0, M, Cambio), Cambio == 1, ciclo(N, M, FB).
aplicar_reglas(N, M, FB):- regla4_fila(0, N, Cambio), Cambio == 1, ciclo(N, M, FB).
% aplicar_reglas(N, M, FB):- regla4_columna(0, M, Cambio), Cambio == 1,
% ciclo(N, M, FB). aplicar_reglas(N, M, FB):- regla5(Cambio), Cambio ==
% 1, ciclo(N, M, FB). aplicar_reglas(N, M, _):- fuerza_bruta(), ciclo(N,
% M, 1).

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
                                 depurar('Regla 1', 7, 7).
miro_abajo_color(I, J, Cambio):- tablero(I, J, Color), % Regla 2
                                 I2 is I+2,
                                 tablero(I2, J, Color),
                                 I1 is I+1,
                                 not(tablero(I1, J, _)),
                                 Cambio is 1,
                                 (Color == x, assertz(tablero(I1, J, o)); assertz(tablero(I1, J, x))),
                                 depurar('Regla 2', 7, 7).
miro_abajo_color(_, _, _).

miro_derecha_color(I, J, Cambio):- tablero(I, J, Color), % Regla 1
                                   J1 is J+1,
                                   tablero(I, J1, Color),
                                   J2 is J1+1,
                                   not(tablero(I, J2, _)),
                                   Cambio is 1,
                                   (Color == x, assertz(tablero(I, J2, o)); assertz(tablero(I, J2, x))),
                                   depurar('Regla 1', 7, 7).
miro_derecha_color(I, J, Cambio):- tablero(I, J, Color), % Regla 2
                                   J2 is J+2,
                                   tablero(I, J2, Color),
                                   J1 is J+1,
                                   not(tablero(I, J1, _)),
                                   Cambio is 1,
                                   (Color == x, assertz(tablero(I, J1, o)); assertz(tablero(I, J1, x))),
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
                            depurar('Regla 3', N, N),
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
                               depurar('Regla 3', M, M),
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

regla4_fila(I, N, _):- I > N, !. % Caso base
regla4_fila(I, N, Cambio):- analizar_fila(I, 0, N, X, O),
                            Y is ((N+1)/2)-1, % Mitad de la fila menos 1
                            ((X =:= Y, A is (N+1)/2-O, A >= 2, C = o, R = A); % Nos aseguramos que "falten 2 o mas de B"
                                                                              % A = Blancos que faltan
                            (O =:= Y, B is (N+1)/2-X, B >= 2, C = x, R = B)), % B = Negros que faltan
                            armar_lista_posiciones_fila(I, 0, N, L),
                            adpc(I, N, L, C, Cambio, R),
                            depurar('Regla 4', N, N).
regla4_fila(I, N, Cambio):- I1 is I+1, regla4_fila(I1, N, Cambio).

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

% analizar_distancias_primer_caso
adpc(I, N, [0|Cola], Color, Cambio, R):- adem(I, N, [0|Cola], Color, Cambio, R).
adpc(I, N, [X|_], Color, Cambio, _):- X == 1, % Segundo elemento con uno libre a la izquierda
                                      not(tablero(I, 2, _)),
                                      tablero(I, X, Color), % Importa el color
                                      completar_fila(I, 1, N, [0, 2], Color),
                                      Cambio is 1.
adpc(I, N, [X|_], Color, Cambio, _):- X >= 2, % Tercer elemento con dos o mas libres a su izquierda
                                      tablero(I, X, Color), % Importa el color
                                      X1 is X-1,
                                      X2 is X-2,
                                      completar_fila(I, 0, N, [X2, X1], Color),
                                      Cambio is 1.
adpc(I, N, [X|_], Color, Cambio, R):- X == 3, % Cuarto elemento con 3 libres a la izquierda,
                                              % no importa el color
                                      R >= 3,
                                      completar_fila(I, 0, N, [0, 1, 2], Color),
                                      Cambio is 1.
adpc(I, N, [X|_], Color, Cambio, _):- X == 4, % Quinto elemento con 4 libres a la izquierda,
                                              % no importa el color
                                      completar_fila(I, 0, N, [1, 2], Color),
                                      Cambio is 1.
adpc(I, N, [X|_], Color, Cambio, _):- X == 5, % Sexto elemento con 5 libres a la izquierda,
                                              % no importa el color
                                      completar_fila(I, 0, N, [2], Color),
                                      (Color == x, Opuesto = o; Opuesto = x),
                                      assertz(tablero(I, 2, Opuesto)),
                                      Cambio is 1.
% No avanzamos en la lista, seguimos en el mismo elemento para poder
% comparar las diferencias con el de la derecha
adpc(I, N, L, Color, Cambio, R):- adem(I, N, L, Color, Cambio, R).

% analizar_distancias_entre_medio
adem(I, N, [X], Color, Cambio, R):- aduc(I, N, [X], Color, Cambio, R).
adem(I, N, [X, Y|_], Color, Cambio, _):- 2 =:= Y-X, % Espacio libre a la derecha
                                         X1 is X-1,
                                         not(tablero(I, X1, _)), % Espacio libre a la izquierda
                                         tablero(I, X, Color), % Importa el color
                                         X2 is X+1,
                                         completar_fila(I, 0, N, [X1, X2], Color),
                                         Cambio is 1.
adem(I, N, [X, Y|_], Color, Cambio, _):- D is Y-X,
                                         D >= 2, % 2 o mas espacios libres a la derecha
                                         tablero(I, X, Color), % Importa el color
                                         X1 is X+1,
                                         X2 is X+2,
                                         completar_fila(I, 0, N, [X1, X2], Color),
                                         Cambio is 1.
adem(I, N, [X, _|_], Color, Cambio, _):- X >= 2, % Me aseguro que puedo mirar dos celdas a la izquierda
                                         X1 is X-1, % Miro 2 lugares a la izquierda, sin aprovechar la lista
                                         not(tablero(I, X1, _)),
                                         X2 is X-2,
                                         not(tablero(I, X2, _)),
                                         tablero(I, X, Color), % Importa el color
                                         completar_fila(I, 0, N, [X2, X1], Color),
                                         Cambio is 1.
adem(I, N, [X, Y|_], Color, Cambio, R):- 4 =:= Y-X, % 3 espacios libres
                                         R >= 3,
                                         X1 is X+1, X2 is X+2, X3 is X+3,
                                         completar_fila(I, 0, N, [X1, X2, X3] , Color),
                                         Cambio is 1.
adem(I, N, [X, Y|_], Color, Cambio, _):- 5 =:= Y-X, % 4 espacios libres
                                         X2 is X+2, X3 is X+3,
                                         completar_fila(I, 0, N, [X2, X3] , Color),
                                         Cambio is 1.
adem(I, N, [X, Y|_], Color, Cambio, _):- 6 =:= Y-X, % 5 espacios libres
                                         X3 is X+3,
                                         completar_fila(I, 0, N, [X3] , Color),
                                         (Color == x, Opuesto = o; Opuesto = x),
                                         assertz(tablero(I, X3, Opuesto)),
                                         Cambio is 1.
adem(I, N, [_, Y|Cola], Color, Cambio, R):- adem(I, N, [Y|Cola], Color, Cambio, R).

% analizar_distancias_ultimo_caso
aduc(_, N, [N], _, _, _). % No hay espacios libres despues del ultimo elemento
aduc(I, N, [X], Color, Cambio, _):- 1 is N-X, % 1 espacio libre a la derecha
                                    X1 is X-1,
                                    not(tablero(I, X1, _)),
                                    tablero(I, X, Color), % Importa el color
                                    completar_fila(I, 0, N, [X1, N], Color),
                                    Cambio is 1.
aduc(I, N, [X], Color, Cambio, _):- D is N-X,
                                    D >= 2, % Ultimo elemento con dos o mas libres a su derecha
                                    tablero(I, X, Color), % Importa el color
                                    X1 is X+1,
                                    completar_fila(I, 0, N, [X1, N], Color),
                                    Cambio is 1.
aduc(I, N, [X], Color, Cambio, R):- 3 =:= N-X, % Ultimo elemento con 3 libres a la derecha,
                                               % no importa el color
                                    R >= 3,
                                    X1 is X+1,
                                    X2 is X+2,
                                    completar_fila(I, 0, N, [X1, X2, N], Color),
                                    Cambio is 1.
aduc(I, N, [X], Color, Cambio, _):- 4 =:= N-X, % Ultimo elemento con 4 libres a la derecha,
                                               % no importa el color
                                    X2 is X+2,
                                    X3 is X+3,
                                    completar_fila(I, 0, N, [X2, X3], Color),
                                    Cambio is 1.
aduc(I, N, [X], Color, Cambio, _):- 5 =:= N-X, % Ultimo elemento con 5 libres a la derecha,
                                               % no importa el color
                                    X3 is X+3,
                                    completar_fila(I, 0, N, [X3], Color),
                                    (Color == x, Opuesto = o; Opuesto = x),
                                    assertz(tablero(I, X3, Opuesto)),
                                    Cambio is 1.

%################################################
%#################   Regla 5   ##################
%################################################

regla5(1).
fuerza_bruta().

%################################################
%#############  Niveles del juego  ##############
%################################################

% Matriz de 6x6: id =  9,575,098
game(1, 5, 5):- assertz(tablero(0,0,o)),
                assertz(tablero(0,4,o)),
                assertz(tablero(1,4,o)),
                assertz(tablero(2,0,x)),
                assertz(tablero(2,2,x)),
                assertz(tablero(3,2,x)),
                assertz(tablero(4,1,o)),
                assertz(tablero(4,4,o)).

% Matriz de 8x8
game(2, 7, 7):- assertz(tablero(0,1,o)),
                assertz(tablero(0,3,x)),
                assertz(tablero(0,6,x)),
                assertz(tablero(0,7,x)),
                assertz(tablero(1,2,x)),
                assertz(tablero(1,7,o)),
                assertz(tablero(2,0,x)),
                assertz(tablero(2,2,x)),
                assertz(tablero(2,4,x)),
                assertz(tablero(3,0,x)),
                assertz(tablero(3,3,x)),
                assertz(tablero(3,5,x)),
                assertz(tablero(3,6,x)),
                assertz(tablero(4,2,o)),
                assertz(tablero(4,3,o)),
                assertz(tablero(4,6,x)),
                assertz(tablero(5,1,o)),
                assertz(tablero(5,2,x)),
                assertz(tablero(5,7,o)),
                assertz(tablero(6,1,x)),
                assertz(tablero(6,2,x)),
                assertz(tablero(6,5,o)),
                assertz(tablero(6,6,o)),
                assertz(tablero(7,4,o)),
                assertz(tablero(7,7,o)).

% Matriz de 10x10
game(3, 9, 9).

% Matriz de 14x14
game(4, 13, 13).

% Matriz de 20x20
game(5, 19, 19).

% Tiene errores este nivel, sirve nada m�s para probarlo
game(6, 7, 7):- assertz(tablero(0,1,o)),
                assertz(tablero(0,3,x)),
                assertz(tablero(0,6,x)),
                assertz(tablero(1,1,o)),
                assertz(tablero(1,7,o)),
                assertz(tablero(1,7,o)),
                assertz(tablero(1,7,o)),
                assertz(tablero(2,0,x)),
                assertz(tablero(2,4,x)),
                assertz(tablero(3,0,x)),
                assertz(tablero(3,3,x)),
                assertz(tablero(3,6,x)),
                assertz(tablero(4,6,x)),
                assertz(tablero(5,1,o)),
                assertz(tablero(5,7,o)),
                assertz(tablero(6,2,x)),
                assertz(tablero(7,4,o)),
                assertz(tablero(7,7,o)).

game(7, 7, 7):- assertz(tablero(0,1,o)),
                assertz(tablero(0,5,x)),
                assertz(tablero(0,6,o)),
                assertz(tablero(0,7,o)).
