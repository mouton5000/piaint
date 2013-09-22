%%%%%%%%%%%%%%%%%
% color(?color, ?hue, ?lightness)
% color(C,H,L) is true if a color named C has a hue H and a lightness L.

color(lred, 0,0).
color(lyellow, 1,0).
color(lgreen, 2,0).
color(lcyan, 3,0).
color(lblue, 4,0).
color(lmagenta, 5,0).
color(red, 0,1).
color(yellow, 1,1).
color(green, 2,1).
color(cyan, 3,1).
color(blue, 4,1).
color(magenta, 5,1).
color(dred, 0,2).
color(dyellow, 1,2).
color(dgreen, 2,2).
color(dcyan, 3,2).
color(dblue, 4,2).
color(dmagenta, 5,2).


% hue(?color, ?hue)
% hue(C,H) is true if a color named C has a hue H.
hue(C, H) :- color(C,H,_).


% lightness(?color, ?lightness)
% lightness(C,L) is true if a color named C has a lightness L.
lightness(C, L) :- color(C,_,L).


% darkdiff(+color1, +color2, -diff)
% darkdiff(+color1, -color2, +diff)
% darkdiff(-color1, +color2, +diff)
% darkdiff(?color1, ?color2, ?diff)
% darkdiff(C1,C2,D) is true if difference of lightness of colors C2 by lightness of color C1 is D.
darkdiff(C1, C2, D) :- lightness(C1,L1), lightness(C2,L2), D is (L2 - L1) mod 3 .


% stepdiff(+color1, +color2, -diff)
% stepdiff(+color1, -color2, +diff)
% stepdiff(-color1, +color2, +diff)
% stepdiff(?color1, ?color2, ?diff)
% stepdiff(C1,C2,D) is true if difference of hue of colors C2 by hue of color C1 is D.
stepdiff(C1, C2, D) :- hue(C1,H1), hue(C2,H2), D is (H2 - H1) mod 6 .

% neq(+color1, +color2)
% neq(C1,C2) is true if C1 cannot be unified with C2.
neq(C1,C2) :- not(C1 = C2).

% diff(+color1, +color2, -operation)
% diff(+color1, -color2, +operation)
% diff(-color1, +color2, +operation)
% diff(?color1, ?color2, ?operation)
% diff(C1,C2,OP) is true if transiting from color C1 to color C2 apply command OP
diff(black,_,onone).
diff(white,_,onone).
diff(_,black,onone).
diff(_,white,onone).
diff(C1, C2, O) :-
  color(C1,H1,L1), color(C2,H2,L2),
  DH is (H2 - H1) mod 6,
  DL is (L2 - L1) mod 3,
  operation(O, DH,DL).


% nextOp(+bloc, +color, +directionPointer, +codelChooser, -operation, -absc,
% -ord, -nextDirectionPointer, -nextCodelChooser)
% nextOp(B,C,DP,CC,OP,X,Y,NDP,NCC) is true if C is black and OP is onone, or
% if C is not black, and if while traversing color bloc B,
% considering direction pointer DP and codel chooser CC, it outs
% at position (X,Y), applying command OP, with new direction pointer NDP, and
% new direction codel chooser NCC. It is false if interpreter
% cannot go out of the bloc.
nextOp(B,C,DP,CC,O,X2,Y2, NDP, NCC) :-
  neq(C,black), neq(C,white),
  tryNextOp(B,C,DP,CC,O,X2,Y2,0,NDP,NCC).

% tryNextOp(+bloc,+color, +directionPointer, +codelChooser, -opetation, -absc,
% -ord, -nextDirectionPointer, -nextCodelChooser, ?numberOfTry)
% tryNextOp(B,C,DP,CC,OP,X,Y,Try) is true if Try is lower than 8, and
% while traversing color bloc B, of color C,
% considering direction pointer DP, and codel chooser CC, it outs at position
% (X,Y), applying command OP, with new direction pointer NDP, and
% new direction codel chooser NCC.
%
% If interpreter can not out with direction pointer DP and codel chooser CC,
% it tries with next direction pointer and codel chooser, and increase Try by 1.
tryNextOp(B,C,DP,CC,O,X2,Y2,Try,DP,CC):-
  Try < 8,
  outbloc(B,DP,CC,X2,Y2),
  program(X2,Y2,C2),
  neq(C2,black),
  diff(C,C2,O).
tryNextOp(B,C,DP,CC,O,X2,Y2,Try,NDP,NCC):-
  Try < 8,
  outbloc(B,DP,CC,X1,Y1),
  program(X1,Y1,black),
  Try2 is Try + 1,
  nextpointer(DP,CC,N1DP,N1CC),
  tryNextOp(B,C,N1DP,N1CC,O,X2,Y2,Try2,NDP,NCC).

% outbloc(+bloc,+directionPointer, +codelChooser, -absc,-ord)
% outbloc(B,DP,CC,X,Y) is true if, considering direction pointer DP and codel chooser CC,
% the bloc out at coordinates (X,Y).
outbloc(B,dright,cleft,X,Y) :- maxXList(B,BR), maxYElem(BR,X1,Y), X is X1+1 .
outbloc(B,dright,cright,X,Y) :- maxXList(B,BR), minYElem(BR,X1,Y), X is X1+1 .
outbloc(B,dleft,cleft,X,Y) :- minXList(B,BR), minYElem(BR,X1,Y), X is X1-1 .
outbloc(B,dleft,cright,X,Y) :- minXList(B,BR), maxYElem(BR,X1,Y), X is X1-1 .
outbloc(B,dbottom,cleft,X,Y) :- minYList(B,BR), maxXElem(BR,X,Y1), Y is Y1-1 .
outbloc(B,dbottom,cright,X,Y) :- minYList(B,BR), minXElem(BR,X,Y1), Y is Y1-1 .
outbloc(B,dtop,cleft,X,Y) :- maxYList(B,BR), minXElem(BR,X,Y1), Y is Y1+1 .
outbloc(B,dtop,cright,X,Y) :- maxYList(B,BR), maxXElem(BR,X,Y1), Y is Y1+1 .

% outblocWhite(+absc, +ord, +directionPointer, +codelChooser, -absc2, -ord2, -nextDirectionPointer, -nextCodelChooser)
% outblocWhite(X1,Y1,DP,CC,X2,Y2,NDP,NCC) is true if, considering direction pointer DP,
% and starting position (X,Y), the white bloc outs at coordinates (X2,Y2)
% with direction pointer NDP, and codel chooser NCC.
outblocWhite(X,Y,DP,CC,X2,Y2,NDP,NCC) :- outblocWhitePro(X,Y,DP,CC,X2,Y2,NDP,NCC,[]).

% outblocWhite(+absc,+ord, +directionPointer, +codelChooser, -absc2, -ord2, -nextDirectionPointer, -nextCodelChooser, +ProList)
% outblocWhite(X1,Y1,DP,CC,X2,Y2,NDP,NCC,L) is true if, considering direction pointer DP, and starting position (X,Y)
% the white bloc outs at coordinates (X2,Y2) with direction pointer NDP and codel chooser NCC, without passing
% through any coordinate (X,Y) with direction pointer DP such that (X,Y,DP) is in L.
outblocWhitePro(X,Y,DP,CC,X2,Y2,NDP,NCC,L) :-
  notmember((X,Y,DP),L),
  neighbour(X,Y,DP,X1,Y1),
  checkNeighbour(X,Y,DP,CC,X1,Y1,X2,Y2,NDP,NCC,[(X,Y,DP)|L]),!.

% checkNeighbour(+abs,+ord,+directionPointer, +codelChooser, +absN, +ordN, -abs2, -ord2, -nextDirectionPointer, -nextCodelChooser, +proList)
% checkNeighbour(X,Y,DP,CC,X1,Y1,X2,Y2,NDP,NCC,L) is true
% - if (X1,Y1) is a black coordinate, and after turning one time clockwise the direction pointer DP,
% and starting at position (X,Y), the white bloc outs at coordinates (X2,Y2) with direction pointer NDP
% and codel chooser cleft, without passing through any coordinate (X',Y') with direction
% pointer DP' such that (X',Y',DP') is in L.
% - if (X1,Y1) is a white coordinate, and starting at position (X1,Y1), the white bloc outs at
% coordinates (X2,Y2) with direction pointer NDP and codel chooser NCC, without passing through
% any coordinate (X',Y') with direction pointer DP' such that (X',Y',DP') is in L.
% - if (X1,Y1) is a neither a black nor a white coordinate, and X1 = X2, Y1 = Y2, DP = NDP, and CC = NCC.
checkNeighbour(X,Y,DP,_,X1,Y1,X2,Y2,NDP,cleft,L) :-
  program(X1,Y1,black),
  nextDirectionPointer(DP,NDP1),
  outblocWhitePro(X,Y,NDP1,cleft,X2,Y2,NDP,_,L),!.
checkNeighbour(_,_,DP,CC,X1,Y1,X2,Y2,NDP,NCC,L) :- program(X1,Y1,white), outblocWhitePro(X1,Y1,DP,CC,X2,Y2,NDP,NCC,L),!.
checkNeighbour(_,_,DP,CC,X1,Y1,X1,Y1,DP,CC,_) :- program(X1,Y1,C), neq(C,white), neq(C,black), !.

% neighbour(+abs,+ord,+directionPointer,-abs1, -ord1)
% neighbour(X,Y,DP,X1,Y1) is true if following direction pointer DP during one codel, starting at
% (X,Y), it ends at (X1,Y1).
neighbour(X,Y,dright,X1,Y) :- X1 is X+1 .
neighbour(X,Y,dleft,X1,Y) :- X1 is X-1 .
neighbour(X,Y,dtop,X,Y1) :- Y1 is Y+1 .
neighbour(X,Y,dbottom,X,Y1) :- Y1 is Y-1 .


% bloc(+abs, +ord, -bloc)
% bloc(X,Y,B) is true if B is the bloc containing coordinate (X,Y)
bloc(X,Y,B) :- program(X,Y,C), blocpro(X,Y,C,B,[]).

% blocpro(+abs,+ord,+color,-bloc,+listpro)
% blocpro(X,Y,C,B,L) is true if B is the bloc containing coordinate (X,Y)
% with color C, if every coordinate in L is prohibited (and is a border of
% the bloc).
blocpro(_,_,black,[],_).
blocpro(X,Y,C,[],_) :- program(X,Y,C1), neq(C,C1).
blocpro(X,Y,_,[],L) :- memberchk((X,Y),L).
blocpro(X,Y,C,[(X,Y)|B],L) :-
  program(X,Y,C),
  notmember((X,Y),L),
  neq(C,black),
  XM1 is X-1,
  XP1 is X+1,
  YM1 is Y-1,
  YP1 is Y+1,
  blocpro(XM1,Y,C,BL,[(X,Y)|L]),
  append(L,BL,BLL),
  blocpro(XP1,Y,C,BR,[(X,Y)|BLL]),
  append(BLL,BR,BLLR),
  blocpro(X,YM1,C,BD,[(X,Y)|BLLR]),
  append(BLLR,BD,BLLRD),
  blocpro(X,YP1,C,BU,[(X,Y)|BLLRD]),
  append(BL,BR,BLR), append(BLR,BD, BLRD), append(BLRD,BU,B),!.

% notmember(+elem, +list)
% notmember(X,L) is true if X is not in list L
notmember(_,[]).
notmember(X,[Y|H]) :- not(X = Y), notmember(X,H).

% maxXList(+list,-xList)
% maxXList(L,ML) is true if L is a list of coordinates (X,Y)
% and ML contains every coordinate of L maximizing X.
maxXList(L,ML) :- maxX(L,X), sublist(eqX(X), L, ML).

% minXList(+list,-xList)
% minXList(L,ML) is true if L is a list of coordinates (X,Y)
% and ML contains every coordinate of L minimizing X.
minXList(L,ML) :- minX(L,X), sublist(eqX(X), L, ML).

% maxYList(+list,-yList)
% maxYList(L,ML) is true if L is a list of coordinates (X,Y)
% and ML contains every coordinate of L maximizing Y.
maxYList(L,ML) :- maxY(L,Y), sublist(eqY(Y), L, ML).

% minYList(+list,-yList)
% minYList(L,ML) is true if L is a list of coordinates (X,Y)
% and ML contains every coordinate of L minimizing Y.
minYList(L,ML) :- minY(L,Y), sublist(eqY(Y), L, ML).


% maxX(+list,-absc)
% maxX(L,X0) is true if L is a list of coordinates (X,Y)
% and X0 is the maximum value of X.
maxX([(X,_)],X).
maxX([(X,_)| H],X) :- maxX(H,X2), X >= X2.
maxX([(X,_)| H],X2) :- maxX(H,X2), X < X2.

% minX(+list,-absc)
% minX(L,X0) is true if L is a list of coordinates (X,Y)
% and X0 is the minimum value of X.
minX([(X,_)],X).
minX([(X,_)| H],X) :- minX(H,X2), X =< X2.
minX([(X,_)| H],X2) :- minX(H,X2), X > X2.

% maxY(+list,-ord)
% maxY(L,Y0) is true if L is a list of coordinates (X,Y)
% and Y0 is the maximum value of Y.
maxY([(_,Y)],Y).
maxY([(_,Y)| H],Y) :- maxY(H,Y2), Y >= Y2.
maxY([(_,Y)| H],Y2) :- maxY(H,Y2), Y < Y2.

% minY(+list,-ord)
% minY(L,Y0) is true if L is a list of coordinates (X,Y)
% and Y0 is the minimum value of Y.
minY([(_,Y)],Y).
minY([(_,Y)| H],Y) :- minY(H,Y2), Y =< Y2.
minY([(_,Y)| H],Y2) :- minY(H,Y2), Y > Y2.

% eqX(+absc, +coordinate)
% eqX(X,(X1,Y1)) is true if X = X1.
eqX(X,(X,_)).

% eqY(+ord, +coordinate)
% eqY(Y,(X1,Y1)) is true if Y = Y1.
eqY(Y,(_,Y)).

% maxXElem(+list,-absc)
% maxXElem(L,X0,Y0) is true if L is a list of coordinates (X,Y)
% each containing a unique X, if L contains (X0,Y0)
% and X0 is the maximum value of X.
maxXElem([(X,Y)],X,Y).
maxXElem([(X,Y)| H],X,Y) :- maxXElem(H,X2,_), X >= X2.
maxXElem([(X,_)| H],X2,Y2) :- maxXElem(H,X2,Y2), X < X2.

% minXElem(+list,-absc)
% minXElem(L,X0,Y0) is true if L is a list of coordinates (X,Y)
% each containing a unique X, if L contains (X0,Y0)
% and X0 is the minimum value of X.
minXElem([(X,Y)],X,Y).
minXElem([(X,Y)| H],X,Y) :- minXElem(H,X2,_), X =< X2.
minXElem([(X,_)| H],X2,Y2) :- minXElem(H,X2,Y2), X > X2.

% minYElem(+list,-ord)
% minYElem(L,X0,Y0) is true if L is a list of coordinates (X,Y)
% each containing a unique Y, if L contains (X0,Y0)
% and Y0 is the maximum value of Y.
maxYElem([(X,Y)],X,Y).
maxYElem([(X,Y)| H],X,Y) :- maxYElem(H,_,Y2), Y >= Y2.
maxYElem([(_,Y)| H],X2,Y2) :- maxYElem(H,X2,Y2), Y < Y2.

% minYElem(+list,-ord)
% minYElem(L,X0,Y0) is true if L is a list of coordinates (X,Y)
% each containing a unique Y, if L contains (X0,Y0)
% and Y0 is the minimum value of Y.
minYElem([(X,Y)],X,Y).
minYElem([(X,Y)| H],X,Y) :- minYElem(H,_,Y2), Y =< Y2.
minYElem([(_,Y)| H],X2,Y2) :- minYElem(H,X2,Y2), Y > Y2.


% pile(-list)
% pile(L) is true if L is the current pile of the interpreter.
:- dynamic(pile/1).
pile([]).

% dp(-directionPointer)
% dp(DP) is true if DP is the current direction pointer of the interpreter.
:- dynamic(dp/1).
dp(dright).

% direction(?directionPointer, ?index)
% direction(DP,I) is true if I is the index of the direction pointer DP.
direction(dright,0).
direction(dbottom,1).
direction(dleft,2).
direction(dtop,3).

% nextDirectionPointer(+directionPointer, -nextDirectionPointer)
% nextDirectionPointer(DP,NDP) is true if NDP is the direction pointer following clockwise DP.
nextDirectionPointer(IDP,NDP) :- direction(IDP,IDi), NDi is (IDi + 1) mod 4, direction(NDP,NDi).

% previousDirectionPointer(+directionPointer, -previousDirectionPointer)
% previousDirectionPointer(DP,NDP) is true if NDP is the direction pointer preceding clockwise DP.
previousDirectionPointer(IDP,NDP) :- direction(IDP,IDi), NDi is (IDi - 1) mod 4, direction(NDP,NDi).

% cc(-codelChooser)
% cc(CC) is true if CC is the current codel chooser of the interpreter.
:- dynamic(cc/1).
cc(cleft).

% codelchooser(?codelChooser, ?index)
% codelchooser(CC,I) is true if I is the index of the codel chooser CC.
codelchooser(cleft,0).
codelchooser(cright,1).

% toggleCodelChooser(+codelChooser, -nextcodelChooser)
% toggleCodelChooser(CC,NCC) is true if NCC is the codel chooser following CC.
toggleCodelChooser(ICC,NCC) :- codelchooser(ICC,ICCi), NCCi is (ICCi + 1) mod 2, codelchooser(NCC,NCCi).

% nextpointer(+directionPointer,+codelChooser,-nextDirectioNPointer,-nextCodelChooser)
% nextpointer(DP,CC,NDP,NCC) is true if starting with direction pointer DP and codel chooser CC, the interpreter,
% encountering a black block, change the two values DP and CC in NDP and NCC (the CC is toggled, if NCCis cleft, then
% DP is turned clockwise).
nextpointer(IDP, cright, NDP, cleft) :- nextDirectionPointer(IDP,NDP).
nextpointer(DP, cleft, DP, cright).

% moveDp(+number)
% moveDp(V) is true if the direction pointer is moved clockwise V times.
moveDp(V) :- V < 0, retract(dp(DP)), previousDirectionPointer(DP,NDP), assert(dp(NDP)), V1 is V + 1, moveDp(V1),!.
moveDp(V) :- V > 0, retract(dp(DP)), nextDirectionPointer(DP,NDP), assert(dp(NDP)), V1 is V - 1, moveDp(V1),!.
moveDp(0).

% toggleCC(+number)
% toggleCC(V) is true if the codel chooser if toggled V times.
toggleCC(V) :- VM2 is V mod 2, VM2 == 1, retract(cc(CC)), toggleCodelChooser(CC,NCC), assert(cc(NCC)),!.
toggleCC(_).


% curs(-absc, -ord)
% curs(X,Y) is true if (X,Y) are the current coordinates of the cursor of the interpretor.
:- dynamic(curs/2).
curs(1,5).


% operation(?operation, ?huediff, ?lightnessdiff)
% operation(OP,DH,DL) is true if command OP corresponds to the step difference DH, and
% the darkness difference DL.
operation(onone, 0,0).
operation(opush, 0,1).
operation(opop, 0,2).
operation(oadd, 1,0).
operation(osubtract, 1,1).
operation(omultiply, 1,2).
operation(odivide, 2,0).
operation(omod, 2,1).
operation(onot, 2,2).
operation(ogreater, 3,0).
operation(opointer, 3,1).
operation(oswitch, 3,2).
operation(oduplicate, 4,0).
operation(oroll, 4,1).
operation(oinnumber, 4,2).
operation(oinchar, 5,0).
operation(ooutnumber, 5,1).
operation(ooutchar, 5,2).

% applyOp(+operation, +blocvalue)
% applyOp(OP, V) is true if the command OP is applied with current bloc value V.
applyOp(opush,V) :- retract(pile(L)), assert(pile([V|L])),!.
applyOp(opop,_) :- retract(pile([_|H])), assert(pile(H)),!.
applyOp(oadd,_) :- retract(pile([A,B|H])), C is B + A, assert(pile([C|H])),!.
applyOp(osubtract,_) :- retract(pile([A,B|H])), C is B - A, assert(pile([C|H])),!.
applyOp(omultiply,_) :- retract(pile([A,B|H])), C is B * A, assert(pile([C|H])),!.
applyOp(odivide,_) :- retract(pile([A,B|H])), C is B // A, assert(pile([C|H])),!.
applyOp(omod,_) :- retract(pile([A,B|H])), C is B mod A, assert(pile([C|H])),!.
applyOp(onot,_) :- retract(pile([0|H])), assert(pile([1|H])),!.
applyOp(onot,_) :- pile([V|H]), 0\=V, retract(pile([V|H])), assert(pile([0|H])),!.
applyOp(ogreater,_) :- pile([A,B|H]), B >= A, retract(pile([A,B|H])), assert(pile([1|H])),!.
applyOp(ogreater,_) :- pile([A,B|H]), B < A, retract(pile([A,B|H])), assert(pile([0|H])),!.
applyOp(opointer,_) :- retract(pile([V|H])), moveDp(V), assert(pile(H)),!.
applyOp(oswitch,_) :- retract(pile([V|H])), toggleCC(V), assert(pile(H)),!.
applyOp(oduplicate,_) :- retract(pile([V|H])), assert(pile([V,V|H])),!.
applyOp(oroll,_) :- retract(pile([A,B|H])), roll(H,A,B,H2), assert(pile(H2)),!.
applyOp(ooutnumber,_) :- retract(pile([V|H])), write(V), assert(pile(H)),!.
applyOp(oinnumber,_) :- retract(pile(L)), read(V), assert(pile([V|L])),!.
applyOp(ooutchar,_) :- retract(pile([V|H])), put(V), assert(pile(H)),!.
applyOp(oinchar,_) :- retract(pile(L)), read(X), char_code(X,V), assert(pile([V|L])),!.
applyOp(_,_).


% roll(+list, +depth, +numberOfTurn, list2)
% roll(L1,N,M,L2) is true if L2 is the list L1 where the N first elements
% are rolled M times. One roll is moving the first elements from index 0 to index N-1.
roll(L,N,M,LR) :- firstElems(L,N,L1,L2), roll(L1,M,L1R), append(L1R,L2,LR),!.

% roll(+list, +numberOfTurn, list2)
% roll(L1,M,L2) is true if L2 is the list L1 where every elements
% are rolled M times. One roll is moving the first elements from index 0 to last index.
roll([],_,[]).
roll(L,M,L) :-
  length(L,S),
  W is M mod S,
  W == 0 .
roll([V|H],M,L2) :-
  length([V|H],S),
  W is M mod S , W \= 0,
  M1 is M - 1,
  append(H,[V],L1),
  roll(L1,M1,L2).

% firstElems(+list,+size,+list1,+list2)
% firstElems(L,N,L1,L2) is true if L1 is the sublist of L containings its first N elements
% and L2 are the others elements.
firstElems([],_,[],[]).
firstElems(L,N,[],L) :- N < 0 .
firstElems(L,N,L1,L2) :- append(L1,L2,L), length(L,S), M is min(N,S), length(L1,M).


% next
% next is true if the interpreter apply the next command of the input program.
next :-
  curs(X,Y),
  program(X,Y,white),
  dp(DP), cc(CC),
  outblocWhite(X,Y,DP,CC,X2,Y2,NDP,NCC),
  retract(dp(DP)), retract(cc(CC)),
  assert(dp(NDP)), assert(cc(NCC)),
  retract(curs(X,Y)),
  assert(curs(X2,Y2)),!.
next :-
  curs(X,Y),
  program(X,Y,C),
  neq(C,white),
  bloc(X,Y,B),
  length(B,V),
  nextOp(B,C,DP,CC,O,X2,Y2,NDP,NCC),
  retract(dp(DP)), retract(cc(CC)),
  assert(dp(NDP)), assert(cc(NCC)),
  retract(curs(X,Y)),
  assert(curs(X2,Y2)),
  applyOp(O,V),!.

% readProgram
% readProgram is true if the input program is interpreted.
readProgram :- next, readProgram,!.
readProgram.


% program(?absc,?ord, ?color)
% program(X,Y,C) is true if the input program contains at coordinate (X,Y) a codel with color C.
program(0,_,black).
program(_,0,black).
program(4,_,black).
program(_,6,black).
program(1,5,lred).
program(2,5,lred).
program(3,5,red).
program(1,4,black).
program(2,4,black).
program(3,4,white).
program(1,3,magneta).
program(2,3,black).
program(3,3,white).
program(1,2,magenta).
program(2,2,lred).
program(3,2,white).
program(1,1,magenta).
program(2,1,black).
program(3,1,black).
