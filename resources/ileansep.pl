%% Version:  1.00   -   Date:  18/01/2005   -   File: ileansep.pl
%%
%% Purpose: ileanSeP: An Intuitionistic Sequent Theorem Prover
%%
%% Author:  Jens Otten
%% Web:     www.leancop.de/ileansep/
%%
%% Usage:   prove(F).   % where F is a first-order formula
%%                      %  e.g. F=(all X: ex Y:(~q,p(Y)=>p(X)))
%%
%% Copyright: (c) 2005 by Jens Otten
%% License:   GNU General Public License


:- lib(iso).
:- op(1130, xfy, <=>). :- op(1110, xfy, =>). :- op(500, fy, ~).
:- op( 500,  fy, all). :- op( 500,  fy, ex). :- op(500,xfy, :).

%%% prove formula

prove(F) :- Time1 is cputime, prove(F,1),
            Time2 is cputime, Time is Time2-Time1, print(Time).

prove(F,VLim) :- display(VLim), nl, prove([],[F],l,[],VLim).
prove(F,VLim) :- \+ no_mul([([F],0)]), VLim1 is VLim+1, prove(F,VLim1).

%%% check multiplicities

no_mul([]).
no_mul([([F],Pol)|T]) :- (F=(A,B);F=(A;B)) ->
                         !, no_mul([([A],Pol),([B],Pol)|T]).
no_mul([([F],Pol)|T]) :- fml(F,Pol,_,L1,R1,L2,R2,_,_,V,U,U), !,
                         V==[], no_mul([(L1,1),(R1,0),(L2,1),(R2,0)|T]).
no_mul([_|T]):- no_mul(T).

%%% specification of inference rules
% fml(formula, polarity, invertible/noninvertible, add to left side
%     of 1st premise, right side of 1st premise, add to left side
%     of 2nd premise, right side of 2nd premise, position, free
%     variables, new free variable, term to be copied, copy of term)

fml((A,B),  1,inv,[A,B],            [], [], [], _, _,[], [], [] ).
fml((A,B),  0,inv,[],               [A],[], [B],_, _,[], [], [] ).
fml((A;B),  1,inv,[A],              [], [B],[], _, _,[], [], [] ).
fml((A;_),  0,nin,[],               [A],[], [], _, _,[], [], [] ).
fml((_;B),  0,nin,[],               [B],[], [], _, _,[], [], [] ).
fml((A=>B), 1,nin,[(A=>B)],         [C],[D],[], _, _,[!],A:B,C:D).
fml((A=>B), 0,inv,[A],              [B],[], [], _, _,[], [], [] ).
fml((A<=>B),1,inv,[((A=>B),(B=>A))],[], [], [], _, _,[], [], [] ).
fml((A<=>B),0,inv,[], [((A=>B),(B=>A))],[], [], _, _,[], [], [] ).
fml((~A),   1,nin,[~A],             [C],[], [], _, _,[!], A, C  ).
fml((~A),   0,inv,[A],              [], [], [], _, _,[], [], [] ).
fml(all X:A,1,nin,[C,all X:A],      [], [], [], _, _,[Y],X:A,Y:C).
fml(all X:A,0,inv,[],               [C],[], [], S,FV,[],(X,A),(S^FV,C)).
fml(ex X:A, 1,inv,[C],              [], [], [], S,FV,[],(X,A),(S^FV,C)).
fml(ex X:A, 0,nin,[],               [C],[], [], _, _,[Y],X:A,Y:C).

%%% proof search
% prove(left side, right side, position, free variables, variable limit)

prove(Left,Right,S,FreeV,VarLim) :-
    ( (append(LeftA,[F|LeftB],Left), Pol=1, append(LeftA,LeftB,LeftP),
       RightP=Right ; [F]=Right, Pol=0, LeftP=Left, RightP=[]),
      fml(F,Pol,inv,L1,R1,L2,R2,S,FreeV,V,Cpy,Cpy1), ! ;
      (append(LeftA,[F|LeftB],Left), Pol=1, append(LeftA,LeftB,LeftP),
       RightP=Right ; [F]=Right, Pol=0, LeftP=Left, RightP=[]),
      fml(F,Pol,nin,L1,R1,L2,R2,S,FreeV,V,Cpy,Cpy1)
    ),
    ( V=[] -> true ; \+ length(FreeV,VarLim) ),
    copy_term((Cpy,FreeV),(Cpy1,FreeV)), append(FreeV,V,FreeV1),
    append(LeftP,L1,Left1), ( R1=[] -> Right1=RightP ; Right1=R1 ),
    append(LeftP,L2,Left2), ( R2=[] -> Right2=RightP ; Right2=R2 ),
    prove(Left1,Right1,l(S),FreeV1,VarLim),
    ( L2=[],R2=[] -> true ; prove(Left2,Right2,r(S),FreeV1,VarLim) ).

prove(Left,Right,_,_,_) :-
    member(F,Left), unify_with_occurs_check([F],Right).
