:- begin_tests(typeInf).
:- include(typeInf). 

/* Note: when writing tests keep in mind that 
    the use of of global variable and function definitions
    define facts for gvar() predicate. Either test
    directy infer() predicate or call
    delegeGVars() predicate to clean up gvar().
*/

% tests for typeExp
test(typeExp_iplus) :- 
    typeExp(iplus(int,int), int).

% this test should fail
test(typeExp_iplus_F, [fail]) :-
    typeExp(iplus(int, int), float).

test(typeExp_iplus_T, [true(T == int)]) :-
    typeExp(iplus(int, int), T).

test(typeExp_print_T, [true(T == unit)]):- 
    typeExp(printStatement(_Pls), T).

test(typeExp_print_F, [fail]) :-
    typeExp(printStatement("Pls work"), float).

test(typeExp_lessThan_T, [true(T==bool)]):-
    typeExp(lessThan(int,int), T).

test(typeExp_lessThan_F, [fail]):-
    typeExp(lessThan(int,int), float),
    typeExp(lessThan(int,int), int),
    typeExp(lessThan(int,int, unit)).

test(typeExp_greaterThan_T, [true(T==bool)]):-
    typeExp(greaterThan(int,int), T).

test(typeExp_greaterThan_F, [fail]):-
    typeExp(greaterThan(int,int), float),
    typeExp(greaterThan(int,int), int),
    typeExp(lessThan(int,int), unit).

test(typeExp_equal_T, [true(T==bool)]):-
    typeExp(greaterThan(int,int), T).

test(typeExp_equal_F, [fail]):-
    typeExp(greaterThan(int,int), float),
    typeExp(greaterThan(int,int), int),
    typeExp(lessThan(int,int), unit).

test(typeExp_notEqual_T, [true(T==bool)]):-
    typeExp(notEqual(int,int), T).

test(typeExp_notEqual_F, [fail]):-
    typeExp(greaterThan(int,int), float),
    typeExp(greaterThan(int,int), int),
    typeExp(lessThan(int,int), unit).

test(typeExp_fplus_T, [true(T==float)]):-
    typeExp(fplus(float,float), T).

test(typeExp_fplus_F, [fail]):-
    typeExp(fplus(float,float), int),
    typeExp(fplus(float,float), unit).

test(typeStatement_ifStatement_T, [true(T == int)]):-
    typeStatement(ifStatement(greaterThan(int,int), iplus(int,int)),T).

test(typeStatement_ifStatement_F, [fail]):-
    typeStatement(ifStatement(iplus(int,int), iToFloat(int)),T).

test(typeStatement_ifElseStatement_T, [true(T == int)]:-
    typeStatement(ifElseStatement(greaterThan(int,int), iplus(int,int), iplus(int,int)),T).
% NOTE: use nondet as option to test if the test is nondeterministic

% test for statement with state cleaning
test(typeStatement_gvar, [nondet]) :- % should succeed with T=int
    deleteGVars(), /* clean up variables */
    typeStatement(gvLet(v, T, iplus(X, Y)), unit),
    assertion(X == int), assertion( Y == int), % make sure the types are int
    gvar(v, int), % make sure the global variable is defined
    assertion(T==int).

% same test as above but with infer 
test(infer_gvar, [nondet]) :-
    infer([gvLet(v, T, iplus(X, Y))], unit),
    assertion(T==int), assertion(X==int), assertion(Y=int),
    gvar(v,int).

% test custom function with mocked definition
test(mockedFct, [nondet]) :-
    deleteGVars(), % clean up variables since we cannot use infer
    asserta(gvar(my_fct, [int, float])), % add my_fct(int)-> float to the gloval variables
    typeExp(my_fct(X), T), % infer type of expression using or function
    assertion(X==int), assertion(T==float). % make sure the types infered are correct

:-end_tests(typeInf).
