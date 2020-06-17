-module(expr).
-export([
  print/1,
  eval/2,
  compile/1, run/3,
  parse/1,
  simplify/1,
  expr2/0,
  env/0
]).
-include_lib("eunit/include/eunit.hrl").

-type expr() :: {'num',integer()}
             |  {'var',atom()}
             |  {'add',expr(),expr()}
             |  {'mul',expr(),expr()}.

% Expression - expr2
% ((1*b) + (((2*b) + (1*b)) * 0))
expr2() ->
  {add,{mul,{num,1},{var,b}},
       {mul,{add,{mul,{num,2},{var,b}},{mul,{num,1},{var,b}}},
            {num,0}}}.

env() ->
  [{b,2}].

%% Pretty printing

-spec print(expr()) -> string().
print({num,N}) ->
  integer_to_list(N);
print({var,A}) ->
  atom_to_list(A);
print({add,E1,E2}) ->
  "(" ++ print(E1) ++ "+" ++ print(E2) ++ ")";
print({mul,E1,E2}) ->
  "(" ++ print(E1) ++ "*" ++ print(E2) ++ ")".

print_test() ->
  ?_assertEqual("(2+(3*4))", print({add,{num,2},{mul,{num,3},{num,4}}})),
  Expression = expr2(),
  ?_assertEqual("((1*b)+(((2*b)+(1*b))*0))", print(Expression)).


%% Expression evaluation

-type env()  :: [{atom(),integer()}].

-spec eval(env(),expr()) -> integer().
eval(_Env,{num,N}) ->
  N;
eval(Env,{var,A}) ->
  lookup(A,Env);
eval(Env,{add,E1,E2}) ->
  eval(Env,E1) + eval(Env,E2);
eval(Env,{mul,E1,E2}) ->
  eval(Env,E1) * eval(Env,E2).

-spec lookup(atom(),env()) -> integer().
lookup(A,[{A,V}|_]) ->
  V;
lookup(A,[_|Rest]) ->
  lookup(A,Rest).

%% Instructions, programs and stacks

-type instr()   :: {'push',integer()}
                |  {'fetch',atom()}
                |  {'add2'}
                |  {'mul2'}.
-type program() :: [instr()].
-type stack() :: [integer()].

-spec compile(expr()) -> program().
compile({num,N}) ->
  [{push, N}];
compile({var,A}) ->
  [{fetch, A}];
compile({add,E1,E2}) ->
  compile(E1) ++ compile(E2) ++ [{add2}];
compile({mul,E1,E2}) ->
  compile(E1) ++ compile(E2) ++ [{mul2}].

% -spec run(program(),env()) -> integer().

-spec run(program(),env(),stack()) -> integer().
run([{push, N} | Continue], Env, Stack) ->
  run(Continue, Env, [N | Stack]);
run([{fetch, A} | Continue], Env, Stack) ->
  run(Continue, Env, [lookup(A,Env) | Stack]);
run([{add2} | Continue], Env, [N1,N2|Stack]) ->
  run(Continue, Env, [(N1+N2) | Stack]);
run([{mul2} | Continue], Env, [N1,N2|Stack]) ->
  run(Continue, Env, [(N1*N2) | Stack]);
run([],_Env,[N]) ->
  N.

%% Parsing

% Tests

parse_test() ->
  ?_assertEqual({{add,{num,2},{mul,{num,3},{num,4}}}, ""}, parse("(2+(3*4))")),
  ?_assertEqual({{num,2}, ""}, parse("2")),
  ?_assertEqual({{num,2}, "+(3*4))"}, parse("2+3(3*4))")),
  % numbers
  ?_assertEqual({{num,-123},")"}, parse("-123")),
  ?_assertEqual({{var,variable},"+3"}, parse("variable+3")),
  ?_assertEqual({var,a}, parse("a")).

-spec parse(string()) -> {expr(), string()}.
parse([$(|Rest]) ->
  {E1,Rest1}      = parse(Rest),
  [Op|Rest2]      = Rest1,
  {E2,Rest3}      = parse(Rest2),
  [$)|RestFinal]  = Rest3,
  {case Op of
    $+ -> {add,E1,E2};
    $* -> {mul,E1,E2}
  end,
  RestFinal};
parse([Ch|Rest]) when $a =< Ch andalso Ch =< $z ->
  {Succeeds,Remainder} = get_while(fun is_alpha/1,Rest),
  {{var, list_to_atom([Ch|Succeeds])}, Remainder}.

is_alpha(Ch) -> $a =< Ch andalso Ch =< $z.

%% @doc Gets the longest initial segment of a list with a given property, where
%% a property is represented as a bolean-valued function.
-spec get_while(fun((T) -> boolean()), [T]) -> {[T],[T]}.
get_while(P,[Ch,Rest]) ->
  case P(Ch) of
    true ->
      {Succeeds,Remainder} = get_while(P,Rest),
      {[Ch|Succeeds],Remainder};
    false ->
      {[],[Ch|Rest]}
    end;
get_while(_P,[]) ->
  {[],[]}.

% Going further
%
% Simplification: (0+(1*v)) simplifies to v.
% More operations: substraction, div/rem, unary minus.
% Setting variables: let v=e1 in e2
% Defining functions for yourself: let f=(\x -> e1) in e2
% Adding other types: if b then e1 else e2
% Changing the syntax: e.g. operator precedense - BODMAS.

% Simplification

zeroA({add,E,{num,0}}) ->
  E;
zeroA({add,{num,0},E}) ->
  E;
zeroA(E) ->
  E.

mulO({mul,E,{num,1}}) ->
  E;
mulO({mul,{num,1},E}) ->
  E;
mulO(E) ->
  E.

mulZ({mul,_,{num,0}}) ->
  {num,0};
mulZ({mul,{num,0},_}) ->
  {num,0};
mulZ(E) ->
  E.

% Given a list of functions applies all of them one to another
compose([]) ->
  fun(E) -> E end;
compose([Rule|Rules]) ->
  fun (E) -> (compose(Rules))(Rule(E)) end.

rules() ->
  [fun zeroA/1, fun mulO/1, fun mulZ/1].

simp(F,{add,E1,E2}) ->
  F({add,simp(F,E1),simp(F,E2)});
simp(F,{mul,E1,E2}) ->
  F({mul,simp(F,E1),simp(F,E2)});
simp(_F, E) ->
  E.

simplify_test() ->
  Expression = expr2(),
  ?assertEqual({var,b}, simplify(Expression)).

simplify(E) ->
  simp(compose(rules()), E).

