-module(rps).
-author("sgobotta").
-include_lib("eunit/include/eunit.hrl").
-export([
  play/1, play_two/3, val/1,tournament/2,const/1,enum/1,get_strategies/0,
  no_repeat/1,rock/1,cycle/1,rand/1,echo/1,least_frequent/1,most_frequent/1,
  random_strategy/1
]).
-import(utils, [least_frequents/1,most_frequents/1,take/2]).

-type play()          :: rock | paper | scissors.
-type strategy()      :: function().
-type strategy_name() :: {rock | echo | no_repeat | cycle | rand | echo | least_frequent | most_frequent | random_strategy, function()}.

%% @doc Plays one strategy against another, for N moves.
play_two(StrategyL,StrategyR,N) ->
  play_two(StrategyL,StrategyR,[],[],N).

%% @doc Tail recursive loop for play_two/3
%% 0 case computes the result of the tournament
play_two(_,_,PlaysL,PlaysR,0) ->
  dummy;

play_two(StrategyL,StrategyR,PlaysL,PlaysR,N) ->
  dummy.

%% @doc Interactively play against a strategy, provided as argument.
play(Strategy) ->
  io:format("Rock - paper - scissors~n"),
  io:format("Play one of rock, paper, scissors, ...~n"),
  io:format("... r, p, s, stop, followed by '.'~n"),
  play(Strategy,[]).

%% @doc Tail recursive loop for play/1
play(Strategy,Moves) ->
  {ok,P} = io:read("Play: "),
  Play = expand(P),
  case Play of
    stop ->
      io:format("Stopped~n");
    _    ->
      Result = result(Play,Strategy(Moves)),
      io:format("Result: ~p~n",[Result]),
      play(Strategy,[Play|Moves])
  end.

%
% Auxiliary functions
%

%% @doc Transforms shorthand atoms to expanded form.
expand(r) -> rock;
expand(p) -> paper;		    
expand(s) -> scissors;
expand(X) -> X.

% @doc Returns the result of one set of plays.
result(rock,rock) -> draw;
result(rock,paper) -> lose;
result(rock,scissors) -> win;
result(paper,rock) -> win;
result(paper,paper) -> draw;
result(paper,scissors) -> lose;
result(scissors,rock) -> lose;
result(scissors,paper) -> win;
result(scissors,scissors) -> draw.

%% @doc Returns the result of a tournament (list of plays).
-spec tournament([play()], [play()]) -> integer().
tournament(PlaysL,PlaysR) ->
  lists:sum(
    lists:map(fun outcome/1,
      lists:zipwith(fun result/2,PlaysL,PlaysR))).

%% @doc Transforms a round outcome to a number, representing a positive number
%% for plays won by the first player and a negative number for the second
%% player.
outcome(win)  ->  1;
outcome(lose) -> -1;
outcome(draw) ->  0.

%% @doc Transforms 0, 1, 2 to rock, paper, scissors.
enum(0) ->
  rock;
enum(1) ->
  paper;
enum(2) ->
  scissors.

%% @doc Transforms rock, paper, scissors to 0, 1, 2.
val(rock) ->
  0;
val(paper) ->
  1;
val(scissors) ->
  2.

%% @doc Gives the play which the argument beats.
beats(rock) ->
  scissors;
beats(paper) ->
  rock;
beats(scissors) ->
  paper.

%% @doc Returns the play the argument looses by
loose(rock) ->
  paper;
loose(paper) ->
  scissors;
loose(scissors) ->
  rock.

%
% strategies.
%

%% @doc Returns a list of strategies
get_strategies() ->
  #{
    echo            => fun echo/1,
    rock            => fun rock/1,
    no_repeat       => fun no_repeat/1,
    cycle           => fun cycle/1,
    rand            => fun rand/1,
    least_frequent  => fun least_frequent/1,
    most_frequent   => fun most_frequent/1
  }.

%% @doc Given a list of moves echoes the last opponent's move.
echo([]) ->
  paper;
echo([Last|_]) ->
  Last.

%% @doc Doesn't matter the opponent's move, will always play rock.
rock(_) ->
  rock.

%% @doc Given a list of moves returns a move that beats the last opponent's play.
no_repeat([]) ->
  paper;
no_repeat([X|_]) ->
  beats(X).

const(Play) ->
  dummy.

%% @doc Given a list of moves returns a move acoording to the number of moves.
%% played so far.
cycle(Xs) ->
  enum(length(Xs) rem 3).

%% @doc Given a list of moves returns a randomly taken play.
rand(_) ->
  enum(rand:uniform(3) - 1).

%% @doc Given a list of moves returns the play that beats the opponent's least
%% used play.
least_frequent(Xs) ->
  modes(Xs, fun utils:least_frequents/1, fun beats/1).

%% @doc Given a list of moves returns the play that looses against the opponent's
%% most used play.
most_frequent(Xs) ->
  modes(Xs, fun utils:most_frequents/1, fun loose/1).

%% @doc Generic function used by most_frequent/1 and least_frequent/1 functions.
%% Given a list of moves, a function that finds the most frequent or least
%% frequent move and a beats/1 or loose/1 function returns a suitable play.
modes([],_,_) ->
  rand([]);
modes(Xs,Mode,ChoosePlay) ->
  Modes = Mode(Xs ++ [paper,rock,scissors]),
  % Modes always return a list of most/least frequent elements, therefore one
  % is choosen randomly, when the algorith is certain, there will only be one
  % element, and will always choose it.
  ChoosePlay(lists:nth(rand:uniform(length(Modes)), Modes)).

%% @doc Given a non empty list of strategy returns a function that given a list
%% of plays chooses a strategy randomly.
random_strategy(Strategies) ->
  fun (Xs) -> 
    (lists:nth(rand:uniform(length(Strategies)), Strategies))(Xs)
  end.

%% @doc Given a list of strategies, returns a function that takes a list of
%% moves to return the best scored strategy.
best_scored(Strategies) ->
  fun (OpponentMoves) ->
    [StrategiesResultHead | StrategiesResultTail] = lists:map(
      fun (StrategyName) ->
        get_strategy_score(StrategyName, OpponentMoves)
      end,
      Strategies
    ),
    {BestScoredStrategy, _Score} = get_best_scored_strategy(StrategiesResultTail, StrategiesResultHead),
    maps:get(BestScoredStrategy, maps:from_list(Strategies))
  end.

best_scored_test() ->
  % Setup
  Strategies = #{rock => fun rock/1, echo => fun echo/1},
  OpponentMoves = [paper, paper, paper],
  ExpectedBestScoredStrategy = maps:get(echo, Strategies),
  % Assertions
  ?assertEqual(
    ExpectedBestScoredStrategy,
    (best_scored(maps:to_list(Strategies)))(OpponentMoves)
  ).

get_best_scored_strategy([], Best) ->
  Best;
get_best_scored_strategy([{StrategyName, Score} = S |Xs], {BestStrategy, BestScore} = B) ->
  case Score > BestScore of
    true  -> get_best_scored_strategy(Xs, S);
    false -> get_best_scored_strategy(Xs, B)
  end.

get_best_scored_strategy_test() ->
  StrategiesScores = [{rock, -2}, {echo, 2}, {least_frequent, -1}, {rand, 0}],
  ExpectedResult = {echo, 2},
  ?assertEqual(ExpectedResult, get_best_scored_strategy(tl(StrategiesScores), hd(StrategiesScores))).

%% @doc Given a strategy_name() and a list of moves, returns a score.
get_strategy_score({StrategyName, StrategyFunction}, OpponentMoves) ->
  StrategyResults = test_strategy(StrategyFunction, OpponentMoves),
  {StrategyName, tournament(StrategyResults, OpponentMoves)}.

get_strategy_score_test() ->
  OpponentMoves = [paper, paper, paper],
  RockStrategyName = {rock, fun rock/1},
  ?assertEqual({rock, -3}, get_strategy_score(RockStrategyName, OpponentMoves)),
  EchoStrategyName = {echo, fun echo/1},
  ?assertEqual({echo, 0}, get_strategy_score(EchoStrategyName, OpponentMoves)).

%% @doc Given a strategy and a list of moves, returns a list of moves played by
%% the strategy.
test_strategy(Strategy, Xs) ->
  test_strategy(Strategy, Xs, [], []).

%% @doc Given a strategy, a list of moves and two accumulators, one that
%% reconstructs the opponent moves and the other accumulates the strategy plays,
%% and returns the last accumulator.
%% The head of the list of moves should be the opponent's first movement.
test_strategy(Strategy, [], OpponentMoves, Acc) ->
  Acc;
test_strategy(Strategy, [X|Xs], OpponentMoves, Acc) ->
  test_strategy(Strategy, Xs, [X|OpponentMoves], [Strategy(OpponentMoves) | Acc]).

test_strategy_using_rock_strategy_test() ->
  OpponentMoves = lists:reverse([scissors,paper,rock]),
  ?assertEqual(
    [rock,rock,rock],
    test_strategy(fun rock/1, OpponentMoves, [], [])
  ).

test_strategy_using_echo_strategy_test() ->
  OpponentMoves = lists:reverse([scissors,paper,rock]),
  EchoDefaultMove = paper,
  ?assertEqual(
    [paper,rock,EchoDefaultMove],
    test_strategy(fun echo/1, OpponentMoves, [], [])
  ).

test_strategy_using_least_frequent_strategy_test() ->
  OpponentMoves = lists:reverse([scissors,paper,rock,rock,rock,paper]),
  ?assertEqual(
    [paper,paper,paper,paper],
    % We take the first 4 elements, because the first ones are randomly
    % computed by the strategy.
    utils:take(4, test_strategy(fun least_frequent/1, OpponentMoves))
  ).

test_strategy_using_most_frequent_strategy_test() ->
  OpponentMoves = lists:reverse([rock,paper,scissors,rock,paper,paper]),
  ?assertEqual(
    [scissors,scissors,scissors,scissors,scissors],
    % We take the first 5 elements, because the first one is randomly
    % computed by the strategy.
    utils:take(5, test_strategy(fun most_frequent/1, OpponentMoves))
  ).
