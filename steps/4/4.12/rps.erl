-module(rps).
-author("sgobotta").
-include_lib("eunit/include/eunit.hrl").
-export([
  play/1, play_two/3, val/1,tournament/2,const/1,enum/1,get_strategies/0,
  no_repeat/1,rock/1,cycle/1,rand/1,echo/1,least_frequent/1,most_frequent/1,
  random_strategy/1, best_scored/1,
  main_test_game/0
]).
-import(utils, [least_frequents/1,most_frequents/1,take/2]).

-type play()           :: rock | paper | scissors.
-type outcome()        :: win | lose | draw.
-type strategy()       :: function().
-type strategy_name()  :: rock | echo | no_repeat | cycle | rand | echo | least_frequent | most_frequent | random_strategy | best_scored.
-type strategy_score() :: {rock | echo | no_repeat | cycle | rand | echo | least_frequent | most_frequent | random_strategy | best_scored, integer()}.

-spec main_test_game() -> ok.
main_test_game() ->
  play_two(best_scored(maps:to_list(get_strategies())), fun rand/1, 20).

%% @doc Plays one strategy against another, for N moves.
-spec play_two(strategy(), strategy(), integer()) -> ok.
play_two(StrategyL,StrategyR,N) ->
  print_play_two_header(integer_to_list(N)),
  play_two(StrategyL,StrategyR,[],[],N,1,0).

%% @doc Given two strategies, two list of moves, a maximum number of rounds, a
%% current round and a score, plays rps using Tail recursive loop for play_two/3.
-spec play_two(strategy(), strategy(), [play()], [play()], integer(), integer(), integer()) -> ok.
play_two(_,_,_PlaysL,_PlaysR,0,_RoundN,Score) ->
  print_overall_result(Score);
play_two(_StrategyL,_StrategyR,_PlaysL,_PlaysR,N,_RoundN,Score) when ((Score + N) < 0) ->
  print_overall_result(Score);
play_two(_StrategyL,_StrategyR,_PlaysL,_PlaysR,N,_RoundN,Score) when ((Score - N) > 0) ->
  print_overall_result(Score);
play_two(StrategyL,StrategyR,PlaysL,PlaysR,N,RoundN,Score) ->
  PlayL = StrategyL(PlaysR),
  PlayR = StrategyR(PlaysL),
  PlayResult = result(PlayL,PlayR),
  print_play(PlayL,PlayR,RoundN,print_play_result(PlayResult)),
  play_two(StrategyL, StrategyR, [PlayL|PlaysL], [PlayR|PlaysR], N-1, RoundN+1,Score+outcome(PlayResult)).

%% @doc Interactively play against a strategy, provided as argument.
-spec play(strategy()) -> ok.
play(Strategy) ->
  print_play_header(),
  io:format("Rock - paper - scissors~n"),
  io:format("Play one of rock, paper, scissors, ...~n"),
  io:format("... r, p, s, stop, followed by '.'~n"),
  play(Strategy,[],[],1).

%% @doc Given a strategy, a list of PlayerL plays, a list of Player plays and
%% a round number, plays a rps game using a tail recursive loop for play/1.
-spec play(strategy(), [play()], [play()], integer()) -> ok.
play(Strategy,Moves,OpponentMoves,RoundN) ->
  {ok,P} = io:read("Play: "),
  Play = expand(P),
  case Play of
    stop ->
      io:format("Stopped~n~n"),
      print_overall_result(tournament(Moves, OpponentMoves));
    _    ->
      OpponentMove = Strategy(Moves),
      print_play(Play, OpponentMove, RoundN, print_play_result(result(Play,OpponentMove))),
      play(Strategy,[Play|Moves],[OpponentMove|OpponentMoves],RoundN+1)
  end.

%
% Print functions
%

-spec wrap_text(string(), integer(), atom()) -> [string()].
wrap_text(Text, CellLength, Alignment) ->
  ["║", string:pad(Text, CellLength, Alignment), "║"].

-spec print_row([string()], integer(), string(), string(), string()) -> [string()].
print_row(Cells, N, FirstChar, ColumnSeparator, LastChar) ->
  print_row(Cells, N, FirstChar, ColumnSeparator, LastChar, "").

-spec print_row([string()], integer(), string(), string(), string(), [string()]) -> [string()].
print_row([X|[]],N,FirstChar,_ColumnSeparator,LastChar,Acc) ->
  [FirstChar, Acc, string:pad(X, N, both), LastChar];
print_row([X|Xs],N,FirstChar,ColumnSeparator,Lastchar, Acc) ->
  print_row(Xs, N, FirstChar, ColumnSeparator, Lastchar, Acc ++ string:pad(X, N, both) ++ ColumnSeparator).

-spec print_play_header() -> ok.
print_play_header() ->
  StartHeader       = ["╔", lists:duplicate(91, "═"), "╗\n"],
  Title             = [wrap_text("Rock, Paper, Scissors", 91, both), "\n"],
  Hint              = [wrap_text("Play one of rock, paper, scissors typing: r, p, s, stop, followed by '.'", 91, both), "\n"],
  BlankLine         = [wrap_text("", 91, trailing), "\n"],
  EndHeader         = ["╚", lists:duplicate(91, "═"), "╝\n"],
  io:format("~ts", [[StartHeader, Title, BlankLine, Hint, BlankLine, EndHeader]]).

-spec print_play_two_header(integer()) -> ok.
print_play_two_header(RoundsNumber) ->
  StartTable        = ["╔", lists:duplicate(91, "═"), "╗\n"],
  Title             = [wrap_text("Rock, Paper, Scissors", 91, both), "\n"],
  BlankLine         = [wrap_text("", 91, trailing), "\n"],
  Rounds            = [wrap_text([RoundsNumber, " Rounds will be played."], 91, both), "\n"],
  HeaderSDecoration = [print_row(lists:duplicate(4, lists:flatten(lists:duplicate(22, "═"))), 22, "╠", "╦", "╣"), "\n"],
  HeaderEDecoration = [print_row(lists:duplicate(4, lists:flatten(lists:duplicate(22, "═"))), 22, "╚", "╩", "╝"), "\n"],
  HeaderTitles      = ["Round", "PlayerL", "PlayerR", "Round Winner"],
  Header            = [print_row(HeaderTitles, (88 div 4), "║", "║", "║"), "\n"],
  io:format("~ts", [[StartTable, Title, BlankLine, Rounds, BlankLine, HeaderSDecoration, Header, HeaderEDecoration]]).

%% @doc Given a score, prints out the overall result.
-spec print_overall_result(integer()) -> ok.
print_overall_result(Score) ->
    case Score of
      0 -> print_result("It's a draw game.");
      _ ->
        case Score > 0 of
          true -> print_result("Player L wins!");
          false -> print_result("Player R wins!")
        end
      end,
    io:format("~nEnd of game.~n").

-spec print_play(play(), play(), integer(), string()) -> ok.
print_play(PlayL, PlayR, RoundN, Result) ->
  Message = [print_row([integer_to_list(RoundN), get_unicode(PlayL), get_unicode(PlayR), Result], (88 div 4), " ", " ", " "), "\n"],
  io:format("~ts", [Message]).

-spec print_result(string()) -> ok.
print_result(Text) ->
  StartTable        = ["╔", lists:duplicate(91, "═"), "╗\n"],
  Result            = [wrap_text(Text, 91, both), "\n"],
  EndTable          = ["╚", lists:duplicate(91, "═"), "╝\n"],
  io:format("~ts", [[StartTable, Result, EndTable, "\n"]]).

-spec print_play_result(outcome()) -> string().
print_play_result(draw) ->
  "Draw";
print_play_result(win) ->
  "PlayerL scores!";
print_play_result(lose) ->
  "PlayerR scores!".

%% @doc Given a play() returns a representation character of that play.
-spec get_unicode(play()) -> string().
get_unicode(rock) -> "r ✊";
get_unicode(paper) -> "p ✋";
get_unicode(scissors) -> [<<"s ✌"/utf8>>, <<6765/utf8>>].

%
% Auxiliary functions
%

%% @doc Transforms shorthand atoms to expanded form.
-spec expand(atom()) -> play().
expand(r) -> rock;
expand(p) -> paper;		    
expand(s) -> scissors;
expand(X) -> X.

% @doc Returns the result of one set of plays.
-spec result(play(), play()) -> outcome().
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
-spec outcome(outcome()) -> integer().
outcome(win)  ->  1;
outcome(lose) -> -1;
outcome(draw) ->  0.

%% @doc Transforms 0, 1, 2 to rock, paper, scissors.
-spec enum(integer()) -> play().
enum(0) ->
  rock;
enum(1) ->
  paper;
enum(2) ->
  scissors.

%% @doc Transforms rock, paper, scissors to 0, 1, 2.
-spec val(play()) -> integer().
val(rock) ->
  0;
val(paper) ->
  1;
val(scissors) ->
  2.

%% @doc Gives the play which the argument beats.
-spec beats(play()) -> play().
beats(rock) ->
  scissors;
beats(paper) ->
  rock;
beats(scissors) ->
  paper.

%% @doc Returns the play the argument looses by
-spec loose(play()) -> play().
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
-spec get_strategies() -> map().
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
-spec echo([play()]) -> play().
echo([]) ->
  paper;
echo([Last|_]) ->
  Last.

%% @doc Doesn't matter the opponent's move, will always play rock.
-spec rock([play()]) -> play().
rock(_) ->
  rock.

%% @doc Given a list of moves returns a move that beats the last opponent's play.
-spec no_repeat([play()]) -> play().
no_repeat([]) ->
  paper;
no_repeat([X|_]) ->
  beats(X).

const(_Play) ->
  dummy.

%% @doc Given a list of moves returns a move acoording to the number of moves.
%% played so far.
-spec cycle([play()]) -> play().
cycle(Xs) ->
  enum(length(Xs) rem 3).

%% @doc Given a list of moves returns a randomly taken play.
-spec rand([play()]) -> play().
rand(_) ->
  enum(rand:uniform(3) - 1).

%% @doc Given a list of moves returns the play that beats the opponent's least
%% used play.
-spec least_frequent([play()]) -> play().
least_frequent(Xs) ->
  modes(Xs, fun utils:least_frequents/1, fun beats/1).

%% @doc Given a list of moves returns the play that looses against the opponent's
%% most used play.
-spec most_frequent([play()]) -> play().
most_frequent(Xs) ->
  modes(Xs, fun utils:most_frequents/1, fun loose/1).

%% @doc Generic function used by most_frequent/1 and least_frequent/1 functions.
%% Given a list of moves, a function that finds the most frequent or least
%% frequent move and a beats/1 or loose/1 function returns a suitable play.
-spec modes([play()], function(), strategy()) -> play().
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
-spec random_strategy([play()]) -> play().
random_strategy(OpponentMoves) ->
  Strategies = maps:to_list(get_strategies()),
  {_, Strategy} = lists:nth(rand:uniform(length(Strategies)), Strategies),
  Strategy(OpponentMoves).

%% @doc Given a list of strategies, returns a function that takes a list of
%% moves to return a move according to the best scored strategy.
-spec best_scored([play()]) -> strategy().
best_scored(Strategies) ->
  fun (OpponentMoves) ->
    [StrategiesResultHead | StrategiesResultTail] = lists:map(
      fun (StrategyName) ->
        get_strategy_score(StrategyName, OpponentMoves)
      end,
      Strategies
    ),
    {BestScoredStrategyName, _Score} = get_best_scored_strategy(StrategiesResultTail, StrategiesResultHead),
    BestScoredStrategy = maps:get(BestScoredStrategyName, maps:from_list(Strategies)),
    BestScoredStrategy(OpponentMoves)
  end.

%% @doc Given a list of strategy_score(), returns the best scored strategy_score().
-spec get_best_scored_strategy([strategy_score()], strategy_score()) -> strategy_score().
get_best_scored_strategy([], Best) ->
  Best;
get_best_scored_strategy([{_StrategyName, Score} = S |Xs], {_BestStrategy, BestScore} = B) ->
  case Score > BestScore of
    true  -> get_best_scored_strategy(Xs, S);
    false -> get_best_scored_strategy(Xs, B)
  end.

get_best_scored_strategy_test() ->
  StrategiesScores = [{rock, -2}, {echo, 2}, {least_frequent, -1}, {rand, 0}],
  ExpectedResult = {echo, 2},
  ?assertEqual(ExpectedResult, get_best_scored_strategy(tl(StrategiesScores), hd(StrategiesScores))).

%% @doc Given a strategy name with it's function and a list of moves, returns a
%% score.
-spec get_strategy_score({strategy_name(), strategy()}, [play()]) -> strategy_score().
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
-spec test_strategy(strategy(), [play()]) -> [play()].
test_strategy(Strategy, Xs) ->
  test_strategy(Strategy, lists:reverse(Xs), [], []).

%% @doc Given a strategy, a list of moves and two accumulators, one that
%% reconstructs the opponent moves and the other accumulates the strategy plays,
%% and returns the last accumulator.
%% The head of the list of moves should be the opponent's first movement,
%% meaning that in some cases the given list should sometimes be reversed.
-spec test_strategy(strategy(), [play()], [play()], [play()]) -> [play()].
test_strategy(_Strategy, [], _OpponentMoves, Acc) ->
  Acc;
test_strategy(Strategy, [X|Xs], OpponentMoves, Acc) ->
  test_strategy(Strategy, Xs, [X|OpponentMoves], [Strategy(OpponentMoves) | Acc]).

test_strategy_using_rock_strategy_test() ->
  OpponentMoves = [scissors,paper,rock],
  ?assertEqual(
    [rock,rock,rock],
    test_strategy(fun rock/1, OpponentMoves)
  ).

test_strategy_using_echo_strategy_test() ->
  OpponentMoves = [scissors,paper,rock],
  EchoDefaultMove = paper,
  ?assertEqual(
    [paper,rock,EchoDefaultMove],
    test_strategy(fun echo/1, OpponentMoves)
  ).

test_strategy_using_least_frequent_strategy_test() ->
  OpponentMoves = [scissors,paper,rock,rock,rock,paper],
  ?assertEqual(
    [paper,paper,paper,paper],
    % We take the first 4 elements, because the first ones are randomly
    % computed by the strategy.
    utils:take(4, test_strategy(fun least_frequent/1, OpponentMoves))
  ).

test_strategy_using_most_frequent_strategy_test() ->
  OpponentMoves = [rock,paper,scissors,rock,paper,paper],
  ?assertEqual(
    [scissors,scissors,scissors,scissors,scissors],
    % We take the first 5 elements, because the first one is randomly
    % computed by the strategy.
    utils:take(5, test_strategy(fun most_frequent/1, OpponentMoves))
  ).
