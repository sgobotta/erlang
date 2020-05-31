-module(rps).
-author("sgobotta").
-export([
  play/1, play_two/3, val/1,tournament/2,const/1,enum/1,
  no_repeat/1,rock/1,cycle/1,rand/1,echo/1,least_frequent/1,most_frequent/1
]).
-import(utils, [least_frequents/1,most_frequents/1]).

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

%% @doc Given a list of moves returns te play that beats the opponent's least
%% used play.
least_frequent(Xs) ->
  modes(Xs, fun utils:least_frequents/1, fun beats/1).

%% @doc Given a list of moves returns te play that looses against the opponent's
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
  ChoosePlay(lists:nth(rand:uniform(length(Modes)), Modes)).
