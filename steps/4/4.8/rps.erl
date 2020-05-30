-module(rps).
-author("@sgobotta").
-include_lib("eunit/include/eunit.hrl").
-export([]).

-type element() :: rock | paper | scissors.
-type round_result() :: win | lose | draw.
-type tournament_result() :: integer().

% interactively play against a strategy, provided as argument.

play(Strategy) ->
  io:format("Rock - paper - scissors~n"),
  io:format("Play one of rock, paper, scisors, ...~n"),
  io:format("... r, p, s, stop followed by '.'~n"),
  play(Strategy,[]).

% tail recursive loop for play/1

play(Strategy, Moves) ->
  {ok,P} = io:read("Play: "),
  Play = expand(P),
  case Play of
    stop -> io:format("Stopped~n");
    _    ->
      Result = result(Play,Strategy(Moves)),
      io:format("Result: ~p~n", [Result]),
      play(Strategy, [Play|Moves])
  end.

% Win and lose
%
% Now we’ll define some functions to use here. Let’s start by defining beat,
% which for a move tells us which more beats that one.

%% @doc Given an atom tells which one beats it.
beat(rock) -> paper;
beat(paper) -> scissors;
beat(scissors) -> rock.

% Next, define the function lose, which tells us which moves lose if played
% against the argument played … for example,

%% @doc Given an atom tells which move losses by it.
lose(rock) -> scissors;
lose(paper) -> rock;
lose(scissors) -> paper.

% A round
%
% Define a function result which when applied to two plays gives the result,
% from the point of view of the first.

result(rock,     rock)     -> draw;
result(rock,     paper)    -> lose;
result(rock,     scissors) -> win;
result(paper,    rock)     -> win;
result(paper,    paper)    -> draw;
result(paper,    scissors) -> lose;
result(scissors, rock)     -> lose;
result(scissors, paper)    -> win;
result(scissors, scissors) -> draw.

% A tournament
%
% A tournament is a series of rounds – each round is a single choice from the
% two players, which we’ll call left and right. Suppose that the choices are
% given as two lists; give the tournament result as an integer, so that the
% number counts the difference between the number of wins for left and right. A
% positive value is an overall win for left, a negative for right, and zero
% represents an overall draw.
-spec tournament([element()], [element()]) -> tournament_result().
tournament(PlayerOneRounds, PlayerTwoRounds) ->
  lists:foldl(
    fun (PlayersResults, Outcomes) ->
      Outcomes + outcome(PlayersResults)
    end,
    0,
    lists:map(
      fun ({X,Y}) -> {result(X,Y), result(Y,X)} end,
      lists:zip(PlayerOneRounds, PlayerTwoRounds)
    )
  ).

-spec outcome({round_result(), round_result()}) -> integer().
outcome({win, lose})  -> 1;
outcome({draw, draw}) -> 0;
outcome({lose, win})  -> -1.

player_one_wins_tournament_test() ->
  PlayerOneRounds = [rock,scissors,paper,paper],
  PlayerTwoRounds = [scissors,paper,scissors,paper],
  ?assertEqual(1, tournament(PlayerOneRounds, PlayerTwoRounds)).

player_two_wins_tournament_test() ->
  PlayerOneRounds = [rock,rock,paper,paper],
  PlayerTwoRounds = [rock,paper,scissors,rock],
  ?assertEqual(-1, tournament(PlayerOneRounds, PlayerTwoRounds)).

draw_tournament_test() ->
  PlayerOneRounds = [rock,rock,rock,paper],
  PlayerTwoRounds = [rock,paper,scissors,paper],
  ?assertEqual(0, tournament(PlayerOneRounds, PlayerTwoRounds)).
