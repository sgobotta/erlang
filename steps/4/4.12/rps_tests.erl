-module(rps_tests).
-author("sgobotta").
-include_lib("eunit/include/eunit.hrl").
-import(rps,[no_repeat/1,cycle/1,least_frequent/1,most_frequent/1]).

%% Strategy tests

no_repeat_test() ->
  ?assertEqual(paper, rps:no_repeat([scissors])),
  ?assertEqual(scissors, rps:no_repeat([rock])),
  ?assertEqual(rock, rps:no_repeat([paper])).

cycle_test() ->
  ?assertEqual(rock,     rps:cycle([])),
  ?assertEqual(paper,    rps:cycle([dummy])),
  ?assertEqual(scissors, rps:cycle([dummy, dummy])),
  ?assertEqual(rock,     rps:cycle([dummy, dummy, dummy])),
  ?assertEqual(paper,    rps:cycle([dummy, dummy, dummy, dummy])),
  ?assertEqual(scissors, rps:cycle([dummy, dummy, dummy, dummy, dummy])).

least_frequent_test() ->
  ?assertEqual(rock, rps:least_frequent([rock])),
  ?assertEqual(rock, rps:least_frequent([rock,paper,paper,scissors,scissors])),
  ?assertEqual(rock, rps:least_frequent([paper,rock,paper,scissors,scissors])),
  ?assertEqual(rock, rps:least_frequent([paper,paper,rock,scissors,scissors])),
  ?assertEqual(rock, rps:least_frequent([scissors,paper,paper,rock,scissors])),
  ?assertEqual(rock, rps:least_frequent([scissors,paper,paper,scissors,rock])).

most_frequent_test() ->
  ?assertEqual(rock, rps:most_frequent([rock])),
  ?assertEqual(rock, rps:most_frequent([rock,rock,paper,rock,scissors])),
  ?assertEqual(rock, rps:most_frequent([rock,paper,rock,scissors,rock])),
  ?assertEqual(rock, rps:most_frequent([paper,paper,rock,rock,rock])),
  ?assertEqual(rock, rps:most_frequent([rock,rock,rock,scissors,scissors])).
