-module(test_pattern_matching).

-export([
    test_max_three/0,
    test_how_many_equal/0
]).

test_max_three() ->
  4 = pattern_matching:max_three(4, 2, 0),
  4 = pattern_matching:max_three(2, 4, 0),
  4 = pattern_matching:max_three(0, 2, 4),
  ':: max_three tests passed succesfully'.

test_how_many_equal() ->
  3 = pattern_matching:how_many_equal(4,4,4),
  2 = pattern_matching:how_many_equal(4,4,0),
  2 = pattern_matching:how_many_equal(4,0,4),
  2 = pattern_matching:how_many_equal(0,4,4),
  0 = pattern_matching:how_many_equal(4,2,0),
  ':: how_many_equal tests passed succesfully'.
