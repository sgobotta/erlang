-module(exs).
-author("@sgobotta").
-export([
  join/2, test_join/0,
  concat/1, test_concat/0,
  member/2, test_member/0
]).

%% @doc Given two lists, joins them into a single list.
-spec join([T], [T]) -> [T].
join([], Ys) -> Ys;
join([X|Xs], Ys) -> [X | join(Xs, Ys)].

test_join() ->
  []            = join([], []),
  [1,2]         = join([1,2], []),
  [1,2]         = join([], [1,2]),
  [1,2]         = join([1], [2]),
  [1,2,3,4,5,6] = join([1,2,3], [4,5,6]),
  ""            = join("", ""),
  "santiago"    = join("san", "tiago"),
  "santiago"    = join("", "santiago"),
  "santiago"    = join("santiago", ""),
  {passed, "join tests passed succesfully"}.

%% @doc Given a list of lists, flattens all lists elements into a single list.
-spec concat([[T]]) -> [T].
concat([]) -> [];
concat([Xs|Xss]) -> join(Xs, concat(Xss)).

test_concat() ->
  []            = concat([[], [], []]),
  [1,2]         = concat([[1,2], [], []]),
  [3,4]         = concat([[], [3,4], []]),
  [5,6]         = concat([[], [], [5,6]]),
  [1,2,3,4,5,6] = concat([[1,2], [3,4], [5,6]]),
  [1,2,3,4,5,6] = concat([[1,2,3], [4,5,6]]),
  [1,2,3,4,5,6] = concat([[1], [2,3], [4,5,6]]),
  [1,2,3,4,5,6] = concat([[1,2,3], [4,5], [6]]),
  ""            = concat(["", "", ""]),
  "santiago"    = concat(["san", "tia", "go"]),
  "santiago"    = concat(["", "santia", "go"]),
  "santiago"    = concat(["", "", "santiago"]),
  "santiago"    = concat(["san", "tiago", ""]),
  "santiago"    = concat(["santiago", "", ""]),
  {passed, "concat tests passed succesfully"}.

%% @doc Given an element and a list, tests whether the element is a member of
%% the given list.
-spec member(T, [T]) -> boolean().
member(_E, []) -> false;
member(E, [E|_Xs]) -> true;
member(E, [_X|Xs]) -> member(E, Xs).

test_member() ->
  false = member(1, []),
  false = member(1, [2]),
  false = member(1, [3,0,0,0,6]),
  true  = member(1, [1]),
  true  = member(1, [1,2,3,4,5]),
  true  = member(1, [2,3,4,5,1]),
  true  = member(1, [3,4,1,5,6]),
  true  = member(1, [3,1,1,1,6]),
  {passed, "member tests passed succesfully."}.
