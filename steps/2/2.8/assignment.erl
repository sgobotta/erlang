-module(assignment).
-author("@sgobotta").
-export([
  area/1, test_area/0,
  perimeter/1, test_perimeter/0,
  enclose/1, test_enclose/0,
  bits_dr/1, test_bits_dr/0,
  bits_tr/1, test_bits_tr/0
]).

sum(X,Y,Z) -> X + Y + Z.

mult(X,Y) -> X * Y.

double(X) -> mult(2,X).

square(X) -> mult(X,X).

%% @doc Given a shape returns it's area.
area({rectangle, H, W}) -> mult(H,W);
area({circle, R}) -> math:pi() * square(R);
area({triangle, S1, S2, S3}) ->
    S = sum(S1,S2,S3)/2,
    math:sqrt(S*(S-S1)*(S-S2)*(S-S3)).

test_area() -> 
  100 = area({rectangle, 10, 10}),
  400 = area({rectangle, 20, 20}),
  5541.769440932395 = area({circle, 42}),
  554176.9440932395 = area({circle, 420}),
  70.25622748198198 = area({triangle, 12, 12, 15}),
  19677.39820199815 = area({triangle, 420, 240, 220}),
  {passed, "area tests passed succesfully"}.

%% @doc Given a shape returns it's perimeter.
perimeter({rectangle, H, W}) -> double(H) + double(W);
perimeter({circle, R}) -> mult(double(math:pi()), R);
perimeter({triangle, S1, S2, S3}) -> sum(S1,S2,S3).

test_perimeter() ->
  80 = perimeter({rectangle, 20, 20}),
  1320 = perimeter({rectangle, 420, 240}),
  2638.9378290154264 = perimeter({circle, 420}),
  263.89378290154264 = perimeter({circle, 42}),
  39 = perimeter({triangle, 12, 12, 15}),
  880 = perimeter({triangle, 420, 240, 220}),
  {passed, "shape tests passed succesfully."}.

%% @doc Given a shape returns the smallest enclosing rectangle of that shape.
enclose({circle, R}) -> {rectangle, double(R), double(R)};
enclose({rectangle, H, W}) -> {rectangle, H, W};
enclose({triangle, S1, S2, S3}) -> {rectangle, S1, triangle_height(S1,S2,S3)}.

triangle_height(B, S1, S2) -> double(area({triangle, B, S1, S2})) / B.

test_enclose() ->
  {rectangle, 840, 840} = enclose({circle, 420}),
  {rectangle, 54, 54} = enclose({circle, 27}),
  {rectangle, 420, 420} = enclose({rectangle, 420, 420}),
  {rectangle, 27, 27} = enclose({rectangle, 27, 27}),
  {rectangle, 12, 10.392304845413264} = enclose({triangle, 12, 12, 12}),
  {rectangle, 12, 10.392304845413264} = enclose({triangle, 12, 12, 12}),
  {rectangle,420,359.03929205956047} = enclose({triangle, 420, 420, 412}),
  {passed, "enclose tests passed succesfully."}.

%% @doc Given a number returns the sum of it's least significant bit to
%% the total sum of its right shifted bits.
bits_dr(0) -> 0;
bits_dr(N) -> N band 1 + bits_dr(N bsr 1).

test_bits_dr() ->
  0 = bits_dr(0),
  1 = bits_dr(1),
  1 = bits_dr(2),
  2 = bits_dr(3),
  1 = bits_dr(4),
  2 = bits_dr(5),
  2 = bits_dr(6),
  3 = bits_dr(7),
  1 = bits_dr(8),
  {passed, "bits_dr tests passed succesfully."}.

%% @doc Given a number returns the sum of it's total bits using tail recursion.
bits_tr(N) -> bits_tr(N, 0).

%% @doc Given a number, keeps calling bits_tr/2 with it's right shifted bit,
%% accumulating the sum of its least significant bit each time.
bits_tr(0, Accumulator) -> Accumulator;
bits_tr(N, Accumulator) -> bits_tr(N bsr 1, Accumulator + (N band 1)).

test_bits_tr() ->
  0 = bits_tr(0),
  1 = bits_tr(1),
  1 = bits_tr(2),
  2 = bits_tr(3),
  1 = bits_tr(4),
  2 = bits_tr(5),
  2 = bits_tr(6),
  3 = bits_tr(7),
  1 = bits_tr(8),
  {passed, "bits_tr tests passed succesfully."}.
