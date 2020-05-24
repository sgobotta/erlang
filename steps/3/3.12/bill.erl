-module(bill).
-author("@sgobotta").
-export([barcode/0, main/1]).
-include_lib("eunit/include/eunit.hrl").

main(Barcode) ->
  io:format("~s~n", [scan_barcode(Barcode)]).

%% @doc Simple database example
db() -> [
  {4719, "Fish Fingers" , 121},
  {5643, "Nappies" , 1010},
  {3814, "Orange Jelly", 56},
  {1111, "Hula Hoops", 21},
  {1112, "Hula Hoops (Giant)", 133},
  {1234, "Dry Sherry, 1lt", 540}
].

barcode() -> [1234,4719,3814,1112,1113,1234].

%% @doc Given a list of integer representing a barcode, returns a string
%% representing a supermarket bill, with a title, a list of items with their
%% name and prices and the total price.
scan_barcode(Codes) ->
  Items = get_items(Codes, db()),
  TotalPrice = get_total_price(Items),
  TotalPriceItem = {0, "Total", TotalPrice},
  print({Items, TotalPriceItem}).

scan_barcode_test() ->
  % Setup
  ExpectedBill =
    "        Erlang Stores\n" ++
    "\n" ++
    "Dry Sherry, 1lt...........5.40\n" ++
    "Fish Fingers..............1.21\n" ++
    "Orange Jelly..............0.56\n" ++
    "Hula Hoops (Giant)........1.33\n" ++
    "Unknown Item..............0.00\n" ++
    "Dry Sherry, 1lt...........5.40\n" ++
    "\n" ++
    "Total....................13.90",
  Bill = scan_barcode(barcode()),
  ?assertEqual(ExpectedBill, Bill).

%% @doc Given a tuple representing a list of items and a tuple representing
%% the total price, returns a string representing a supermarket bill.
print({Items, Total}) ->
  MaxLineLength = 30,
  PrintedTitle = left_padding("Erlang Stores", MaxLineLength),
  PrintedItems = print_items(Items, MaxLineLength, ""),
  PrintedTotal = print_item(Total, MaxLineLength),
  PrintedTitle ++ [$\n,$\n] ++ PrintedItems ++ [$\n] ++ PrintedTotal.

print_test() ->
  %% Setup
  Items = get_items(barcode(), db()),
  TotalPrice = get_total_price(Items),
  TotalPriceItem = {0, "Total", TotalPrice},
  ExpectedBill =
    "        Erlang Stores\n" ++
    "\n" ++
    "Dry Sherry, 1lt...........5.40\n" ++
    "Fish Fingers..............1.21\n" ++
    "Orange Jelly..............0.56\n" ++
    "Hula Hoops (Giant)........1.33\n" ++
    "Unknown Item..............0.00\n" ++
    "Dry Sherry, 1lt...........5.40\n" ++
    "\n" ++
    "Total....................13.90",
  %% Exercise
  Bill = print({Items, TotalPriceItem}),
  %% Assertion
  ?assertEqual(ExpectedBill, Bill).

%% @doc Given a string representing a title and an integer representing a line
%% length, returns a left padded string.
left_padding(Title, N) ->
  TitleLength = length(Title),
  FillLength = (N - TitleLength) div 2,
  left_padding(Title, FillLength, "").

%% @doc Given a string representing a title, an integer representing the number
%% of spaces to add and an accumulator, returns a string with extra left
%% padding equal to N.
left_padding(Title, 0, Accumulator) -> Accumulator ++ Title;
left_padding(Title, N, Accumulator) -> left_padding(Title, N-1, [$\s] ++ Accumulator).

left_padding_test() ->
  ?assertEqual(
    "        Erlang Stores",
    left_padding("Erlang Stores", 8, "")
  ).

%% @doc Given a list of tuples representing items returns the total price.
get_total_price([]) -> 0;
get_total_price([{_,_,Price}|Items]) -> Price + get_total_price(Items).

get_total_price_test() ->
  %% Setup
  Items = get_items(barcode(), db()),
  %% Exercise
  ?assertEqual(0, get_total_price([])),
  ?assertEqual(540, get_total_price([hd(Items)])),
  ?assertEqual(850, get_total_price(tl(Items))),
  ?assertEqual(1390, get_total_price(Items)).

%% @doc Given a list of tuples representing items, a line length and an
%% accumulator, returns a string representing the formatted line bills for each
%% item, with their names and prices.
print_items([], N, Accumulator) ->
  Accumulator;
print_items([Item|Items], N, Accumulator) ->
  print_items(Items, N, Accumulator ++ print_item(Item, N) ++ [$\n]).

print_items_test() ->
  %% Setup
  Items = get_items(barcode(), db()),
  ExpectedText =
  "Dry Sherry, 1lt...........5.40\n" ++
  "Fish Fingers..............1.21\n" ++
  "Orange Jelly..............0.56\n" ++
  "Hula Hoops (Giant)........1.33\n" ++
  "Unknown Item..............0.00\n" ++
  "Dry Sherry, 1lt...........5.40\n",
  %% Exercise
  ?assertEqual(ExpectedText, print_items(Items, 30, "")).

%% @doc Given a tuple representing an item and a line length, returns a string
%% representing a formatted line bill with an item name and it's price.
print_item({_Code, Name, Price}, N) ->
  FormattedPrice = format_price(Price),
  PriceLength = length(FormattedPrice),
  NameLength = length(Name),
  FillLength = N - (PriceLength + NameLength),
  Name ++ fill(FillLength, ".") ++ FormattedPrice.

print_item_test() ->
  ?assertEqual(
    "Dry Sherry, 1lt...........5.40",
    print_item({1234, "Dry Sherry, 1lt", 540}, 30)
  ),
  ?assertEqual(
    "Fish Fingers..............1.21",
    print_item({4719, "Fish Fingers" , 121}, 30)
  ),
  ?assertEqual(
    "Nappies..................10.10",
    print_item({5643, "Nappies" , 1010}, 30)
  ),
  ?assertEqual(
    "Orange Jelly..............0.56",
    print_item({3814, "Orange Jelly", 56}, 30)
  ),
  ?assertEqual(
    "Hula Hoops (Giant)........1.33",
    print_item({1112, "Hula Hoops (Giant)", 133}, 30)
  ),
  ?assertEqual(
    "Unknown Item..............0.00",
    print_item({0, "Unknown Item", 0}, 30)
  ),
  ?assertEqual(
    "Total....................13.90",
    print_item({0, "Total", 1390}, 30)
  ).

%% @doc Given an integer and a character, returns a string with N characters C.
fill(0, C) -> [];
fill(N, C) -> C ++ fill(N-1, C).

fill_test() ->
  ?assertEqual("", fill(0, ".")),
  ?assertEqual(".", fill(1, ".")),
  ?assertEqual("..", fill(2, ".")),
  ?assertEqual("...", fill(3, ".")),
  ?assertEqual("....", fill(4, ".")),
  ?assertEqual("......", fill(6, ".")).

%% @doc Given an integer representing a price, returns a string representing
%% the price expressed as a float.
format_price(Price) ->
  TextPrice = integer_to_list(Price),
  lists:reverse(string_to_float_string(lists:reverse(TextPrice), 2)).

format_price_test() ->
  ?assertEqual("10.10", format_price(1010)),
  ?assertEqual("5.40", format_price(540)),
  ?assertEqual("1.21", format_price(121)),
  ?assertEqual("0.56", format_price(56)),
  ?assertEqual("1.33", format_price(133)),
  ?assertEqual("0.11", format_price(11)),
  ?assertEqual("0.01", format_price(1)),
  ?assertEqual("0.00", format_price(0)).

%% @doc Given a list of characters and an integer, returns a string representing
%% a float number.
string_to_float_string([], 0) -> ".0";
string_to_float_string([], N) -> "0.0";
string_to_float_string(Cs, 0) -> "." ++ Cs;
string_to_float_string([C|Cs], N) -> [C | string_to_float_string(Cs, N-1)].

string_to_float_string_test() ->
  ?assertEqual("00.0", string_to_float_string("000", 2)),
  ?assertEqual("04.5", string_to_float_string("045", 2)),
  ?assertEqual("00.21", string_to_float_string("0021", 2)).

%% @doc Given a list of integer representing codes, returns an array of tuples
%% representing items from the dummy database.
get_items([], _Db) -> [];
get_items([Code | Codes], Db) -> [get_item(Code, Db) | get_items(Codes, Db)].

get_items_test() ->
  ?assertEqual([], get_items([], [])),
  ?assertEqual(
    [{0, "Unknown Item", 0}, {1234, "Dry Sherry, 1lt", 540}],
    get_items([0000,1234], db())
  ),
  ?assertEqual(
    [
      {0, "Unknown Item", 0},
      {1234, "Dry Sherry, 1lt", 540},
      {4719, "Fish Fingers" , 121}
    ],
    get_items([0000,1234,4719], db())
  ).

%% @doc Given an integer representing an item code, returns a tuple representing
%% an item from the dummy database.
get_item(Code) -> get_item(Code, db()).

%% @doc Given an integer representing an item code and a dummy database, returns
%% a tuple representing an item from the database.
get_item(_Code, []) ->
  {0, "Unknown Item", 0};
get_item(Code, [{Code, _Name, _Price} = Item | _Items]) ->
  Item;
get_item(Code, [{_AnotherCode, _Name, _Price} | Items]) ->
  get_item(Code, Items).

get_item_test() ->
  ?assertEqual({0, "Unknown Item", 0}, get_item(420)),
  ?assertEqual({4719, "Fish Fingers" , 121}, get_item(4719)),
  ?assertEqual({3814, "Orange Jelly", 56}, get_item(3814)),
  ?assertEqual({1234, "Dry Sherry, 1lt", 540}, get_item(1234)).
