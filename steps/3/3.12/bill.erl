-module(bill).
-author("@sgobotta").
-export([barcode/0, main/1]).
-include_lib("eunit/include/eunit.hrl").

-type item_code()         :: integer().
-type item_name()         :: string().
-type item_price()        :: integer().
-type item()              :: {item_code(), item_name(), item_price()}.
-type item_quantity()     :: {item_code(), integer()}.
-type barcode()           :: [item_code()].
-type discount()          :: quantity_discount.
-type quantity_discount() :: {item_code(), integer(), item_price()}.

-type quantity_discount_maybe() :: nothing | {just, quantity_discount() }.

main(Barcode) ->
  io:format("~s~n", [scan_barcode(Barcode)]).

%% @doc Simple database example
-spec db() -> [item()].
db() -> [
  {4719, "Fish Fingers" , 121},
  {5643, "Nappies" , 1010},
  {3814, "Orange Jelly", 56},
  {1111, "Hula Hoops", 21},
  {1112, "Hula Hoops (Giant)", 133},
  {1234, "Dry Sherry, 1lt", 540}
].

%% @doc Dummy discounts by quantity database in a tuples structure where the
%% first component is the item code, the
%% second component is the minimum item quantity to apply a discount, and the
%% third component is the price discount.
-spec quantity_discounts_db() -> [quantity_discount()].
quantity_discounts_db() -> [
  {1234, 2, -100}
].

%% @doc An example of a barcode, which represents a list of item codes.
-spec barcode() -> [item_code()].
barcode() -> [1234,4719,3814,1112,1113,1234].

%% @doc Given a list of item_code() representing a barcode, returns a string
%% representing a supermarket bill, with a title, a list of items with their
%% name and prices and the total price.
scan_barcode(Codes) ->
  DiscountItems = get_discounts_by_category(get_discount_categories(), Codes),
  Items = get_items(Codes, db()),
  TotalPrice = get_total_price(Items ++ DiscountItems),
  TotalPriceItem = {0, "Total", TotalPrice},
  print({Items ++ DiscountItems, TotalPriceItem}).

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

get_discount_categories() ->
  [quantity_discount].

%% @doc Given a list of discount category and a barcode() returns a list of
%% item() representing the available discounts.
-spec get_discounts_by_category([discount()], barcode()) -> [item()].
get_discounts_by_category([], _ItemCodes) -> [];
get_discounts_by_category([Category | Categories], ItemCodes) ->
  AvailableDiscounts = get_discounts({Category, ItemCodes}),
  lists:flatten([AvailableDiscounts | get_discounts_by_category(Categories, ItemCodes)]).

get_discounts_by_category_test() ->
  Barcode = barcode(),
  DiscountCategories = get_discount_categories(),
  ExpectsASingleDiscount = [serialize_discount({1234, 2, -100})],
  ?assertEqual(
    ExpectsASingleDiscount,
    get_discounts_by_category(DiscountCategories, Barcode)
  ),
  ExpectsTwoDiscounts = [serialize_discount({1234, 2, -100}), serialize_discount({1234, 2, -100})],
  ?assertEqual(
    ExpectsTwoDiscounts,
    get_discounts_by_category(DiscountCategories, Barcode ++ Barcode)
  ).

%% @doc Given a discount type and a barcode() returns a list of item() that
%% represent the available discounts for the given items in a barcode.
-spec get_discounts({discount(), barcode()}) -> [item()].
get_discounts({quantity_discount, ItemCodes}) ->
  SortedCodes = lists:sort(ItemCodes),
  ItemQuantities = get_item_quantities(SortedCodes, []),
  get_discounts_by_quantity(ItemQuantities, quantity_discounts_db()).

get_discounts_for_quantity_type_discounts_test() ->
  ExpectsNoDiscounts = [],
  ?assertEqual(ExpectsNoDiscounts, get_discounts({quantity_discount, [1234]})),
  ?assertEqual(ExpectsNoDiscounts, get_discounts({quantity_discount, [9999]})),
  ExpectsASingleDiscount = [serialize_discount({1234, 2, -100})],
  ?assertEqual(ExpectsASingleDiscount, get_discounts({quantity_discount, [1234, 1234, 1234]})),
  ExpectsTwoDiscounts = [serialize_discount({1234, 2, -100}), serialize_discount({1234, 2, -100})],
  ?assertEqual(ExpectsTwoDiscounts, get_discounts({quantity_discount, [1234, 1234, 1234, 1234, 1234]})),
  ?assertEqual(ExpectsTwoDiscounts, get_discounts({quantity_discount, [1234, 1234, 1234, 1234]})).

%% @doc Given a list of item_code() and an empty item_quantity(), returns a list
%% of item_quantity().
-spec get_item_quantities([item_code()], [item_quantity()]) -> [item_quantity()].
get_item_quantities([], Acc) -> Acc;
get_item_quantities([Code | _Codes] = CodeList, Acc) ->
  {Occurrences, Remaining} = take_occurrences(Code, CodeList, {[], []}),
  get_item_quantities(Remaining, Acc ++ [{Code, length(Occurrences)}]).

get_item_quantities_test() ->
  % Setup
  Codes = lists:sort(barcode()),
  ExpectedResult = [{1112, 1}, {1113, 1}, {1234, 2}, {3814, 1}, {4719, 1}],
  % Exercise and Assertions
  ?assertEqual(ExpectedResult, get_item_quantities(Codes, [])),
  % Setup
  OtherCodes = lists:sort([1234, 1234, 1234]),
  % Exercise and Assertions
  ?assertEqual([{1234, 3}], get_item_quantities(OtherCodes, [])),
  % Setup
  EmptyList = [],
  % Exercise and Assertions
  ?assertEqual([], get_item_quantities(EmptyList, [])).

%% @doc Given an item_code() and an ordered list of item_code(), returns a
%% tuple where the first element is the list of occurrences of the given
%% item_code and the second component is the remaining code list.
-spec take_occurrences(item_code(), [item_code()], {[item_code()], [item_code()]}) -> {[item_code()], [item_code()]}.
take_occurrences(_Code, [], {Occurrences, Remaining}) ->
  {Occurrences, Remaining};
take_occurrences(Code, [Code | Codes], {Occurrences, Remaining}) ->
  take_occurrences(Code, Codes, {Occurrences ++ [Code], Remaining});
take_occurrences(Code, [AnotherCode | Codes], {Occurrences, Remaining}) ->
  take_occurrences(Code, Codes, {Occurrences, Remaining ++ [AnotherCode]}).

take_occurrences_test() ->
  ?assertEqual(
    {[1234, 1234], [1112, 1113, 3814, 4719]},
    take_occurrences(1234, [1112, 1113, 1234, 1234, 3814, 4719], {[], []})
  ),
  ?assertEqual(
    {[1234, 1234, 1234], [1112, 1113, 3814, 4719]},
    take_occurrences(1234, [1112, 1113, 1234, 1234, 1234, 3814, 4719], {[], []})
  ),
  ?assertEqual(
    {[1234, 1234, 1234], [1112, 1112, 1113, 3814, 4719]},
    take_occurrences(1234, [1112, 1112, 1113, 1234, 1234, 1234, 3814, 4719], {[], []})
  ),
  ?assertEqual(
    {[1234, 1234, 1234], []},
    take_occurrences(1234, [1234, 1234, 1234], {[], []})
  ),
  ?assertEqual(
    {[], [1112, 1113, 1234, 1234, 1234, 3814, 4719]},
    take_occurrences(4200, [1112, 1113, 1234, 1234, 1234, 3814, 4719], {[], []})
  ).

remove_duplicates([]) -> [];
remove_duplicates(Xs) -> remove_duplicates(Xs, []).

remove_duplicates([], Acc) -> Acc;
remove_duplicates([{Code,_,_} = X | Xs], Acc) ->
  case is_item_member(Code, Acc) of
    true  -> remove_duplicates(Xs, Acc);
    false -> remove_duplicates(Xs, Acc ++ [X])
  end.

remove_duplicates_test() ->
  Items = get_items(barcode(), db()),
  ItemsSet = remove_duplicates(Items),
  ?assertEqual(init(Items), ItemsSet).

%% @doc Given a list of item_quantity() and a list of quantity_discount(),
%%  returns a list of quantity_discount().
-spec get_discounts_by_quantity([item_quantity()], [quantity_discount()]) -> [item()].
get_discounts_by_quantity([], _DiscountsData) -> [];
get_discounts_by_quantity([{Code, _Quantity} = Item | Items], DiscountsData) ->
  ItemQuantityDiscounts = from_quantity_discounts_maybe(
    get_quantity_discounts_maybe(Item, lists:flatten(DiscountsData))
  ),
  lists:flatten([ItemQuantityDiscounts | get_discounts_by_quantity(Items, DiscountsData)]).

get_discounts_by_quantity_test() ->
  % Setup
  DrySherryItemQuantity = {1234, 2},
  HoolaHoopsItemQuantity = {1111, 1},
  Items = [DrySherryItemQuantity, HoolaHoopsItemQuantity],
  DrySherryDiscount = hd(quantity_discounts_db()),
  ExpectedDiscounts = [serialize_discount(DrySherryDiscount)],
  % Exercise
  Discounts = get_discounts_by_quantity(Items, quantity_discounts_db()),
  % Assertion
  ?assertEqual(ExpectedDiscounts, Discounts),
  % Exercise
  HoolaHoopsDiscount = {1111, 1, 100},
  SerializedHoolaHoopsDiscount = serialize_discount({1111, 1, 100}),
  MoreExpectedDiscounts = ExpectedDiscounts ++ [SerializedHoolaHoopsDiscount],
  MoreDiscounts = get_discounts_by_quantity(Items, [[DrySherryDiscount] ++ [HoolaHoopsDiscount]]),
  % Assertion
  ?assertEqual(MoreExpectedDiscounts, MoreDiscounts),
  % Exercise
  EmptyDiscountsList = get_discounts_by_quantity([], MoreExpectedDiscounts),
  % Assertion
  ?assertEqual([], EmptyDiscountsList).

%% @doc Given a list of quantity_discount_maybe() of the same item, unwraps
%% discounts to return a list of item().
-spec from_quantity_discounts_maybe([quantity_discount_maybe()]) -> item().
from_quantity_discounts_maybe([]) -> [];
from_quantity_discounts_maybe([nothing | DsMaybe]) ->
  from_quantity_discounts_maybe(DsMaybe);
from_quantity_discounts_maybe([{just, Discount} | DsMaybe]) ->
  [Discount | from_quantity_discounts_maybe(DsMaybe)].

from_quantity_discounts_maybe_test() ->
  % Setup
  DiscountsDb = quantity_discounts_db(),
  DrySherryDiscount = hd(DiscountsDb),
  DiscountsMaybeList = [{just, DrySherryDiscount}, {just, DrySherryDiscount}, nothing],
  EmptyDiscountsMaybeList = [],
  % Exercise and Assertions
  ?assertEqual(
    [DrySherryDiscount, DrySherryDiscount],
    from_quantity_discounts_maybe(DiscountsMaybeList)
  ),
  ?assertEqual([], from_quantity_discounts_maybe(EmptyDiscountsMaybeList)).

% -spec has_discount(item_code(), [quantity_discount()]) -> boolean().
% has_discount(Code, DiscountsData) ->
%   is_item_member(Code, DiscountsData).

% has_discount_test() ->
%   %% Setup
%   FishFingersItem = hd(db()),
%   DrySherryItem = lists:last(db()),
%   %% Exercise and Assertions
%   ?assert(has_discount(DrySherryItem, quantity_discounts_db())),
%   ?assertNot(has_discount(FishFingersItem, quantity_discounts_db())).

-spec is_item_member(item_code(), [quantity_discount]) -> boolean().
is_item_member(_Code, []) -> false;
is_item_member(Code, [{Code,_,_} | _Ds]) -> true;
is_item_member(Code, [{_AnotherCode,_,_} | Ds]) -> is_item_member(Code, Ds).

is_item_member_test() ->
  ?assert(is_item_member(1234, quantity_discounts_db())),
  ?assertNot(is_item_member(420, quantity_discounts_db())).

-spec get_quantity_discounts_maybe(item_quantity(), [quantity_discount()]) -> quantity_discount_maybe().
get_quantity_discounts_maybe({_Code,_ItemQuantity}, [])->
  [];
get_quantity_discounts_maybe({Code,ItemQuantity}, [{Code,DiscountQuantity,_} = Discount | Ds]) when (ItemQuantity - DiscountQuantity) > 0 ->
  [{just, serialize_discount(Discount)} | get_quantity_discounts_maybe({Code, ItemQuantity - DiscountQuantity}, [Discount | Ds])];
get_quantity_discounts_maybe({Code,ItemQuantity}, [{Code,DiscountQuantity,_} = Discount  | Ds]) when (ItemQuantity - DiscountQuantity) == 0 ->
  [{just, serialize_discount(Discount)} | get_quantity_discounts_maybe({Code, ItemQuantity - DiscountQuantity}, Ds)];
get_quantity_discounts_maybe({Code,_ItemQuantity} = Item, [{Code,_DiscountQuantity,_} | Ds]) ->
  [nothing | get_quantity_discounts_maybe(Item, Ds)];
get_quantity_discounts_maybe({_Code,_} = Item, [{_AnotherCode,_,_} | Ds]) ->
  get_quantity_discounts_maybe(Item, Ds).

get_quantity_discounts_maybe_test() ->
  % Setup
  DiscountsDb = quantity_discounts_db(),
  DrySherryItemQuantity = {1234, 3},
  % Exercise and Assertions
  ?assertEqual(
    [{just, serialize_discount(hd(DiscountsDb))}, nothing],
    get_quantity_discounts_maybe(DrySherryItemQuantity, DiscountsDb)
  ),
  OtherDrySherryItemQuantity = {1234, 2},
  ?assertEqual(
    [{just, serialize_discount(hd(DiscountsDb))}],
    get_quantity_discounts_maybe(OtherDrySherryItemQuantity, DiscountsDb)
  ),
  AnotherDrySherryItemQuantity = {1234, 1},
  ?assertEqual(
    [nothing],
    get_quantity_discounts_maybe(AnotherDrySherryItemQuantity, DiscountsDb)
  ),
  ItemThatHasNoDiscount = {9999, 1},
  ?assertEqual(
    [],
    get_quantity_discounts_maybe(ItemThatHasNoDiscount, DiscountsDb)
  ).

-spec serialize_discount(discount()) -> item().
serialize_discount({Code, _Quantity, Price}) -> {Code, "Discount", Price}.

serialize_discount_test() ->
  ExpectsDiscount = {1234, "Discount", -100},
  DrySHerryDiscount = hd(quantity_discounts_db()),
  ?assertEqual(ExpectsDiscount, serialize_discount(DrySHerryDiscount)).

%% @doc Given a tuple representing a list of items and a tuple representing
%% the total price, returns a string representing a supermarket bill.
print({Items, Total}) ->
  MaxLineLength = 30,
  PrintedTitle = left_padding("Erlang Stores", MaxLineLength),
  PrintedItems = print_items(Items, MaxLineLength, ""),
  PrintedTotal = print_item(Total, MaxLineLength),
  PrintedTitle ++ [$\n,$\n] ++ PrintedItems ++ [$\n] ++ PrintedTotal.

print_test() ->
  % Setup
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
  % Exercise
  Bill = print({Items, TotalPriceItem}),
  % Assertion
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
  % Setup
  Items = get_items(barcode(), db()),
  % Exercise
  ?assertEqual(0, get_total_price([])),
  ?assertEqual(540, get_total_price([hd(Items)])),
  ?assertEqual(850, get_total_price(tl(Items))),
  ?assertEqual(1390, get_total_price(Items)).

%% @doc Given a list of tuples representing items, a line length and an
%% accumulator, returns a string representing the formatted line bills for each
%% item, with their names and prices.
print_items([], _N, Accumulator) ->
  Accumulator;
print_items([Item|Items], N, Accumulator) ->
  print_items(Items, N, Accumulator ++ print_item(Item, N) ++ [$\n]).

print_items_test() ->
  % Setup
  Items = get_items(barcode(), db()),
  ExpectedText =
  "Dry Sherry, 1lt...........5.40\n" ++
  "Fish Fingers..............1.21\n" ++
  "Orange Jelly..............0.56\n" ++
  "Hula Hoops (Giant)........1.33\n" ++
  "Unknown Item..............0.00\n" ++
  "Dry Sherry, 1lt...........5.40\n",
  % Exercise and Assertion
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
fill(0, _C) -> [];
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
string_to_float_string([], _N) -> "0.0";
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

init([_X|[]]) -> [];
init([X|Xs]) -> [X | init(Xs)].
