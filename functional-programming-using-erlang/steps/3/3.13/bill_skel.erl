-module(bill_skel).
-export([lookup/2,lookup/1,make_bill/1,total/1,count/2,multibuy/3,add_multibuy/3,add_total/1]).
-export([my_db/0,my_codes/0]).
-export([replicate/2,rpad/2,lpad/2,print/1,show/1]).

%
% The basic types of the solution
%

% An item is represented by a string
-type item() :: string().

% Cost is an integer: think of this as pence.
-type cost() :: integer().

% Barcodes are represented by integers.
-type code() :: integer().

% A list of barcodes
-type codes() :: list(code()).

% The database is a list of triples, mapping
% codes to item/cost pairs.
-type database() :: list({code(),item(),cost()}).

% A bill is a list of item/cost pairs.
-type bill() :: list({item(),cost()}).

%
%  Example database and bill.
%

% Example barcode database.

-spec my_db() -> database().

my_db() -> [{237891,"Gin",1099}, {457381,"Tonic",199}, {872354,"Fish Fingers",99}].

% Example bill.

-spec my_codes() -> list(code()).

my_codes() -> [872354,237891,457381,457381,872354].

%
% Dummy definition: you need to replace occurrences of this.
%

dummy() -> dummy().

%
% Database operations: forming the bill
%

% Look up a code in a database.
% Returns {"Unknown item",0} for missing codes.
% e.g. lookup(237891,my_db()) = {"Gin",1099}

-spec lookup(code(),database()) -> {item(),cost()}.

lookup(_,_) -> dummy().

% Look up in the example database.
% Uses lookup/2
% e.g. lookup(237891) = {"Gin",1099}

-spec lookup(code()) -> {item(),cost()}.

lookup(_) -> dummy().

% Turn a list of codes into a bill.
% You can use lists:map if you wish
% e.g. make_bill([237891]) =[{"Gin",1099}]

-spec make_bill(list(code())) -> bill().

make_bill(_) -> dummy().

% Total cost
% e.g. total([{"Gin",1099}]) = 1099

-spec total(bill()) -> cost().

total(_) -> dummy().

% Bill with total added at the end.
% e.g. add_total([{"Gin",1099}]) = [{"Gin",1099}, {"TOTAL",1099}]

-spec add_total(bill()) -> bill().

add_total(_) -> dummy().

%
% Enhancement: multibuy discount
%

% Count an item in a bill

-spec count(item(),bill()) -> integer().

count(_,_) -> dummy().

% Multibuy discount: Disc off per pair of Item in Bill

-spec multibuy(integer(),item(),bill()) -> integer().

multibuy(_,_,_) -> dummy().

% Bill with a {"Multibuy", M} item added at the end.

-spec add_multibuy(integer(),item(),bill()) -> bill().

add_multibuy(_,_,_) -> dummy().

%
% Printable forms of bills
%

% Functions print and show

% Put into a readable form. Can apply to codes, databases or bills,
% as well as single items within them.
% show builds the string
% print also prints using io.erl, so that formatting applied.

-spec print({item(),cost()}|{code(),item(),cost()}|database()|bill()) -> ok.

print(_) -> dummy().

-spec show(code()|{item(),cost()}|{code(),item(),cost()}|codes()|database()|bill()) -> string().

show(_) -> dummy().

% pad strings to the left (lpad) or right (rpad) to the length Len
% doesn't truncate over length-strings.

-spec rpad(integer(),string()) -> string().

rpad(_,_) -> dummy().

-spec lpad(integer(),string()) -> string().

lpad(_,_) -> dummy().

% A list of N X's, e.g. replicate(3,5) = [5,5,5].

-spec replicate(integer,T) -> list(T).

replicate(_,_) -> dummy().