# FutureLearn - Erlang

## 3.13

### Feedback on the supermarket billing programming challenge

**In this step we introduce a skeleton solution for the problem.**

The skeleton shows how we first choose to define types representing the various components of the problem – barcodes, costs, collections of items etc – and then the functions that we build to construct and manipulate these types to build up the overall solution.

The skeleton is available as an Erlang file: bill_skel.erl

#### The basic types of the solution

An item is represented by a string

```erlang
-type item() :: string().
```

Cost is an integer: think of this as pence.

```erlang
-type cost() :: integer().
```

Barcodes code are represented by integers.

```erlang
-type code() :: integer().
```

A list of barcodes

```erlang
-type codes() :: list(code()).
```

A database is a list of triples, mapping a code to an item/cost pair.

```erlang
-type database() :: list({code(),item(),cost()}).
```

A bill is a list of item/cost pairs.

```erlang
-type bill() :: list({item(),cost()}).
```

An example database and list of codes.

```erlang
-spec my_db() -> database().
my_db() -> [{237891,"Gin",1099}, {457381,"Tonic",199}, {872354,"Fish Fingers",99}].
     
-spec my_codes() -> codes().
my_codes() -> [872354,237891,457381,457381,872354].
```

#### The database operations: forming the bill

Look up a code in a database, returning {"Unknown item",0} for missing codes.

```erlang
-spec lookup(code(),database()) -> {item(),cost()}.
% e.g. lookup(237891,my_db()) = {"Gin",1099}
```

Look up in the example database, using `lookup/2`.

```erlang
-spec lookup(code()) -> {item(),cost()}.
% e.g. lookup(237891) = {"Gin",1099}
```

Turn a list of codes into a bill. % You can use `lists:map` if you wish

```erlang
-spec make_bill(list(code())) -> bill().
% e.g. make_bill([237891]) =[{"Gin",1099}]
```

Find the total cost for a bill.

```erlang
-spec total(bill()) -> cost().
% e.g. total([{"Gin",1099}]) = 1099
```

A bill with total added at the end.

```erlang
-spec add_total(bill()) -> bill().
% e.g. add_total([{"Gin",1099}]) = [{"Gin",1099}, {"TOTAL",1099}]
```

#### Enhancement: multibuy discount

Count an item in a bill.

```erlang
-spec count(item(),bill()) -> integer().
```

Calculate the multibuy discount given the discount per pair of item in bill.

```erlang
-spec multibuy(integer(),item(),bill()) -> integer().
```

Add the multibuy discount to the end of a bill.

```erlang
-spec add_multibuy(integer(),item(),bill()) -> bill().
```

#### Printable forms of bills: the functions print and show

The show function puts the types we’ve described earlier into readable strings. The print function wraps show with a call to a function from the io module to print the results.

The show function can be applied to codes, databases or bills, as well as single items within them.

```erlang
-spec show(code()|{item(),cost()}|{code(),item(),cost()}|
          codes()|database()|bill()) -> string().

-spec print({item(),cost()}|{code(),item(),cost()}|database()|bill()) -> ok.
```

The definition of show will use some auxiliary functions.

To pad strings to the left lpad or right rpad to the given length Len. These functions will not truncate strings that are already longer than that.

```erlang
-spec rpad(integer(),string()) -> string().
-spec lpad(integer(),string()) -> string().
```

Build a list of N Xs.

```erlang
-spec replicate(integer,T) -> list(T).
% e.g. replicate(3,5) = [5,5,5].
```
