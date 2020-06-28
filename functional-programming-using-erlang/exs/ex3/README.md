# FutureLearn - Erlang

## 1.15

### Variables and patterns in practice

The aim of this exercise is to give you experience of writing functions using pattern matching in Erlang, as well as to do some “hand calculation” too.

You can solve all of these questions just using pattern matching – in particular you don’t need any other mechanism for distinguishing between different cases.

We’ll provide some feedback for these exercises in the next step, but when you have completed the step, maybe you would like to discuss your approach, and compare it with what other learners have done, in the comments for this step?

#### Adding comments to your program files

If you want to add comments to your Erlang program files, you can add them anywhere in a program, after the % symbol. This could be a comment on a line of its own, or it can follow program text. You can see examples of both of these in this program:

```erlang
-module(foo).
-export([foo/0]).
% this is a comment
foo() -> % this is a comment too
  42.    % and so is this
```

Unlike other languages, this is the only way of including comments, so multi-line comments need to contain % at the beginning of every line.

---

#### Exclusive or

In the previous video step on pattern matching we saw two ways of defining “exclusive or”. Give at least three others. You might find it useful to know that:

=/= and == are the operations for inequality and equality in Erlang;

not is the Erlang negation function; and,

and and or are the Erlang conjunction and disjunction (infix) operators.

---

#### Maximum of three

Give a definition of the function maxThree which takes three integers and returns the maximum of the three. You can use the max function, which gives the maximum of two numbers, in writing your definition.

```erlang
maxThree(34,25,36) = 36
```

---

#### How many equal?

Give a definition of the function howManyEqual which takes three integers and returns an integer, counting how many of its three arguments are equal, so that:

```erlang
howManyEqual(34,25,36) = 0
howManyEqual(34,25,34) = 2
howManyEqual(34,34,34) = 3
```

---

#### Testing your programs

The Erlang shell provides the ideal way of testing that your programs work as they should. If you have written a definition of howManyEqual then you can test it by evaluating it on an example, such as

```erlang
howManyEqual(34,25,36)
```

Alternatively, you can add this as an explicit test function to your module:

```erlang
test1() ->
     howManyEqual(34,25,36) == 0.
```

but remember that you will need to export this function before you are able to evaluate it – hopefully to true – in the Erlang shell. We’ll come back to testing in the second week of the course, at step 2.23.

© University of Kent
