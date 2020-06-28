# FutureLearn - Erlang

## 2.9

### Pulling it all together: review

Looking at the work of other programmers who are trying the same tasks as you can be really useful. It can help you to see different styles and think about other ways to approach a problem.

Now, take a look at some work by your fellow programmers.

Do provide positive feedback, but also try to find at least one ‘criticism’ where you think the assignment could be improved. Give reasons for your feedback - not just ‘I like this’ or ‘I don’t like that’ - but thinking about why. Perhaps the names used for variables or functions are confusing, or the way the program works is unclear and would benefit from using comments?

If you have time, do provide feedback on more than one assignment – but remember it is better to do a thorough job on just a few assignments than a less helpful job on more.

For reference, you can download a copy of the original assignment instructions.

#### Assignment Guidelines

You’re going to be asked to give feedback on the following aspects of the author’s assignment:

+ Ensure that the solution is correct; perhaps include some tests to show how a function works in a number of representative cases.
+ Ensure that the solution is readable; for example by including comments that explain how the functions work, and indicate any particular aspects that need explanation.
+ Comment on any potential alternative approaches that may have been considered, and why the one chosen is the most appropriate.

Please keep this window open and do not navigate away before submitting your feedback. If you close the window or navigate to a different page, you will be given a new assignment to review when you return.

### My Review

> Ensure that the solution is correct; perhaps include some tests to show how a function works in a number of representative cases.

I've checked your assignment and had to manually test every function. Though the enclose function is missing everything is working! I wonder if you tried to provide any test. Here's an example:

```erlang
test_sub_bit() ->
  0 = sumBit(0),
  1 = sumBit(1),
  1 = sumBit(2),
  2 = sumBit(3),
  {passed, "tests passed succesfully"}.
```

Test could help you quickly acknowledge your code is working fine in a more efficient way :)
According to the assignment, the function should have been called `bits`.

> Ensure that the solution is readable; for example by including comments that explain how the functions work, and indicate any particular aspects that need explanation.

I think your code looks very clear since functions are quite easy to understand and the solutions are readable. Though maybe you could add documentation. I personally use the @doc decorator inside comments telling what the function does.
Also, did you notice you're doing the sum of sides twice? One in the perimeter/2 function and then in the area/2 function. You could just refactor that to a sum/3 function.

> Comment on any potential alternative approaches that may have been considered, and why the one chosen is the most appropriate.

I'm not sure if you've considered it, but you could also implement the sumBits function using tail recursion. It would look just like yours but using a two parameters function, where the second one would accumulate the bits sum while the first one would consider which is the next number to be evaluated for a sum.

Nice work!

© University of Kent
