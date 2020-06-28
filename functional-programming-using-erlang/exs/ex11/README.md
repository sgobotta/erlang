# FutureLearn - Erlang

## 2.16

### More functions over lists

In this video we continue to explore ways of defining functions over lists in Erlang.

We’ll start with some feedback on the previous exercise to find the maximum of the elements in a list. We’ll then look at an example involving geometric shapes, so we’re not restricted to just looking at lists of numbers.

Note that there is a supporting file shapes.erl available under ‘Downloads’ below.

© University of Kent

### Comments

Simon speaks of maps in this video, and those are certainly really interesting, but what's a little confusing is that Erlang already has a "maps" library, but those maps are really more like dictionaries in Python. For example,

```erlang
Name = #{first => "Douglas", middle => "Edan", last => "Lewit"}.
```

And then I could do the following:
`maps:get(first, Name)` and that gives me back "Douglas",
`maps:get(middle, Name)` and that gives me back "Edan,
and finally `maps:get(last, Name)` and that gives me back "Lewit". But that's more like a dictionary rather than the type of map that Simon speaks of in the tutorial video above. But it's a little confusing because both ideas are called "maps" in Erlang! Are they somehow related? They seem really different, but perhaps there is some hidden relationship between the two types of maps that I am not really aware of. Can someone please shed a little light on this for me. Thanks! 

---

I might be wrong, but I think @SimonThompson is referring to the operation of mapping (i.e. `lists:map/2`) a function on a list… like in map/reduce scenarios.
The other maps `(#{these => things})` are data structures that map keys to values.
They might seem confusing now, but don't worry… they're clearly distinguishable from each other in code.

---

Thanks for the reply, Brujo. By the way, forgive my ignorance, but why do people keep on talking about Map/Reduce. Isn't that part of Hadoop? Okay, but then what is Hadoop? Is it a programming language? Or maybe a platform for "big data" analysis? Is it like Scala's Spark? There are too many technologies out there! I am unable to keep track of all of them! :-)

---

Map/Reduce is certainly not exclusive to Hadoop. It's a programming model: https://en.wikipedia.org/wiki/MapReduce
