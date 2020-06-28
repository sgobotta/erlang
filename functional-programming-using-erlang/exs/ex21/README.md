# FutureLearn - Erlang

## 3.10

### Programming challenge: Text processing

**We end this third week with two programming challenges, to help to consolidate what you’ve learnt so far.**

The first challenge is on **text processing**, and in the next step we’ll present a challenge based on **supermarket billing**. In both examples, as well as the ‘basic’ problem we suggest opportunities for you to take things further. And we’ll follow up each challenge with some feedback on how we set about tackling these problems.

#### Text processing

In word processing systems it is customary for lines to be filled and broken automatically, to enhance the appearance of the text. Input of the form

```text
The heat bloomed                  in December
 as the      carnival  season
                 kicked into gear.
Nearly helpless with sun and glare, I avoided Rio's brilliant
sidewalks
 and glittering beaches,
panting in dark     corners
  and waiting out the inverted southern summer.
```

would be transformed by *filling to*

```text
The heat bloomed in December as the
carnival season kicked into gear.
Nearly helpless with sun and glare,
I avoided Rio's brilliant sidewalks
and glittering beaches, panting in
dark corners and waiting out the
inverted southern summer.
```

Define a function that takes an input file in Erlang as a string (list) of characters. and a line length `len` (a positive integer) and which returns a list of lines, each of which is filled to include the maximum number of words up to the overall length `len` (including punctuation characters).

You should think about how best to represent lines in solving this problem: is a string the best representation or is there a better alternative?

#### Taking it further: justification

To take the problem further you might like to try the following.

To align the right-hand margin, the text is justified by adding extra inter-word spaces on all lines but the last:

```text
 The heat bloomed in December as the
 carnival  season  kicked into gear.
 Nearly helpless with sun and glare,
 I avoided Rio's brilliant sidewalks
 and glittering beaches, panting  in
 dark  corners  and  waiting out the
 inverted southern summer.
```

Define a function that takes a line, represented as above, and justifies it so that when printed it has exactly the length `len`.

#### Taking it further: adding commands

If we have different kinds of layout, then we need to be able to describe how a particular paragraph of text should be laid out. One way to do this is to add *formatting commands* of the form `.XX` on a line on their own. Commands could include

+ `.VB` for verbatim (so no formatting, just copy lines from input to output)
+ `.CV` lines are copied from input to output but also each line is centred (if possible),
+ `.LP` for lines filled and aligned to the left (as shown above),
+ `.RP` for lines filled and aligned to the right, and
+ `.JU` for lines filled and justified.

Define a function which takes a string containing text and formatting commands, and which outputs a string representing the formatted input.

---

© University of Kent
