-module(index).
-export([
    get_file_contents/1,show_file_contents/1,
    nopunct/1,
    index/1, test_index/0,
    test_contains/0, test_nub/0
]).

% Used to read a file into a list of lines.
% Example files available in:
%   gettysburg-address.txt (short)
%   dickens-christmas.txt  (long)
  

% Get the contents of a text file into a list of lines.
% Each line has its trailing newline removed.

get_file_contents(Name) ->
    {ok,File} = file:open(Name,[read]),
    Rev = get_all_lines(File,[]),
    lists:reverse(Rev).

% Auxiliary function for get_file_contents.
% Not exported.

get_all_lines(File,Partial) ->
    case io:get_line(File,"") of
        eof -> file:close(File),
               Partial;
        Line -> {Strip,_} = lists:split(length(Line)-1,Line),
                get_all_lines(File,[Strip|Partial])
    end.

% Show the contents of a list of strings.
% Can be used to check the results of calling get_file_contents.

show_file_contents([L|Ls]) ->
    io:format("~s~n",[L]),
    show_file_contents(Ls);
 show_file_contents([]) ->
    ok.    
     

%% 0. identify a word
%% 1. map every word with it's page
%% 2. merge word coincidences with it's ine
%% 3. identify gaps
%% 4. remove gaps and build ranges of lines
%% 5. remove uppercase of the whole file before analyzing it (normalise)
%% 6. remove special characters
%% 7. remove short words

%% @doc given a list of strings, indexes every word with it's occurrences.
-spec index([T]) -> [T].
index([]) -> [];
index(Xs) ->
    NormalisedList = lists:map(fun (X) -> nocaps(nopunct(X)) end, Xs),
    WordsByLine = lists:map(fun (X) -> nub(identify_words(X)) end, NormalisedList),
    IndexedWordsByLine = index_lines(WordsByLine),
    FlattenedIndexedWords = lists:flatten(IndexedWordsByLine),
    MergedIndexedWords = merge_words_indexes(FlattenedIndexedWords),
    MergedIndexedWords.

test_index() ->
    index(get_file_contents("dickens-christmas.txt")).
    % index(get_file_contents("gettysburg-address.txt")). %3'20''  :(

%% Helpers

%% @doc Given a list of word-index tuples, returns a word-index tuple list where
%% their indexes, representing line appearance are flattened by word.
merge_words_indexes(Xs) -> merge_words_indexes(Xs, []).

merge_words_indexes([], Accumulator) -> Accumulator;
merge_words_indexes([{Word, Index}|Xs], Accumulator) ->
    {Occurrences, FilteredList} = take_occurrences(Word, Xs, {[],[]}),
    merge_words_indexes(
        FilteredList,
        Accumulator ++ [{Word, nogaps([Index | merge_occurrences(Occurrences)], Index, [])}]
    ).

%% @doc Given a list of numbers representing text lines of a word appeance,
%% merges neighbour numbers to return a list of tuples where the first
%% component is the ealiest appearance of the word and the second component
%% is where the word was last seen before line gaps.
nogaps([], _Index, Accumulator) -> Accumulator;
nogaps([X], _Index, Accumulator) -> Accumulator ++ [{X,X}];
nogaps([X|[Y|[]]], FirstAppearance, Accumulator) ->
    case (X+1) == Y of
        true  -> Accumulator ++ [{FirstAppearance, Y}];
        false -> Accumulator ++ [{FirstAppearance, X}, {Y,Y}]
    end;
nogaps([X|[Y|Xs]], FirstAppearance, Accumulator) ->
    case (X+1) == Y of
        true  -> nogaps([Y|Xs], FirstAppearance, Accumulator);
        false -> nogaps([Y|Xs], Y, Accumulator ++ [{FirstAppearance, X}])
    end.

%% @doc Given a list of word-index tuples, extracts it's index to return
%% a list of indexes.
merge_occurrences([]) -> [];
merge_occurrences([{_Word, Index}|Xs]) -> [Index | merge_occurrences(Xs)].

%% @doc Given a word, a list of word-index tuples and an accumulator,
%% returns a tuple where the first component is the occurrences list of the
%% word, and the second component the rest of the word-index tuples that don't
%% match the given word.
take_occurrences(_X, [], Accumulator) -> Accumulator;
take_occurrences(Word, [{Word, _Index} = WordIndex|Xs], {Occurrences, Remaining}) ->
    take_occurrences(Word, Xs, {Occurrences ++ [WordIndex], Remaining});
take_occurrences(Word, [{_AnotherWord, _Index} = WordIndex|Xs], {Occurrences, Remaining}) ->
    take_occurrences(Word, Xs, {Occurrences, Remaining ++ [WordIndex]}).

%% @doc Given a list of list of words returns a list where each element is a
%% list of tuples, representing a word and the number of line each word belongs
%% to.
index_lines(Xss) -> index_lines(Xss, [], 1).

%% @doc Given a list of lists of words, an accumulator and an index, indexes
%% each line element to it's corresponding line number.
index_lines([], Accumulator, _Index) -> Accumulator;
index_lines([Xs|Xss], Accumulator, Index) ->
    index_lines(Xss, Accumulator ++ [index_line(Xs, Index)], Index + 1).

%% @doc Given a list of words and an index, returns a tuple where the first
%% component is the word, and the second the given index.
index_line([], _Index) -> [];
index_line([X|Xs], Index) -> [{X, Index} | index_line(Xs, Index)].

%% @doc Given a string, identifies words to return a list of words
-spec identify_words([Char]) -> [[Char]].
identify_words(String) -> string:tokens(String, [$\s]).

%% @doc given a string removes specials characters
nopunct([]) ->
    [];
nopunct([X|Xs]) ->
    case lists:member(X,".,\;:!?\t\n\'\"`\\-") of
	true ->
	    nopunct(Xs);
	false ->
	    [ X | nopunct(Xs) ]
    end.

% @doc Given a string, removes capital letters.
nocaps([]) ->
    [];
nocaps([X|Xs]) ->
    [ nocap(X) | nocaps(Xs) ].

nocap(X) ->
    case $A =< X andalso X =< $Z of
	true ->
	    X+32;
	false ->
	    X
    end.

%% @doc Given an element and a list tells whether the element exists in the list
%% or not.
contains(_E, []) -> false;
contains(E, [E|_Xs]) -> true;
contains(E, [_X|Xs]) -> contains(E, Xs).

test_contains() ->
  false = contains(1, []),
  false = contains(1, [2]),
  false = contains(1, [3,0,0,0,6]),
  true  = contains(1, [1]),
  true  = contains(1, [1,2,3,4,5]),
  true  = contains(1, [2,3,4,5,1]),
  true  = contains(1, [3,4,1,5,6]),
  true  = contains(1, [3,1,1,1,6]),
  {passed, "contains tests passed succesfully."}.

%% @doc Given a list returns a new list without duplicates.
nub([]) -> [];
nub([X|Xs]) ->
  case contains(X, Xs) of
    true -> nub(Xs);
    false -> [X | nub(Xs)]
  end.

test_nub() ->
  [1,2,3,4,5] = nub([1,2,3,1,2,3,1,2,3,4,5]),
  [1,2,3,4,5] = nub([5,4,3,2,1,2,3,1,2,3,1,2,3,4,5]),
  [1,2] = nub([1,2,1,2,1,2,1,2]),
  [420] = nub([420]),
  {passed, "nub tests passed succesfully."}.
