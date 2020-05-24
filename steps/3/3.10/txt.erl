-module(txt).
-include_lib("eunit/include/eunit.hrl").
-export([
  get_file_contents/1,
  format_fills_lines_with_a_given_length_test/0,
  format/2,
  main/0
]).

main() ->
  FormattedText = format(get_file_string("text.txt"), 35),
  io:format("~s", [print_text(FormattedText, "")]).

print_text([], Text) -> Text;
print_text([X|Xs], Text) -> print_text(Xs, break_line(Text ++ X)).

test_format(MaxLineLength) ->
  %% Setup
  Text = get_file_string("text.txt"),
  %% Excersice
  FormattedLines = format(Text, MaxLineLength),
  lists:foldr(fun (Line, Acc) -> length(Line) + Acc end, 0, FormattedLines),
  %% Assertions
  assertLinesLength(FormattedLines, MaxLineLength).

format_fills_lines_with_a_given_length_test() ->
  test_format(19),
  test_format(20),
  test_format(69),
  test_format(100),
  test_format(120),
  test_format(250).

assertLinesLength([], _) -> void;
assertLinesLength([Line|Lines], MaxLineLength) when length(Line) =< MaxLineLength ->
  ?assert(length(Line) =< MaxLineLength),
  assertLinesLength(Lines, MaxLineLength);
assertLinesLength(_,_) -> throw({error}).


get_file_string(FileName) ->
  Lines = get_file_contents(FileName),
  lists:foldr(fun (Line, Acc) -> Line ++ Acc end, "", Lines).

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
    Line -> {Strip,_} = lists:split(length(Line),Line),
      get_all_lines(File,[Strip|Partial])
  end.

format(Text, N) ->
  [FirstWord|Words] = string:tokens(Text, [$\s,$\n]),
  AppendWord =
    fun (Word, {LengthPerLine, RemainingLength, CurrentLine, Acc}) ->
      WordLength = length(Word),
      case WordLength =< RemainingLength of
        true ->
          {
            LengthPerLine,
            RemainingLength - WordLength - 1,
            add_space(CurrentLine) ++ Word,
            Acc
          };
        false ->
          CurrentFormattedLine = break_line(
            fill_with_spaces(CurrentLine, LengthPerLine - length(CurrentLine))
          ),
          {
            LengthPerLine,
            LengthPerLine - WordLength - 1,
            Word,
            Acc ++ CurrentFormattedLine
          }
      end
    end,
  {_,_,Leftover,FormattedText} = lists:foldl(
    AppendWord,
    {N, N-length(FirstWord)-1, FirstWord, ""},
    Words
  ),
  FilledLines = string:tokens(FormattedText ++ Leftover, [$\n]),
  FilledLines.

break_line(Text) -> Text ++ "\n".

add_space(Text) -> Text ++ [$\s].

%% @doc Given a string and an integer, adds as many spaces as specified by N to
%% return a new formatted string.
fill_with_spaces(Text, 0) -> Text;
fill_with_spaces(Text, N) ->
  FormattedText = fill_with_spaces(Text, N, ""),
  FormattedText.

%% @doc Given a string, an integer and an accumulator, adds as many spaces as
%% specified by N, to return an accumulator with a formatted string.
fill_with_spaces(Xs, 0, Acc) -> Acc ++ Xs;
fill_with_spaces([], N, Acc) -> fill_with_spaces(Acc, N, "");
fill_with_spaces([X|Xs], N, Acc) ->
    case [X] == [$\s] of
      true  -> fill_with_spaces(Xs, N-1, Acc ++ [X,X]);
      false -> fill_with_spaces(Xs, N, Acc ++ [X])
    end.

fill_with_spaces_test() ->
  ?assertEqual(
    "Monsters  around  my  neck",
    fill_with_spaces("Monsters  around  my  neck", 0)
  ),
  ?assertEqual(
    "Monsters  around  my neck",
    fill_with_spaces("Monsters around my neck", 2)
  ),
  ?assertEqual(
    "Monsters  around  my  neck",
    fill_with_spaces("Monsters around my neck", 3)
  ),
  ?assertEqual(
    "Monsters   around  my  neck",
    fill_with_spaces("Monsters around my neck", 4)
  ).
