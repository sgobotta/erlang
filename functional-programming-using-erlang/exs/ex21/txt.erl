-module(txt).
-author("@sgobotta").
-include_lib("eunit/include/eunit.hrl").
-export([
  get_file_contents/1,
  format/3,
  main/2
]).

%% @doc Given a command and a line length, reads the text.txt file to output
%% a formatted text.
%% Allowed commands are strings of the form: .VB, .CV, .LP, .RP, .JU.
main(Command, N) ->
  FormattedText = format(get_file_string("text.txt"), N, Command),
  io:format("~s", [print_text(FormattedText, "")]).

print_text([], Text) -> Text;
print_text([X|Xs], Text) -> print_text(Xs, break_line(Text ++ X)).

test_format(Command, MaxLineLength) ->
  %% Setup
  Text = get_file_string("text.txt"),
  %% Excersice
  FormattedLines = format(Text, MaxLineLength, Command),
  lists:foldr(fun (Line, Acc) -> length(Line) + Acc end, 0, FormattedLines),
  %% Assertions
  assertLinesLength(FormattedLines, MaxLineLength).

format_text_with_filled_lines_aligned_to_the_left_with_a_given_length_test() ->
  test_format(".LP", 19),
  test_format(".LP", 20),
  test_format(".LP", 69),
  test_format(".LP", 100),
  test_format(".LP", 120),
  test_format(".LP", 250).

format_text_without_line_formatting_test() ->
  test_format(".VB", 19),
  test_format(".VB", 20),
  test_format(".VB", 69),
  test_format(".VB", 100),
  test_format(".VB", 120),
  test_format(".VB", 250).

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

%% @doc Given a string, a line length and a command, returns an array of strings
%% where each element is a formatted line of the given text.
-spec format(string(), integer(), string()) -> [string()].
format(Text, N, Command) ->
  [FirstWord|Words] = string:tokens(Text, [$\s,$\n]),
  AppendWord =
    %% Given a word, checks whether it should be added or a line break must be
    %% performed to th current line.
    fun (Word, {LengthPerLine, RemainingLength, CurrentLine, Acc, C}) ->
      WordLength = length(Word),
      case WordLength =< RemainingLength of
        %% returns a tuple where the third component is a line with a word
        %% appended at the end, and the second component represents how many
        %% more characters can be added.
        true ->
          {
            LengthPerLine,
            RemainingLength - WordLength - 1,
            add_space(CurrentLine) ++ Word,
            Acc,
            C
          };
        %% returns a tuple where the fourth component is the whole text with
        %% a word appended to the last line break and the second component is
        %% reseted.
        false ->
          CurrentFormattedLine = break_line(
            apply_command(C, CurrentLine, LengthPerLine - length(CurrentLine), false)
          ),
          {
            LengthPerLine,
            LengthPerLine - WordLength - 1,
            Word,
            Acc ++ CurrentFormattedLine,
            C
          }
      end
    end,
  {_,_,LastLine,FormattedText,_} = lists:foldl(
    AppendWord,
    {N, N-length(FirstWord)-1, FirstWord, "", Command},
    Words
  ),
  FormattedLastLine = apply_command(Command, LastLine, N - length(LastLine), true),
  FilledLines = string:tokens(FormattedText ++ FormattedLastLine, [$\n]),
  FilledLines.

%% @doc Given a command, a text line, an integer representing a number of
%% characters and a boolean, returns a formatted string.
-spec apply_command(string(), string(), integer(), boolean()) -> string().
apply_command(".LP", CurrentLine, RemainingLength, false) ->
  fill_with_spaces(CurrentLine, RemainingLength);
apply_command(".LP", CurrentLine, _RemainingLength, true) ->
  fill_with_spaces(CurrentLine, 0);
apply_command(".VB", CurrentLine, _RemainingLength, _IsTheLastLine) -> CurrentLine;
apply_command(".RP", CurrentLine, RemainingLength, false) ->
  lists:reverse(fill_with_spaces(lists:reverse(CurrentLine), RemainingLength));
apply_command(".RP", CurrentLine, RemainingLength, true) ->
  align_right(CurrentLine, RemainingLength);
apply_command(".CV", CurrentLine, RemainingLength, _IsTheLastLine) ->
  center(CurrentLine, RemainingLength);
apply_command(".JU", CurrentLine, RemainingLength, _IsTheLastLine) ->
  fill_with_spaces(CurrentLine, RemainingLength).

%% @doc Given a string returns a new string with a new line attached at the end.
break_line(Text) -> Text ++ "\n".

%% @doc Given a string returns a new string with a space at the end.
add_space(Text) -> Text ++ [$\s].

%% @doc Given a string and a number of spaces, returns a new string aligned to
%% the right.
align_right(Text, 0) -> Text;
align_right(Text, N) -> align_right([$\s] ++ Text, N-1).

a_text_line_is_aligned_to_the_right_test() ->
  % Setup
  TextLine = "There're Monsters around my neck.",
  % Exercices + ASsertions
  ?assertEqual(" There're Monsters around my neck.", align_right(TextLine, 1)),
  ?assertEqual("  There're Monsters around my neck.", align_right(TextLine, 2)),
  ?assertEqual("    There're Monsters around my neck.", align_right(TextLine, 4)),
  ?assertEqual("     There're Monsters around my neck.", align_right(TextLine, 5)),
  ?assertEqual("       There're Monsters around my neck.", align_right(TextLine, 7)).

%% @doc Given a string and a number of spaces, returns a new string with N
%% additional left and right padding.
center(Text, 0) -> Text;
center(Text, N) when (N rem 2) == 0 -> center([$\s] ++ Text, N-1);
center(Text, N) -> center(Text ++ [$\s], N-1).

a_text_line_is_centered_test() ->
  % Setup
  TextLine = "There're Monsters around my neck.",
  % Exercises + Assertions
  ?assertEqual("There're Monsters around my neck. ", center(TextLine, 1)),
  ?assertEqual(" There're Monsters around my neck. ", center(TextLine, 2)),
  ?assertEqual("  There're Monsters around my neck.  ", center(TextLine, 4)),
  ?assertEqual("  There're Monsters around my neck.   ", center(TextLine, 5)),
  ?assertEqual("   There're Monsters around my neck.    ", center(TextLine, 7)).

%% @doc Given a string and an integer, adds as many spaces as specified by N to
%% return a new formatted string.
fill_with_spaces(Text, 0) -> Text;
fill_with_spaces(Text, N) ->
  FormattedText = fill_with_spaces(Text, N, ""),
  FormattedText.

%% @doc Given a string, an integer and an accumulator, adds as many spaces as
%% specified by N, to return an accumulator with a formatted string with its
%% lines justified and aligned to the left.
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
