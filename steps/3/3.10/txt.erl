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
  test_format(35),
  test_format(20),
  test_format(10),
  test_format(69),
  ?assertThrow({error}, test_format(9)),
  ?assertThrow({error}, test_format(5)),
  ?assertThrow({error}, test_format(0)).

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
    fun (Word, {LengthPerLine, RemainingLength, Acc}) ->
      WordLength = length(Word),
      case WordLength =< RemainingLength of
        true  -> {
          LengthPerLine,
          RemainingLength - WordLength - 1,
          add_space(Acc) ++ Word
        };
        false -> {
          LengthPerLine,
          LengthPerLine - WordLength - 1,
          break_line(Acc) ++ Word
        }
      end
    end,
  {_,_,FormattedText} = lists:foldl(
    AppendWord,
    {N, N-length(FirstWord)-1, FirstWord},
    Words
  ),
  FilledLines = string:tokens(FormattedText, [$\n]),
  FilledLines.

break_line(Text) -> Text ++ "\n".

add_space(Text) -> Text ++ "\s".
