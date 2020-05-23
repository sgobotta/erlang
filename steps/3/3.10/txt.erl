-module(txt).
-include_lib("eunit/include/eunit.hrl").
-export([
  main/1,
  main_test/0
]).

main_test() ->
  main("text.txt").

main(FileName) ->
  Lines = get_file_contents(FileName),
  Text = lists:foldr(fun (Line, Acc) -> Line ++ Acc end, "", Lines),
  format(Text, 35).

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
  {_,_,FormattedText} = lists:foldl(
    fun (Word, {LengthPerLine, RemainingLength, Acc}) ->
      case length(Word) =< RemainingLength of
        true  -> {
          LengthPerLine,
          RemainingLength - length(Word) - 1,
          add_space(Acc) ++ Word
        };
        false -> {
          LengthPerLine,
          LengthPerLine - length(Word) - 1,
          break_line(Acc) ++ Word
        }
      end
    end,
    {N, N-length(FirstWord)-1, FirstWord},
    Words
  ),
  FilledLines = string:tokens(FormattedText, [$\n]),
  FilledLines.

break_line(Text) -> Text ++ "\n".

add_space(Text) -> Text ++ "\s".

% "TheheatbloomedinDecemberasthe\ncarnivalseasonkickedintogear.Nearly\nhelplesswithsunandglare,Iavoided\nRio'sbrilliantsidewalksand\nglitteringbeaches,pantingindark\ncornersandwaitingouttheinverted\nsouthernsummer."

% ["The heat bloomed in December as th",
%  "carnival season kicked into gear",
%  "Nearly helpless with sun and glare",
%  "I avoided Rio's brilliant sidewalk",
%  "and glittering beaches, panting i",
%  "dark corners and waiting out th",
%  "inverted southern summer."]
