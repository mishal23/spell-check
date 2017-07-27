-module (check).
-export ([readfile/1,deletion_edits/1,transposition_edits/1,alteration_edits/1,insertion_edits/1,edits1/1,edits2/1,known/1]).

% Read the whole file and makes a list of each word.
%% 3rd Release:- Following code takes less time
readfile(Filename) ->
  {ok,Binary} = file:read_file(Filename),
    Words = string:tokens(binary_to_list(Binary), " \v\t\r\n\'\"\\`1234567890-=!@#$%^&*()_+[]{};,./<>?:|"),
    lists:usort(lists:map(fun(Word) -> string:to_lower(Word) end, Words)).

%% 2nd Release:- Following code takes more time to read compared to the above one
%readfile(FileName) -> {ok,Binary} = file:read_file(FileName),
%%                      re:split(binary_to_list(Binary), "[^a-zA-Z]").
%                      lists:usort(lists:map(fun(X) -> string:to_lower(binary_to_list(X)) end,(re:split(binary_to_list(Binary),"[^a-zA-Z]")))).
letters() ->	"abcdefghijklmnopqrstuvwxyz".

% Form list of words by deleting each letter from the word
% eg. for "hello" -> ["hell","helo","helo","hllo","ello"]
deletion_edits(Word) ->	deletion_edits([], Word, []).

deletion_edits(_, [], Edits) ->	Edits;
deletion_edits(Before, [Current | After], Edits) ->	deletion_edits([Current | Before], After,[lists:reverse(Before) ++ After | Edits]).


% Form list of words by interchanging two consecutive letters of the word
% eg. for "hello" -> ["helol","hello","hlelo","ehllo"]
transposition_edits(Word) -> transposition_edits([], Word, []).

transposition_edits(_, [], Edits) -> Edits;
transposition_edits(_, [_], Edits) ->	Edits;
transposition_edits(Before, [Current, Next | After], Edits) ->	transposition_edits([Current | Before], [Next | After],[ lists:reverse(Before) ++ [Next, Current] ++ After | Edits]).


% Form list of words by changing one letter with each of the letters
% eg. for "me" -> ["ma","mb","mc","..","..","ae","be","ce","..",".."]
alteration_edits(Word) -> alteration_edits([], Word, []).

alteration_edits(_, [], Edits) ->  Edits;
alteration_edits(Before, [Current | After], Edits) ->  BeforeR = lists:reverse(Before),
                                                        alteration_edits([Current | Before], After, [BeforeR ++ [X] ++ After || X <- letters()] ++ Edits).


% Form list of words by inserting each letters in the word at each possible position in the word
% eg. for "me" -> ["mea","meb","mec","...","...","mae","mbe","mce","...","...","ame","bme","cme","...","..."]
insertion_edits(Word) -> insertion_edits([], Word, [[X] ++ Word || X <- letters()]).

insertion_edits(_, [], Edits) -> Edits;
insertion_edits(Before, [Current | After], Edits) -> BeforeR = lists:reverse(Before),
                                                      insertion_edits([Current | Before], After, [BeforeR ++ [Current, X] ++ After || X <- letters()] ++ Edits).


% Forms list with all the edits which are possible and filters the elements repeating using "lists:usort"
edits1(Word) -> lists:usort(deletion_edits(Word) ++ transposition_edits(Word) ++ alteration_edits(Word) ++ insertion_edits(Word)).


% Forms list with 2 edits i.e adding/changing two letters in each word at each position
edits2(Word) -> lists:usort(lists:foldr(fun(A, AccIn) -> AccIn ++ edits1(A) end, [], edits1(Word))).


% Compares all the elements of the words obtained from big.txt and the words formed by edits and returns the possible list of words
known(Word) ->  FinalDictionary = readfile("big.txt"),
%                FinalDictionary = lists:map(fun(X) -> string:to_lower(binary_to_list(X)) end,Words_in_dictionary),
                Error1_words = edits1(string:to_lower(Word)),
%                Error2_words = edits1(string:to_lower(Word)),
%                AppendedList = lists:usort(lists:append(Error1_words,Error2_words)),
                SuggestedList = lists:filter(fun(X) -> lists:member(X,FinalDictionary) end,Error1_words),
                case lists:member(string:to_lower(Word),SuggestedList) of
                    true -> Word;
                    false ->  case length(SuggestedList)=/=0 of
                               true -> io:format("Did you mean?~n"),
                                      SuggestedList;
                               false -> Error2_words = edits2(string:to_lower(Word)), 
                                        SuggestedList2 = lists:filter(fun(X) -> lists:member(X,FinalDictionary) end,Error2_words),
                                        case lists:member(string:to_lower(Word),SuggestedList2) of
                                          true -> Word;
                                          false ->  case length(SuggestedList2)=:=0 of
                                                      true -> Word;
                                                      false -> io:format("Did you mean?~n"), 
                                                            SuggestedList2
                                                    end
                                        end
                              end
                end.

