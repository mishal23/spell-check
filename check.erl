-module (check).
-export ([readfile/1,deletion_edits/1,transposition_edits/1,alteration_edits/1,insertion_edits/1,edits1/1,edits2/1,known/1,add/1]).
-define (FileName, "big.txt").

% Read the whole file and makes a list of each word.
readfile(Filename) ->
  {ok,Binary} = file:read_file(Filename),
  Words = string:tokens(binary_to_list(Binary), " \v\t\r\n\'\"\\`1234567890-=!@#$%^&*()_+[]{};,./<>?:|"),
  lists:usort(lists:map(fun(Word) -> string:to_lower(Word) end, Words)).

letters() ->	
  "abcdefghijklmnopqrstuvwxyz".

% Form list of words by deleting each letter from the word
% eg. for "hello" -> ["hell","helo","helo","hllo","ello"]
deletion_edits(Word) ->	
  deletion_edits([], Word, []).

deletion_edits(_, [], Edits) ->
	Edits;
deletion_edits(Before, [Current | After], Edits) ->	
  deletion_edits([Current | Before], After, [lists:reverse(Before) ++ After | Edits]).


% Form list of words by interchanging two consecutive letters of the word
% eg. for "hello" -> ["helol","hello","hlelo","ehllo"]
transposition_edits(Word) -> 
  transposition_edits([], Word, []).

transposition_edits(_, [], Edits) -> 
  Edits;
transposition_edits(_, [_], Edits) ->	
  Edits;
transposition_edits(Before, [Current, Next | After], Edits) ->
  transposition_edits([Current | Before], [Next | After], [ lists:reverse(Before) ++ [Next, Current] ++ After | Edits]).


% Form list of words by changing one letter with each of the letters
% eg. for "me" -> ["ma","mb","mc","..","..","ae","be","ce","..",".."]
alteration_edits(Word) -> 
  alteration_edits([], Word, []).

alteration_edits(_, [], Edits) ->  
  Edits;
alteration_edits(Before, [Current | After], Edits) ->  
  BeforeR = lists:reverse(Before),
  alteration_edits([Current | Before], After, [BeforeR ++ [X] ++ After || X <- letters()] ++ Edits).


% Form list of words by inserting each letters in the word at each possible position in the word
% eg. for "me" -> ["mea","meb","mec","...","...","mae","mbe","mce","...","...","ame","bme","cme","...","..."]
insertion_edits(Word) -> 
  insertion_edits([], Word, [[X] ++ Word || X <- letters()]).

insertion_edits(_, [], Edits) -> 
  Edits;
insertion_edits(Before, [Current | After], Edits) -> 
  BeforeR = lists:reverse(Before),
  insertion_edits([Current | Before], After, [BeforeR ++ [Current, X] ++ After || X <- letters()] ++ Edits).

% Forms list with all the edits which are possible and filters the elements repeating using "lists:usort"
edits1(Word) ->
  Self = self(),
  
  start_word_workers(Self, Word),
  %% TODO: Can handle this more gracefully, throw an exception or handle an error tuple or something similar
  collect(4, Self).

start_word_workers(Collector, Word) ->
  words_worker(Collector, fun() -> deletion_edits(Word) end),
  words_worker(Collector, fun() -> transposition_edits(Word) end),
  words_worker(Collector, fun() -> alteration_edits(Word) end),
  words_worker(Collector, fun() -> insertion_edits(Word) end).

words_worker(CollectorPid, Fun) ->
  spawn(fun() ->
    Words = Fun(),
    CollectorPid ! {words, Words}
  end),
  ok.

collect(N, Parent) ->
  collect(N, Parent, []).

collect(0, Parent, Acc) ->
  lists:usort(Acc);
collect(N, Parent, Acc) when N > 0 ->
  receive
    {words, Words} ->
      collect(N - 1, Parent, Words ++ Acc)
  end.

% Forms list with 2 edits i.e adding/changing two letters in each word at each position
edits2(Word) ->
  Folded = lists:foldr(fun(A, AccIn) -> [edits1(A) | AccIn] end, [], edits1(Word)),
  lists:usort(Folded).


% Compares all the elements of the words obtained from big.txt and the words formed by edits and returns the possible list of words
known(Word) ->
  FinalDictionary = readfile(?FileName),
  % Always compare lowercase words
  LowerWord = string:to_lower(Word),
  Error1_words = edits1(LowerWord),
  SuggestedList = words_in_list(FinalDictionary, Error1_words),
  case lists:member(LowerWord,SuggestedList) of
    true -> 
      Word;
    false ->
      case length(SuggestedList) /= 0 of
        true ->
          prompt(SuggestedList);
        false ->
          Error2_words = edits2(LowerWord),
          SuggestedList2 = words_in_list(FinalDictionary, Error2_words),
          case lists:member(LowerWord,SuggestedList2) of
            true ->
              Word;
            false ->
              case length(SuggestedList2)=:=0 of
                true -> 
                  Word;
                false -> 
                  prompt(SuggestedList2)
              end
          end
      end
  end.

words_in_list(Words, List) ->
  lists:filter(fun(X) -> lists:member(X,List) end, Words).

prompt(SuggestedList) ->
  io:format("Did you mean?~n"),
  SuggestedList.

% Adds missing words to File "big.txt" only if it is not present in the file
add(Word) ->
  WordsInDictionary = readfile(?FileName),
  case lists:member(Word,WordsInDictionary) of
    true -> io:format("Word already present~n");
    false ->
            {ok, File} = file:open(?FileName, [append]),
            file:write(File," " ++ Word),
            io:format("Thank You!~nWord added to dictionary~n")
  end.
