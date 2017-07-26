# Spell-Checker
- Inspired by the spelling checker in various Search-Engines, Office Packages and many more, here is an attempt to implement spelling-corrector in Erlang.
- [Norvig](https://research.google.com/pubs/author205.html)(**Director of Research at Google Inc**) in 2007 had released the Toy Spelling Corrector in Python,achieving 80 or 90% accuracy at a processing speed of at least 10 words per second in about half a page of code.
- He had released it after his two friends [Dean](https://en.wikipedia.org/wiki/Jeff_Dean_(computer_scientist)) and [Bill](https://en.wikipedia.org/wiki/Bill_Maris) were amazed at Google's spelling correction and did not have good intuitions about how the process works,though being highly accomplished engineers and mathematicians.

### Implementation
- It takes reference of words from [big.txt](https://github.com/mishal23/spell-check/blob/master/big.txt) which has about a million words(The same was used by Norvig in his implementation of Spell-Corrector).
- All the words of the file [big.txt](https://github.com/mishal23/spell-check/blob/master/big.txt) are splitted and saved as a list.
- New list is formed with various edits from the 4 functions( ```deletion_edits```, ```transposition_edits```, ```alteration_edits```, ```insertion_edits```).
- After which list is filtered by comparing the words of list formed by [big.txt](https://github.com/mishal23/spell-check/blob/master/big.txt) and the list formed by various edits, and returns a list with the similarities found. 

### Steps to Run
- Clone the repository after forking it and then head to the Erlang Shell.
- Change the directory to cloned repository.
- Compile it.
- Input a word in double quotes and check the recommendations given.
- For my system after heading to Erlang Shell, it is as follows
```
1> cd("C:/Users/Mishal Shah/Desktop/Erlang"). 
C:/Users/Mishal Shah/Desktop/Erlang
ok
2> c(check).                                  
check.erl:8: Warning: variable 'Words_in_dictionary' is unused
{ok,check}
3> check:known("helo").
Did you mean?
["felo","halo","held","hell","hello","helm","help","hero"]
4>  check:known("seach"). 
Did you mean?
["beach","each","reach","search","teach"]
5>  check:known("somthing").
Did you mean?
["something","soothing"]
```
### Timer
- For 1st Release
<table>
<tr>
<td>Word</td>
<td>Time(in seconds)</td>
</tr>
<tr>
<td>somthing</td>
<td>13.3</td>
</tr>
<tr>
<td>seach</td>
<td>9.5</td>
</tr>
<tr>
<td>helo</td>
<td>8.2</td>
</tr>
</table>

### Future Scope
- [ ] Work on run-time speed.
- [ ] Work on increasing accuracy.
- [ ] Work on spell-checker in more than one word.

### License
- This repository is under [MIT License](https://github.com/mishal23/spell-check/blob/master/LICENSE)
