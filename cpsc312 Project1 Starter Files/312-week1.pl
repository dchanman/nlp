%

% Part 1:
% Writing the predicate definition
%
% We will require WordNet's 's' and 'g' operators.
%	wn_s.pl: This indexes all of WordNet's words and gives them an ID
%	wn_g.pl: This contains all the definitions for all of WordNet's words (WordNet's glossary)
% 
% Import the modules with the following: 
% 	[wordnet_prolog_2007/wn_s].
% 	[wordnet_prolog_2007/wn_g].

definition(Word, Meaning) :- 
	s(Synset_id, _, Word, _, _, _), % Get the Synset_id from s
	g(Synset_id, Meaning). % Get the Meaning from g (the glossary)

% Part 2:
% Retrieving morphological phrasings
%
% We will require ProNTo_Morph's engine to get all of the 
% morphological phrasings.
% 
% Import ProNTo_Morph with the following:
% 	[pronto_morph_engine].

word_line_morphs() :- read_word(CharArray), morph_tokens_bag(w(CharArray), Bag), write(Bag).

% read_word is a helper procedure to take an input word
% from the user and split it into a character list.
%
% This code was mostly taken from Steve Wolfman's 312-pess-grammar.pl
read_word([]) :- peek_char(Ch), Ch = ' ', !.
read_word([]) :- peek_char(Ch), Ch = '.', !.
read_word([]) :- peek_char(Ch), Ch = 'end_of_file', !.
read_word([]) :- peek_char(Ch), Ch = '\n', !.
read_word([Ch|Chs]) :- get_char(Ch), read_word(Chs).
