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
