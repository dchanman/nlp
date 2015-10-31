% Project 1 - Week 1 Deliverables
% Nikhil Agarwal, Derek Chan, Christopher Chou

% Part 1:
% Writing the predicate definition
%
% We will require WordNet's 's' and 'g' operators.
%	wn_s.pl: This indexes all of WordNet's words and gives them an ID
%	wn_g.pl: This contains all the definitions for all of WordNet's words (WordNet's glossary)
% 
% Import the modules with the following: 
:- consult('wordnet_prolog_2007/wn_s.pl').
:- consult('wordnet_prolog_2007/wn_g.pl').

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
:- consult('pronto_morph_engine.pl').

word_line_morphs() :- read_word(CharArray), morph_tokens_bag(w(CharArray), Bag), write(Bag).

% read_word is a helper procedure to take an input word
% from the user and split it into a character list.
%
% This code was mostly taken from Steve Wolfman's 312-pess-grammar.pl
read_word([]) :- peek_char(Ch), Ch = ' ', !, get_char(_).
read_word([]) :- peek_char(Ch), Ch = '.', !, get_char(_).
read_word([]) :- peek_char(Ch), Ch = '\n', !, get_char(_).
read_word([]) :- peek_char(Ch), Ch = 'end_of_file', !.
read_word([Ch|Chs]) :- get_char(Ch), read_word(Chs).

<<<<<<< Updated upstream
Part 3:

from 312-pess.pl
process(['words:'|L]) :-    % Process Vocabularies
		vocab(R,L,[]),		% Parse the rule.
		bug(R),				% Print it for debugging
		assert_rules(R), !.	% Assert it (them, potentially) in the DB.
		
from 312-pess-grammar.pl

%%%%%
%Parse Vocabulary
%%%%%

sentence1([]). %base case. Empty like our souls.

sentence1([Word, Type]) --> [Word], vis, det_opt, wordType(Type). %This deals with 'is' 'is a' and so on. vis refers to line 367 det_opt refers to line 312 and 333-337

% this deals with if 'and' exists or not to conjoin multiple phrases. conjuction should and both 'and' and a blank.
sentence1([Word, Word2|Type]) --> [Word], wordType(Word2), conjunction, sentence1(Type).
sentence1([Word, Word2]) --> [Word], wordType(Word2).



conjunction --> [and].	%if a line of words is separated by 'and'
conjunction --> []. % This one's a blank
wordType(n) --> [noun].	%the following are the various types.
wordType(v) --> [verb].
wordType(adj) --> [adjective].
wordType(adv) --> [adverb].		

% Part 4:
% Simplifying a goal
simplify_list([],[]).
simplify_list([H|T],[H1|T1]) :- simplify_attr(H,H1),simplify_list(T,T1).

simplify_attr(A,SimpleA) :- functor(A,attr,2), arg(2,A,[]), !, arg(1,A,X), SimpleA = X.

simplify_attr(A,SimpleA) :- functor(A,attr,2), arg(2,A,[Z]), !, functor(SimpleA,attr,2), arg(1,A,X),
	arg(1,SimpleA,X), arg(2,SimpleA,Z1), simplify_attr(Z,Z1).
	
simplify_attr(A,SimpleA) :- functor(A,attr,3), arg(3,A,[]), !, functor(Dummy,attr,2), arg(1,A,X), arg(2,A,Y),
	functor(W,X,1), arg(1,W,Y), arg(1,Dummy,W), arg(2,Dummy,[]), simplify_attr(Dummy,SimpleA).

simplify_attr(A,SimpleA)  :- functor(A,attr,3), not(arg(3,A,[])), !, functor(SimpleA,attr,2), arg(1,A,X), arg(2,A,Y),
	functor(W,X,1), arg(1,W,Y), arg(1,SimpleA,W), arg(3,A,Z), arg(2,SimpleA,Z1), simplify_list(Z,Z1).
	
simplify_attr(A,SimpleA) :- functor(A,rule,2), arg(2,A,[]), !, functor(SimpleA,fact,1), arg(1,A,H), simplify_attr(H,H1),
	arg(1,SimpleA,H1).

simplify_attr(A,SimpleA) :- functor(A,rule,2), arg(2,A,[B]), !, functor(SimpleA,rule,2), arg(1,A,H), simplify_attr(H,H1),
	arg(1,SimpleA,H1), simplify_attr(B,B1), arg(2,SimpleA,B1).


