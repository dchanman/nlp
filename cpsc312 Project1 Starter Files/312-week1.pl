% Project 1 - Week 1 Deliverables
% Nikhil Agarwal(53835138), Derek Chan(33184128), Christopher Chou(35592120)

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


% Part 3:
% This question is in 312-pess.pl starting on line 352 
% and updated grammar is in 312-pess-grammar.pl starting on line 572



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


