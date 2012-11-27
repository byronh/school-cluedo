
% Ian Lavery #36506095 t2i7
% Byron Henze #66809088 l6f7
% CS 312 Project 2 - Clue

% How to use:
% In SWI-PL, compile the program and then type "clue." to begin the prompt.

% Game Basics
character(mustard).
character(scarlet).
character(plum).
character(green).
character(white).
character(peacock).

weapon(rope).
weapon(pipe).
weapon(knife).
weapon(wrench).
weapon(candlestick).
weapon(revolver).

room(kitchen).
room(ballroom).
room(conservatory).
room(dining).
room(billiard).
room(library).
room(lounge).
room(hall).
room(study).

:- dynamic num_players/1.
:- dynamic player_character/1.
:- dynamic whose_turn/1.

:- dynamic eliminated/1.

% Read in the number of players
clue :-
    write_ln('Enter the number of players:'),
    read(X),
    assert(num_players(X)),
    set_player_character.

set_player_character :-
    write_ln('Which character piece are you controlling?'),
    read(X),
    (character(X) -> assert(player_character(X));
     write_ln('Invalid suspect.'), set_player_character),
    starting_cards.

starting_cards :-
    write_ln('Enter your starting cards. When you are finished type "done."'),
    read(X),
    (X = done -> set_turn;
     character(X) -> assert(eliminated(X)), starting_cards;
     weapon(X) -> assert(eliminated(X)), starting_cards;
     room(X) -> assert(eliminated(X)), starting_cards;
     write_ln('Invalid card.'), starting_cards).

set_turn :-
    write_ln('Whose turn is it?'),
    read(X),
    (character(X) -> assert(whose_turn(X));
     write_ln('Invalid player.'), set_turn),
    (whose_turn(X),player_character(X),should_accuse -> write_ln('You should make an accusation!'),
        remaining_character(C),write_ln(C),
        remaining_weapon(W),write_ln(W),
        remaining_room(R),write_ln(R),nl;
     record_event).

% Record a suggestion or accusation
record_event :-
    write_ln('Input s for suggestion, a for accusation, l for list of cards not yet eliminated:'),
    read(X),
    (X = s -> write_ln('Suggestion!');
     X = a -> write_ln('Handle accusation here...'), set_turn;
     X = l -> show_not_eliminated, record_event;
     write_ln('Invalid action.'), record_event).

show_not_eliminated :-
    write_ln('Remaining possible suspects:'),
    forall(remaining_character(C), write_ln(C)),nl,
    write_ln('Remaining possible weapons:'),
    forall(remaining_weapon(W), write_ln(W)),nl,
    write_ln('Remaining possible rooms:'),
    forall(remaining_room(R), write_ln(R)),nl.

remaining_character(C) :- character(C), not(eliminated(C)).
remaining_weapon(W) :- weapon(W), not(eliminated(W)).
remaining_room(R) :- room(R), not(eliminated(R)).

% You've seen a card, we know it was not involved in the crime
shown_card(X) :- assert(eliminated(X)).

% True if only one combination remains
should_accuse :-
    count_solutions(remaining_character(X),Y), Y = 1,
    count_solutions(remaining_weapon(X),Y), Y = 1,
    count_solutions(remaining_room(X),Y), Y = 1.

% Helpers

count_solutions(P,Count) :- findall(1,P,L), length(L,Count).

substring(Sub,Str) :- prefix_of(Sub,Str).
substring(Sub,[_|Str]) :- substring(Sub,Str).

prefix_of(Pre, Str) :- append(Pre, _, Str).

