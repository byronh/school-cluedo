
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

% Read in the number of players
clue :- abolish(num_players/1),
        write_ln('Enter the number of players:'),
        read(X),
        assert(num_players(X)),
        set_player_character.

set_player_character :- abolish(player_character/1),
                        write_ln('Which character piece are you controlling?'),
                        read(X),
                        (character(X) -> assert(player_character(X));
                         write_ln('Invalid suspect.'), set_player_character),
                        set_turn.

%starting_cards :- write_ln('Enter your starting cards'),


set_turn :- write_ln('Whose turn is it?'),
            read(X),
            (character(X) -> assert(whose_turn(X));
             write_ln('Invalid player.'), set_turn),
            (whose_turn(X),player_character(X),should_accuse -> write_ln('You should make an accusation!'),
                character(C),not(eliminated(C)),write_ln(C),
                weapon(W),not(eliminated(W)),write_ln(W),
                room(R),not(eliminated(R)),write_ln(R),nl;
             record_event).

% Record a suggestion or accusation
record_event :- write_ln('What action was taken? Input s for suggestion, a for accusation:'),
                read(X),
                (X = s -> write_ln('Suggestion!');
                 X = a -> write_ln('Handle accusation here...'), set_turn;
                 write_ln('Invalid action.'), record_event).

% You've seen a card, we know it was not involved in the crime
:- dynamic eliminated/1.
shown_card(X) :- assert(eliminated(X)).

% True if only one combination remains
should_accuse :- count_solutions((character(X),not(eliminated(X))),Y), Y = 1,
                 count_solutions((weapon(X),not(eliminated(X))),Y), Y = 1,
                 count_solutions((room(X),not(eliminated(X))),Y), Y = 1.

% Helpers

count_solutions(P,Count) :- findall(1,P,L), length(L,Count).

substring(Sub,Str) :- prefix_of(Sub,Str).
substring(Sub,[_|Str]) :- substring(Sub,Str).

prefix_of(Pre, Str) :- append(Pre, _, Str).

