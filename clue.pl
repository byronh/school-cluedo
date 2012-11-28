
% Ian Lavery #36506095 t2i7
% Byron Henze #66809088 l6f7
% CS 312 Project 2 - Clue

% How to use:
% In SWI-PL, compile the program and then type "clue." to begin the prompt.


clue :- input_num_players.  % Top level procedure


% GAME BASICS

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


% DYNAMIC RULES

% The number of players playing the game
:- dynamic num_players/1.

% The player number of the person using this assistant
:- dynamic player_num/1.

% The players, identified by a number (their turn order)
:- dynamic player/1.

% The room that the player is currently in
:- dynamic player_location/1.

% The information we know about a particular card and player. The arguments are as follows:
% 1. Card
% 2. Player #
% 3. Status: -1 if player is known not to have that card, 0 if they have it for sure, and a positive integer otherwise
:- dynamic cardstatus/3.


% GAME SETUP

% Input the number of players
input_num_players :-
    clear,
    write_ln('Hello! I am your personal Clue Assistant.'),
    nl,
    write_ln('How many players are there?'),
    read(NumPlayers),
    assert(num_players(NumPlayers)),
    input_players(NumPlayers),
    nl,
    input_player_num.

% Add the players (each represented by a number from 1 to NumPlayers) to the knowledge base
input_players(0).
input_players(I) :-
    I > 0,
    asserta(player(I)),
    J is I - 1,
    input_players(J).

% Input which player number you are
input_player_num :-
    write_ln('What player number are you?'),
    read(PlayerNum),
    assert(player_num(PlayerNum)),
    nl,
    input_starting_cards(PlayerNum).

% Input the cards you started with
input_starting_cards(Player) :-
    write_ln('Enter a card you started with. If you are finished type "done."'),
    read(Card),
    (
        Card = done -> nl, record_event;
        valid_card(Card) -> input_card(Card,Player),input_starting_cards(Player);
        write_ln('Invalid card, please try again.'),input_starting_cards(Player)
    ).


% MAIN LOOP

% Record knowledge we have acquired at any point in the game
% TODO:
% record an accusation
% receive recommended action
% view list of eliminated cards
record_event :-
    write_ln('What would you like to do? (Enter the number of the choice you want)'),
    write_ln('1. Change what room I am in'),
    write_ln('2. Record a suggestion made by me'),
    write_ln('3. Record a suggestion made by another player'),
    write_ln('4. Receive a recommendation on what action to take'),
    write_ln('5. Exit'),
    read(Choice),
    (
        Choice = 1 -> nl,change_room;
        Choice = 2 -> nl,record_suggestion_me;
        Choice = 3 -> nl,record_suggestion_other;
        Choice = 4 -> nl,receive_recommendation;
        Choice = 5 -> clear,halt;
        write_ln('Invalid action, please try again.'),nl,record_event
    ).

change_room :-
    retractall(player_location(_)),
    write_ln('What room are you in now?'),
    read(Room),
    (
        room(Room) -> assert(player_location(Room)),nl,record_event;
        write_ln('Invalid room, please try again.'),nl,change_room
    ).

% TODO: check if you suggested a card you already know about
record_suggestion_me :-
    (
        player_location(R),room(R) -> true;
        write_ln('You can not make a suggestion because you are not in a room.'),nl,record_event    
    ),
    write_ln('What suspect did you suggest?'),
    read(Suspect),
    (
        character(Suspect) -> true;
        write_ln('Invalid suspect, please try again.'),nl,record_suggestion_me
    ),
    write_ln('What weapon did you suggest?'),
    read(Weapon),
    (
        weapon(Weapon) -> true;
        write_ln('Invalid weapon, please try again.'),nl,record_suggestion_me
    ),
    write_ln('Did someone show you a card? ("y." or "n.")'),
    read(Shown),
    (
        Shown = y -> record_shown_card;
        % TODO: make it so that the loop doesn't exit after saying "n."
        player_location(Room),record_no_one_has_cards([Suspect,Weapon,Room]),nl,record_event
    ).

record_shown_card :- 
    write_ln('What card was shown to you?'),
    read(Card),
    write_ln('What player showed you the card?'),
    read(OtherPlayer),
    (
        valid_card(Card),is_another_player(OtherPlayer) -> input_card(Card,OtherPlayer),nl,record_event;
        write_ln('Invalid card or player, please try again.'),nl,record_shown_card
    ).

record_no_one_has_cards([H|T]) :-
    is_another_player(Player),
    retractall(cardstatus(H,Player,-1)), % remove duplicates
    assert(cardstatus(H,Player,-1)),
    record_no_one_has_cards(T).


% TODO: Finish this
record_suggestion_other :- record_event.
%    write_ln('Which player number made the suggestion?'),
%    read(PlayerNum),
%    write_ln('What suspect was suggested?'),
%    read(Suspect),
%    write_ln('What weapon was suggested?'),
%    read(Weapon),
%    write_ln('What room was suggested?'),
%    read(Room),

receive_recommendation :-
    (
        should_accuse ->
            % TODO: print out the 3 remaining possibilities
            write_ln('You should make an accusation!'),nl,record_event;
        write_ln('I do not have any recommendation at this time.'),nl,record_event
    ).


% CLUE FUNCTIONS

% Record a card that you have actually seen
% TODO: Also add cardstatus's with -1 to all the other players since they can't possibly have this card
input_card(Card,Player) :- assert(cardstatus(Card,Player,0)).

% Determines if a card is valid character, weapon, or room
valid_card(Card) :- character(Card);weapon(Card);room(Card).

% Determines if a player is a valid player and not me
is_another_player(OtherPlayer) :- player(OtherPlayer),player_num(Player),not(Player = OtherPlayer).

% Fallback predicate for when every single card has been revealed to you and using no other information
% Hopefully this case won't be needed
should_accuse :- count_solutions(cardstatus(_,_,0),Count),Count = 3.


% HELPER FUNCTIONS

% Counts the number of solutions to a given predicate
count_solutions(P,Count) :- findall(1,P,L),length(L,Count).

% Clears the screen - borrowed from http://osdir.com/ml/lang.swi-prolog.general/2006-12/msg00079.html
clear :- format('~c~s~c~s',[0x1b,"[H",0x1b,"[2J"]).


