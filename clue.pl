% Ian Lavery #36506095 t2i7
% Byron Henze #66809088 l6f7
% CS 312 Project 2 - Clue

% How to use:
% In SWI-PL, compile the program and then type "clue." to begin the prompt.


clue :- input_num_players.  % Top level procedure


% GAME BASICS

character(mustard).
character(scarlet).
%character(plum).
%character(green).
%character(white).
%character(peacock).

weapon(rope).
weapon(pipe).
%weapon(knife).
%weapon(wrench).
%weapon(candlestick).
%weapon(revolver).

room(kitchen).
room(ballroom).
%room(conservatory).
%room(dining).
%room(billiard).
%room(library).
%room(lounge).
%room(hall).
%room(study).


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

% Records how many times any given player has shown a card to another
:- dynamic cards_shown/2.

% The solutions to the crime, if we are completely sure about it
:- dynamic solution/1.


% GAME SETUP

% Input the number of players
input_num_players :-
    clear,
    write_ln('Hello! I am your personal Clue Assistant.'),
    nl,
    retractall(num_players(_)),
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
    asserta(cards_shown(I,0)),
    J is I - 1,
    input_players(J).

% Input which player number you are
input_player_num :-
    retractall(player_num(_)),
    write_ln('What player number are you?'),
    read(PlayerNum),
    assert(player_num(PlayerNum)),
    nl,
    input_starting_cards(PlayerNum).

% Input the cards you started with
input_starting_cards(Player) :-
    start_with_no_cards(Player),
    write_ln('Enter a card you started with. If you are finished type "done."'),
    read(Card),
    (
        Card = done -> nl, record_event;
        valid_card(Card) -> num_players(NumPlayers),input_card(Card,Player,NumPlayers),input_starting_cards(Player);
        write_ln('Invalid card, please try again.'),input_starting_cards(Player)
    ).

% Initially marks all cards as cards we do not have. Inputting the starting cards will override this
start_with_no_cards(Player) :-
    assert(cardstatus(mustard,Player,-1)),
    assert(cardstatus(scarlet,Player,-1)),
    %assert(cardstatus(plum,Player,-1)),
    %assert(cardstatus(green,Player,-1)),
    %assert(cardstatus(white,Player,-1)),
    %assert(cardstatus(peacock,Player,-1)),

    assert(cardstatus(rope,Player,-1)),
    assert(cardstatus(pipe,Player,-1)),
    %assert(cardstatus(knife,Player,-1)),
    %assert(cardstatus(wrench,Player,-1)),
    %assert(cardstatus(candlestick,Player,-1)),
    %assert(cardstatus(revolver,Player,-1)),

    assert(cardstatus(kitchen,Player,-1)),
    assert(cardstatus(ballroom,Player,-1)).
    %assert(cardstatus(conservatory,Player,-1)),
    %assert(cardstatus(dining,Player,-1)),
    %assert(cardstatus(billiard,Player,-1)),
    %assert(cardstatus(library,Player,-1)),
    %assert(cardstatus(lounge,Player,-1)),
    %assert(cardstatus(hall,Player,-1)),
    %assert(cardstatus(study,Player,-1)).


% MAIN LOOP

% Main menu - record knowledge we have acquired at any point in the game
record_event :-
    (
        should_accuse ->
            write_ln('You should make an accusation!'),nl,
            listing(solution(_)),nl,nl,nl;
        true
    ),
    write_ln('What would you like to do? (Enter the number of the choice you want)'),
    write_ln('1. Change what room I am in'),
    write_ln('2. Record a suggestion made by me'),
    write_ln('3. Record a suggestion made by another player'),
    write_ln('4. Advanced: View knowledge base'),
    write_ln('5. Exit'),
    read(Choice),
    (
        Choice = 1 -> nl,change_room;
        Choice = 2 -> nl,record_suggestion_me;
        Choice = 3 -> nl,record_suggestion_other;
        Choice = 4 -> nl,show_knowledge_base;
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
        player_location(Room),player_num(Player),num_players(NumPlayers),
        record_no_one_else_showed_card(Player,Suspect,NumPlayers),
        record_no_one_else_showed_card(Player,Weapon,NumPlayers),
        record_no_one_else_showed_card(Player,Room,NumPlayers),
        nl, record_event
    ).

record_shown_card :-
    write_ln('What card was shown to you?'),
    read(Card),
    write_ln('What player showed you the card?'),
    read(OtherPlayer),
    (
        valid_card(Card),is_another_player(OtherPlayer) ->
            num_players(NumPlayers),input_card(Card,OtherPlayer,NumPlayers),nl,record_event;
        write_ln('Invalid card or player, please try again.'),nl,record_shown_card
    ).

% Records the results of the suggestion of another player
record_suggestion_other :-
    write_ln('Which player number made the suggestion?'),
    read(Player),
    (
    is_another_player(Player)->true;
    write_ln('Invalid player, please reenter suggestion'), nl, record_suggestion_other
    ),
    write_ln('What suspect was suggested?'),
    read(Suspect),
    (
    character(Suspect)->true;
    write_ln('Invalid suspect, please reenter suggestion'), nl, record_suggestion_other
    ),
    write_ln('What weapon was suggested?'),
    read(Weapon),
    (
    weapon(Weapon)->true;
    write_ln('Invalid weapon, please reenter suggestion'), nl, record_suggestion_other
    ),
    write_ln('What room was suggested?'),
    read(Room),
    (
    room(Room)->true;
    write_ln('Invalid room, please reenter suggestion'), nl, record_suggestion_other
    ),
    write_ln('Did someone show them a card? (y or n)'),
    read(Shown),
        (
          Shown = y -> check_shown(Player, Suspect,Weapon,Room);
          Shown = n ->num_players(NumPlayers),
           record_no_one_else_showed_card(Player,Suspect,NumPlayers),
           record_no_one_else_showed_card(Player,Weapon,NumPlayers),
           record_no_one_else_showed_card(Player,Room,NumPlayers),
           nl, record_event;
          write_ln('Invalid input, please reenter suggestion'), nl, record_suggestion_other
          
        ).

% If the suggestion of an opponent causes another player to show them a card		
check_shown(Player, Suspect, Weapon, Room) :-
        write_ln('Which player showed their card?'),
        read(PlayerShowing),
        (
        is_another_player(PlayerShowing) -> record_shown_card_other(PlayerShowing,[Suspect,Weapon,Room]), nl, record_event;
                write_ln('Invalid player entered, please try again.'), nl, check_shown(Player, Suspect, Weapon, Room)
        ).

% Records the results of another player showing an opponent a card		
record_shown_card_other(_,[]).
record_shown_card_other(Player,[Card|T]) :- inc_cards_shown(Player),
    (
        % Case 1: No rule exists yet about that player and card. Create it with integer 1
        not(cardstatus(Card,Player,_)) -> assert(cardstatus(Card,Player,1));
        % Case 2: Rule exists about that player and card, positive integer. Increment the integer
        cardstatus(Card,Player,I),I > 0 -> increment(I,J), retract(cardstatus(Card,Player,I)),assert(cardstatus(Card,Player,J));
        % Otherwise: The rule exists with a 0 or -1, don't do anything (yet)
        true
    ),
    record_shown_card_other(Player,T).
    
% increments the total number of shown cards for a particular player
inc_cards_shown(Player) :-
  cards_shown(Player, X),
  increment(X,Y),
  retract(cards_shown(Player,X)),
  assert(cards_shown(Player,Y)).

% Spits out the whole knowledge base
show_knowledge_base :-
    write_ln('Knowledge base:'),
    listing(cardstatus),
    record_event.


% CLUE FUNCTIONS

% Record a card that you have actually seen
% Flag the player who showed it with a 0, and all others with -1
% Remove any duplicate identical rules if necessary
input_card(_,_,0).
input_card(Card,Player,PlayerNum) :-
    PlayerNum > 0,
    retractall(cardstatus(Card,Player,0)),
    retractall(cardstatus(Card,Player,-1)),
    assert(cardstatus(Card,Player,0)),
    (
        not(PlayerNum = Player) ->
            retractall(cardstatus(Card,PlayerNum,0)),
            retractall(cardstatus(Card,PlayerNum,-1)),
            assert(cardstatus(Card,PlayerNum,-1));
        true
    ),
    NewPlayerNum is PlayerNum - 1,
    input_card(Card,Player,NewPlayerNum).

% Record when a suggestion was made and no one showed any of them to the suggester
% Note that the suggester may have been bluffing
% Remove any duplicate identical rules if necessary
record_no_one_else_showed_card(_,_,0).
record_no_one_else_showed_card(Suggester,Card,PlayerNum) :-
    PlayerNum > 0,
    (
        not(PlayerNum = Suggester) ->
            retractall(cardstatus(Card,PlayerNum,0)),
            retractall(cardstatus(Card,PlayerNum,-1)),
            assert(cardstatus(Card,PlayerNum,-1));
        true
    ),
    NewPlayerNum is PlayerNum - 1,
    record_no_one_else_showed_card(Suggester,Card,NewPlayerNum).


% Determines if a card is valid character, weapon, or room
valid_card(Card) :- character(Card);weapon(Card);room(Card).

% Determines if two players are different
different_players(Player, OtherPlayer) :- player(Player),player(OtherPlayer),not(Player=OtherPlayer).

% Determines if a player is a valid player and not me
is_another_player(OtherPlayer) :- player(OtherPlayer),player_num(Player),not(Player = OtherPlayer).

% Whether or not you should accuse (deduced by elimination)
should_accuse :-
    count_solutions(
        (
            valid_card(Card),
            no_one_has(Card),
            assert(solution(Card))
        ),
        Count
    ),
    Count = 3.

no_one_has(Card) :-
    num_players(NumPlayers),
    count_solutions(cardstatus(Card,_,-1),Count),
    Count = NumPlayers.


% HELPER FUNCTIONS

% Increments X, outputs Y = X+1
increment(X,Y):- Y is X+1.

% Counts the number of solutions to a given predicate
count_solutions(P,Count) :- findall(1,P,L),length(L,Count).

% Clears the screen - borrowed from http://osdir.com/ml/lang.swi-prolog.general/2006-12/msg00079.html
clear :- format('~c~s~c~s',[0x1b,"[H",0x1b,"[2J"]).

% Logical exclusive-or
xor(P1, P2) :- (P1,not(P2);P2,not(P1)),!.