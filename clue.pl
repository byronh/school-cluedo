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
    write_ln('Enter a card you started with. If you are finished type "done."'),
    read(Card),
    (
        Card = done -> nl, record_event;
        valid_card(Card) -> num_players(NumPlayers),assert(in_hand(Card)),input_card(Card,Player,NumPlayers),input_starting_cards(Player);
        write_ln('Invalid card, please try again.'),input_starting_cards(Player)
    ).
	
	
% MAIN LOOP

% Main menu - record knowledge we have acquired at any point in the game
record_event :-
    % Check first if there's enough information to make an accusation or suggestion
    (
        should_accuse ->
            write_ln('You should make an accusation!'),nl,
            character(C),solution(C),write_ln(C),
            weapon(W),solution(W),write_ln(W),
            room(R),solution(R),write_ln(R),nl,nl;
        best_suggest(Card) ->
            write_ln('Next turn, you should make a suggestion containing the following:'),
            write_ln(Card),
            (
                character(Card) ->
                    ((get_known_weapon(Weapon),write_ln(Weapon));(get_known_room(Room),write_ln(Room));true);
                weapon(Card) ->
                    ((get_known_character(Character),write_ln(Character));(get_known_room(Room),write_ln(Room));true);
                room(Card) ->
                    ((get_known_character(Character),write_ln(Character));(get_known_weapon(Weapon),write_ln(Weapon));true);
                true
            ),
            nl,nl;
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

% Record a suggestion by me
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
        check_base(NumPlayers), nl, record_event
    ).

record_shown_card :-
    write_ln('What card was shown to you?'),
    read(Card),
    write_ln('What player showed you the card?'),
    read(OtherPlayer),
    (
        valid_card(Card),is_another_player(OtherPlayer) ->
            num_players(NumPlayers),input_card(Card,OtherPlayer,NumPlayers),check_base(NumPlayers),nl,record_event;
        write_ln('Invalid card or player, please try again.'),nl,record_shown_card
    ).

% checks the knowledge base and deduces	
check_base(0).	
check_base(NumPlayers) :- player_num(Player), valid_card(Card),
		NumPlayers >0,
		(
		cardstatus(NumPlayers,Card,X),X>0->checks_for_0s(Player, Card), check_for_alone(Player, Card);
		true
		),
		NewNumPlayers is NumPlayers-1, check_base(NewNumPlayers).

%checks if someone else has the card
%assures that player is marked as not having that card if we know that another player has it for sure
check_for_0s(Player, Card) :-
	(
		cardstatus(Card,_,0)->retractall(cardstatus(Card,Player,_)),assert(cardstatus(Card,Player,-1));
		true
	).

%checks if player has only one positive integer cardstatus
%if so it can be deduced that they must have this card, marks it as 0
check_for_alone(Player, Card) :- retract(cardstatus(Card,Player,X)), X>0,
	(	
		not(cardstatus(_,Player,Y)),Y>0 -> asserta(cardstatus(Card,Player,0));
		true
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
           check_base(NumPlayers), nl, record_event;
          write_ln('Invalid input, please reenter suggestion'), nl, record_suggestion_other

        ).

% If the suggestion of an opponent causes another player to show them a card
check_shown(Player, Suspect, Weapon, Room) :-
        write_ln('Which player showed their card?'),
        read(PlayerShowing),
        (
        is_another_player(PlayerShowing) -> record_shown_card_other(PlayerShowing,[Suspect,Weapon,Room]), num_players(NumPlayers), 
				check_suggestion_with_base(PlayerShowing, Suspect,Weapon,Room, NumPlayers), check_base(NumPlayers), nl, record_event;
		player_num(PlayerShowing)-> nl,record_event;
                write_ln('Invalid player entered, please try again.'), nl, check_shown(Player, Suspect, Weapon, Room)
        ).

% Brings suggestion to knowledge base to see if anything can be deduced		
check_suggestion_with_base(Player, Suspect, Weapon, Room,NumPlayers) :- 
		check_suggestion_neg1s(Player, Suspect, Weapon, Room, NumPlayers), 
		check_suggestion_0s(Player, Suspect, Weapon, Room, NumPlayers).

% Checks if the player showing cards is known to not have two of the three suggested cards
% sets third card to 0 if this is the case, marking down that we know this player has that card		
check_suggestion_neg1s(Player, Suspect, Weapon, Room, NumPlayers) :- 
			( 				
				cardstatus(Suspect, Player, -1), cardstatus(Weapon, Player, -1) -> retractall(cardstatus(Room, Player,_)),input_card(Room, Player,NumPlayers);
				cardstatus(Suspect, Player, -1), cardstatus(Room, Player, -1) -> retractall(cardstatus(Weapon, Player,_)),input_card(Weapon, Player,NumPlayers);
				cardstatus(Room, Player, -1), cardstatus(Weapon, Player, -1) -> retractall(cardstatus(Suspect, Player,_)),input_card(Suspect, Player, NumPlayers);
				true
			).

% Checks if any other players are known to be in posession of two of the three suggested cards
% if so, marks the third card as known to be in the players hand who is showing			
check_suggestion_0s(Player, Suspect, Weapon, Room, NumPlayers) :- 
		(	
			cardstatus(Suspect,X , 0), cardstatus(Weapon, Y, 0), not(X=Player),not(Y=Player) -> retractall(cardstatus(Room, Player,__)), input_card(Room, Player,NumPlayers);
			cardstatus(Suspect,X , 0), cardstatus(Room, Y, 0), not(X=Player), not(Y=Player) -> retractall(cardstatus(Weapon, Player,_)), input_card(Weapon, Player,NumPlayers);
			cardstatus(Room,X , 0), cardstatus(Weapon, Y, 0), not(X=Player), not(Y=Player) -> retractall(cardstatus(Suspect, Player,_)), input_card(Suspect, Player,NumPlayers);
			true
		).
			
				
% Records the results of another player showing an opponent a card
record_shown_card_other(_,[]).
record_shown_card_other(Player, [Card|T]) :- inc_cards_shown(Player),
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
    retractall(cardstatus(Card,Player,_)),
    asserta(cardstatus(Card,Player,0)),
    (
        not(PlayerNum = Player) ->
            retractall(cardstatus(Card,PlayerNum,_)),
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
            retractall(cardstatus(Card,PlayerNum,_)),
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
            player_num(PlayerNum),
            num_players(NumPlayers),
            no_one_has(Card,PlayerNum,NumPlayers),
            assert(solution(Card))
        ),
        Count
    ),
    Count = 3.

% Check if a card has been deduced (no player can possibly have it)
no_one_has(_,_,0).
no_one_has(Card,Player,PlayerNum) :-
    PlayerNum > 0,
    (
        PlayerNum = Player -> not(cardstatus(Card,PlayerNum,0));
        cardstatus(Card,PlayerNum,-1)
    ),
    NewPlayerNum is PlayerNum - 1,
    no_one_has(Card,Player,NewPlayerNum).

% We want to suggest about cards that we are close to figuring out about
% The card is one of two or three predicates about a player that has a positive integer
best_suggest_by_count(Card,Count) :-
    cardstatus(Card,Player,I),
    I > 0,
    count_solutions(cardstatus(_,Player,I),Count),
    player(Player).

% Find the best suggestion (we only want the first one in this strategy). The only possibilities for Count are 2 and 3:
%   2 because anything less would be deduced by check_base
%   3 because a suggestion contains 3 cards
best_suggest(Card) :-
    (best_suggest_by_count(Card,2);best_suggest_by_count(Card,3)),!.

% Gets a known card (status = 0)
% It doesn't really matter which one, but in order to not always present the same cards, we used asserta
% instead of assert in some places earlier in the program so the "top" predicate changes as suggestions are recorded
get_known_character(Card) :- character(Card),cardstatus(Card,_,0),!.
get_known_weapon(Card) :- weapon(Card),cardstatus(Card,_,0),!.
get_known_room(Card) :- room(Card),cardstatus(Card,_,0),!.


% HELPER FUNCTIONS

% Increments X, outputs Y = X+1
increment(X,Y):- Y is X+1.

% Counts the number of solutions to a given predicate
count_solutions(P,Count) :- findall(1,P,L),length(L,Count).

% Clears the screen - borrowed from http://osdir.com/ml/lang.swi-prolog.general/2006-12/msg00079.html
clear :- format('~c~s~c~s',[0x1b,"[H",0x1b,"[2J"]).

% Logical exclusive-or
xor(P1, P2) :- (P1,not(P2);P2,not(P1)),!.