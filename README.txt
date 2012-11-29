**********************************************************
		CPSC PROJECT 2 - CLUE
**********************************************************

Byron Henze #66809088 l6f7	
Ian Lavery #36506095 t2i7

**********************************************************

Capabilities and Limitations:

Our program is more than just a electronic notepad; our Clue assistant helps you in numerous achieve your goal of making the quickest accurate accusation possible! 

Through our easy to use menu the program will allow you to: 

	- tell it if you've changed rooms 
	- record suggestions you've made and their results 
	- record a suggestion made by another player and record the results 
	- view the list of current knowledge, which grows as suggestion are made
	- stop the program

It will also tell you when an accusation can be made and make helpful suggestions based on what information is needed most to solve the crime.

The programs limitations are only in the fact that it doesn't fully account for bluffing (but doesn't ignore it) and it doesn't build models of opponent's knowledge to help you throw them off.


How to use it:

It's easy! The program does everything through a series of prompts. At the beginning of the game you type "clue." to start the program and then follow a series of prompts to initialize the game. From then on you use a home menu to lead the program through the events of the game. Each event has a series of prompts where you let the program know what room you're in, suggestions that are made, the results of suggestions, etc. Anytime between events you can reference your knowledge base, which will allow you to weigh your options, but you may not have to, because betwen each event the clue assistent will automatically give you the best possible suggestion to make based on what it knows or tell you to make an accusation if it's sure it knows who did it.


How it works:

The foundation in it's functionality lies in our program's use of a dynamic directive called "cardstatus". Each player will have a cardstatus for every card that has a piece of knowledge uncovered about it during play. The knowledge is represented in the knowledge base as:
	-1 - the card is NOT in the player's hand for sure
	0 -  the card IS in the player's hand for sure
	>0 - a positive integer detailing the amount of times the card has been potentially shown by the player   


It works by not only recording what we know about the each players' cards from what has been shown to us or not shown at all, but by also inferring and deducing further through the information that is placed in our program's knowledge base. Everytime a suggestion is made the results are recorded in our programs knowledge base and then the base itself is checked for any general deductions that can be made (eg. we now know the location of one of the cards, so we can be sure it is no one else's hand). In addition, an opponent is shown a card, the program records the suggestion and cross-references it with the knowledge base to see if anything can be inferred (eg. we know two of the suggested cards are held by players other than the player showing a card, so the player showing the card must be showing the third card). To make this easier the program keeps track of all the possiblities of cards that we can infer may be in their hand, through knowledge gained when the player is forced to show a card (in the form of a positive int tracking how many times it has been potentially shown by the player). These possibilities are used in the programs next best suggestion function and are used to narrow down possibilities as the game goes on.

    