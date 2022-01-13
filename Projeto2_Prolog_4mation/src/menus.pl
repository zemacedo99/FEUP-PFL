:- consult('board.pl').

display_main_menu:-
    write('-----------------------------------\n'),
    write('-----                         -----\n'),
    write('-----         4Mation         -----\n'),
    write('-----                         -----\n'),
    write('-----     1: Human / Human    -----\n'),
    write('-----     2: Human / PC       -----\n'),
    write('-----     3: PC    / Human    -----\n'),
    write('-----     4: PC    / PC       -----\n'),
    write('-----     5: Exit             -----\n'),
    write('-----                         -----\n'),
    write('-----------------------------------\n'),
    read_menu_option.

display_not_implemented:-
    write('-----------------------------------\n'),
    write('-----                         -----\n'),
    write('-----         4Mation         -----\n'),
    write('-----                         -----\n'),
    write('-----     This option is not  -----\n'),
    write('-----     available yet.      -----\n'),
    write('-----                         -----\n'),
    write('-----     0: Go back          -----\n'),
    write('-----     5: Exit             -----\n'),
    write('-----                         -----\n'),
    write('-----------------------------------\n'),
    read_menu_option.

read_menu_option:-
    read(Option),
    menu_option(Option).

/*Go back*/
menu_option(0):-
    display_main_menu.

/*Human vs Human*/
menu_option(1):-
    % initial_board(Board),
    initial_state(7,Board),
    display_game(Board).
    
/*Human vs PC*/
menu_option(2):-
    display_not_implemented.

/*PC vs Human*/
menu_option(3):-
    display_not_implemented.

/* PC vs. PC option*/
menu_option(4):-
    display_not_implemented.

/* Exit option */
menu_option(5):-
    write('Exiting game...\n').