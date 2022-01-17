:- consult('board.pl').
:- consult('game.pl').

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
    choose_board_size(BoardSize),
    initial_state(BoardSize,GameState),
    Player = 1,
    game_cycle(GameState-Player).
    
/*Human vs PC*/
menu_option(2):-
    choose_board_size(BoardSize),
    initial_state(BoardSize,GameState),
    Player = 'Human',
    game_cycle(GameState-Player).

/*PC vs Human*/
menu_option(3):-
    choose_board_size(BoardSize),
    initial_state(BoardSize,GameState),
    Player = 'PC',
    game_cycle(GameState-Player).

/* PC vs PC */
menu_option(4):-
    choose_board_size(BoardSize),
    initial_state(BoardSize,GameState),
    Player = 'PC1',
    game_cycle(GameState-Player).

/* Exit option */
menu_option(5):-
    write('Exiting game...\n').

choose_board_size(BoardSize):-
    write('-----------------------------------\n'),
    write('-----                         -----\n'),
    write('-----      Choose the size    -----\n'),
    write('-----        of the board     -----\n'),
    write('-----                         -----\n'),
    write('-----------------------------------\n'),
    read(BoardSize).