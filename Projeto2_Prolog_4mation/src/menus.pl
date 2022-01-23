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
    write('-----------------------------------\n').

read_menu_option:-
    read(Option),
    menu_option(Option).

/*Human vs Human*/
menu_option(1):-
    choose_board_size(BoardSize),
    initial_state(BoardSize,Board),
    Player = 1,
    game_cycle(Board-Player-'first_move'-'first_move',0).
    
/*Human vs PC*/
menu_option(2):-
    choose_pc_level(Level),
    choose_board_size(BoardSize),
    initial_state(BoardSize,Board),
    Player = 'Human',
    game_cycle(Board-Player-'first_move'-'first_move',Level).

/*PC vs Human*/
menu_option(3):-
    choose_pc_level(Level),
    choose_board_size(BoardSize),
    initial_state(BoardSize,Board),
    Player = 'PC',
    game_cycle(Board-Player-'first_move'-'first_move',Level).


/* PC vs PC */
menu_option(4):-
    choose_pc_level(Level),
    choose_board_size(BoardSize),
    initial_state(BoardSize,Board),
    Player = 'PC1',
    game_cycle(Board-Player-'first_move'-'first_move',Level).

/* Exit option */
menu_option(5):-
    write('Exiting game...\n'),
    read(_),
    halt.

/* Invalid option */
menu_option(_):-
    cls,
    print_invalid_input,
    display_main_menu.

read_board_size_option(BoardSize):-
    read(Option),
    number(Option),
    BoardSize is Option.

read_board_size_option(BoardSize):-
    cls,
    print_invalid_input,
    choose_board_size(BoardSize).

choose_board_size(BoardSize):-
    write('-----------------------------------\n'),
    write('-----                         -----\n'),
    write('-----      Choose the size    -----\n'),
    write('-----        of the board     -----\n'),
    write('-----                         -----\n'),
    write('-----------------------------------\n'),
    read_board_size_option(BoardSize).

read_level_option(Level):-
    read(Option),
    number(Option),
    Option == 1,
    Level is Option.
    
read_level_option(Level):-
    cls,
    display_not_implemented,
    choose_pc_level(Level).

choose_pc_level(Level):-
    write('-----------------------------------\n'),
    write('-----                         -----\n'),
    write('-----      Choose the Level   -----\n'),
    write('-----      of the PC player   -----\n'),
    write('-----                         -----\n'),
    write('-----          Level 1:       -----\n'),
    write('-----        random move      -----\n'),
    write('-----                         -----\n'),
    write('-----          Level 2:       -----\n'),
    write('-----         best move       -----\n'),
    write('-----                         -----\n'),
    write('-----------------------------------\n'),
    read_level_option(Level).

    
