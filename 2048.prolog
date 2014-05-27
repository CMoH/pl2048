:- module('2048', [new_game/1, abort_game/0]).

%% :- set_prolog_stack(global, limit(2 * 10**9)).
%% :- set_prolog_stack(local,  limit(2 * 10**9)).
%% :- set_prolog_stack(trail,  limit(2 * 10**9)).

:- debug('2048').

%% for thread management

new_game(Board) :-
    thread_loop([ board(Board, []) ]).

abort_game :-
    throw(aborted).

%% paths representation:
%% Path = [  board(Board, [
%%                     move(left, Score, Path), 
%%                     move(right, ...),
%%                     move(down, ...),
%%                     move(up, ...) ] ), 
%%           board(Board, [
%%                     move(left, Score, Path), 
%%                     move(right, ...),
%%                     move(...) ] ), 
%%           ...
%%        ]

thread_loop(Paths) :-
    update_paths(Paths, NewPaths),
    handle_messages(NewPaths, SelectedPaths),
    thread_loop(SelectedPaths).

update_paths(Paths, Paths) :-
    depth(Paths, Depth),
    Depth > 3, !.
update_paths(Paths, NewPaths) :-
    iddfs(Paths, NewPaths).

handle_messages(Paths, NewPaths) :-
    thread_peek_message(_), !,
    thread_get_message(Message),
    handle_message(Message, Paths, Paths1),
    handle_messages(Paths1, NewPaths).
handle_messages(Paths, Paths).

handle_message(print_stats, Paths, Paths) :-
    print_stats(Paths).
handle_message(needNextMove(ThreadId), Paths, Paths) :-
    find_best_move(Paths, Direction),
    print_best_move(Paths, Direction),
    thread_send_message(ThreadId, nextMove(Direction)).
handle_message(board(LastMove, Board), Paths, NewPaths) :-
    select_board(LastMove, Board, Paths, NewPaths).
handle_message(abort, _, _) :-
    fail.
handle_messages(Msg, Paths, Paths) :-
    thread_self(Self),
    debug('2048', 'thread ~w: invalid message received: ~w', [Self, Msg]).


iddfs(Paths, NewPaths) :-
    findall(NewState, ( member(State, Paths), dfs1(State, NewState) ), NewPaths).

dfs1(board(Board, []), board(Board, NewMoves)) :- 
    !,                          % explore one level down
    findall(NewMove, dfs1_gen_moves(Board, NewMove), NewMoves).
dfs1(board(Board, Moves), board(Board, NewMoves)) :-
    findall(NewMove, 
            (   member(move(Direction, _Score, Path), Moves), 
                iddfs(Path, NewPath),
                sum_score(NewPath, NewScore),
                NewMove = move(Direction, NewScore, NewPath)  ),
            NewMoves).

dfs1_gen_moves(Board, Move) :-
    gen_moves(Board, Direction, MovedBoard),
    findall(board(NewBoard, []), 
            gen_new_tiles(MovedBoard, NewBoard),
            NewBoards),
    sum_score(NewBoards, TotalScore),
    Move = move(Direction, TotalScore, NewBoards).

sum_score(Boards, TotalScore) :-
    findall(Score, ( 
                member(board(Board, _), Boards), 
                board_score(Board, Score) ),
            Scores),
    sum_list(Scores, TotalScore).


find_best_move(Paths, Direction) :-
    Paths = [board(_, Moves)],
    Moves = [H|T], !,
    find_best_move(T, H, Direction).
find_best_move(Paths, _Direction) :-
    debug('2048', 'find_best_move failed', []),
    print_moves(Paths),
    fail.

find_best_move([], move(Direction, _, _), Direction) :- !.
find_best_move([H|T], Current, Direction) :-
    Current = move(_, CurrentScore, _),
    H = move(_, OtherScore, _), !,
    ( CurrentScore > OtherScore 
      -> find_best_move(T, Current, Direction) 
      ;  find_best_move(T, H, Direction) ).
      
%% executes a move by selecting the board chosen randomly by the game after LastMove
select_board(LastMove, Board, Paths, NewGameTree) :-
    Paths = [board(_, Moves)],
    member(move(LastMove, _, NextBoards), Moves),
    member(board(Board, NextMoves), NextBoards), !,
    NewGameTree = [board(Board, NextMoves)].
select_board(LastMove, Board, Paths, NewGameTree) :-
    Paths = [board(LastBoard, _) | _], % make sure we succeed in reporting
    print_message(warning, select_board_failure(LastBoard, LastMove, Board)),
    iddfs([board(Board, [])], NewGameTree).


:- multifile prolog:message/1.
prolog:message(select_board_failure(LastBoard, Move, Board)) -->
    {},
    ['select_board failure: cannot move board(~w) ~w with result board(~w)'-[LastBoard,Move,Board]].
                    

print_moves(Paths) :-
    length(Paths, TopBoards),
    findall(board(Board, AvailableMoves), 
            (  member(board(Board, Moves), Paths),
               findall(move(Direction, Score), 
                       member(move(Direction, Score, _), Moves),
                       AvailableMoves)  ),
            Boards),
    debug('2048', 'Available moves (~w top boards): ~w', [TopBoards, Boards]).
    
print_best_move(Paths, BestMove) :-
    print_moves(Paths),
    debug('2048', 'Best move is: ~w', [BestMove]).



print_stats(Paths) :-
    length(Paths, Len),
    depth(Paths, Depth),
    state_count(Paths, Count),
    debug('2048', 'Top-level boards: ~w~n', [Len]),
    debug('2048', 'Depth: ~w~n', [Depth]),
    debug('2048', 'States: ~w~n', [Count]).

depth([], 0).
depth(Paths, DepthOut) :-
    findall(Depth, (
                member(board(_, Moves), Paths),
                ( Moves = [] -> Depth = 0 
                  ; ( member(move(_,_,Path), Moves),
                      depth(Path, Depth) )
                ) ),
            Depths),
    max_list(Depths, Depth),
    DepthOut is Depth + 1.


state_count([], 0).
state_count(Paths, CountOut) :-
    findall(Count, (
                member(board(_, Moves), Paths),
                ( Moves = [] -> Count = 0 
                  ; ( member(move(_,_,Path), Moves),
                      state_count(Path, Count) )
            ) ),
            Counts),
    sum_list(Counts, Count),
    length(Paths, BoardCount),
    CountOut is Count + BoardCount.
                       
    

%% development helpers

%% solve(Solution) :-
%%     empty_board(Board),
%% %%    breadth_first_search([[Board]],Solution),
%%     depth_first_search([], Board, Solution).


test_board([0, 2, 4, 8,
            0, 2, 4, 8,
            0, 2, 4, 8,
            0, 2, 4, 8]).

test_board1([0,0,2,0,0,2,0,0,0,0,0,0,0,0,0,0]).

%% board printing predicates
print_board(B) :-
    nl, 
    write('   '), print_header(0), nl,
    write('  '), writef('%r', ['-',4*5+3]), nl,
    print_board(B, 0, 0),
    write('  '), writef('%r', ['-',4*5+3]), nl,
    nl.

print_header(4) :- !.
print_header(N) :- writef('%5r', [N]), N1 is N + 1, print_header(N1).

print_board([H|T], Row, 0) :- !, 
                              writef('%2l|', [Row]),
                              print_tile(H),
                              print_board(T, Row, 1).
print_board([H|T], Row, 4) :- !, 
                              write(' |'), nl, 
                              Row1 is Row + 1,
                              print_board([H|T], Row1, 0).
print_board([H|T], Row, N) :- !, 
                              print_tile(H), 
                              N1 is N + 1, 
                              print_board(T, Row, N1).
print_board([], _, _) :- write(' |'), nl.

print_tile(0) :- !, writef('     ').
print_tile(N) :- writef('%5r', [N]).


%% search algorithms

%% bfs
breadth_first_search([[Node|Path]|_],[Node|Path]) :-
    win(Node), !.       % stop searching this path after 2048

breadth_first_search([[Node|Path]|RestPaths],Solution) :-
    findall([NewNode,Node|Path],
            successor(Node,NewNode),
            NewPaths),
    append(RestPaths,NewPaths,CurrentPaths),
    breadth_first_search(CurrentPaths,Solution).



%% dfs
depth_first_search(CurrentPath,CurrentState,Solution) :-
    win(CurrentState), !, % stop searching this path after 2048
    Solution=[CurrentState|CurrentPath].

depth_first_search(CurrentPath,CurrentState,Solution) :-
    successor(CurrentState,NewState),
    not(member(NewState,CurrentPath)),
    depth_first_search([CurrentState|CurrentPath],NewState,Solution).


%% depth-limited dfs
depth_first_search(CurrentPath,CurrentState,Solution, _MaxDepth) :-
    win(CurrentState), !, % stop searching this path after 2048
    Solution=[CurrentState|CurrentPath].

depth_first_search(CurrentPath,CurrentState,Solution, MaxDepth) :-
    length(CurrentPath, Depth), Depth =< MaxDepth, !,
    successor(CurrentState,NewState),
    not(member(NewState,CurrentPath)),
    depth_first_search([CurrentState|CurrentPath],NewState,Solution, MaxDepth).


%% depth-limited bfs
breadth_first_search([[Node|Path]|_],[Node|Path], _MaxDepth) :-
    win(Node), !.       % stop searching this path after 2048

breadth_first_search([Path | RestPaths], Solution, MaxDepth) :-
    length(Path, Depth), Depth > MaxDepth, !,
    append(RestPaths, [Path], CurrentPaths),
    breadth_first_search(CurrentPaths, Solution, MaxDepth).

breadth_first_search([[Node|Path]|RestPaths],Solution, MaxDepth) :-
    length([Node|Path], Depth), Depth =< MaxDepth,
    findall([NewNode,Node|Path],
            successor(Node,NewNode),
            NewPaths),
    append(RestPaths,NewPaths,CurrentPaths),
    breadth_first_search(CurrentPaths,Solution, MaxDepth).


%% the strategy for scoring the board
board_score(Board, Score) :-
    findall(S, board_score_rule(Board, S), L),
    sum_list(L, Score).

board_score_rule(Board,  10) :- win(Board).
board_score_rule(Board, -10) :- lose(Board).
board_score_rule(Board,  FT) :- count(0, Board, FT).


%% game definition

win(Board) :- member(2048, Board).
lose(Board) :- count(0, Board, 0), \+ member(2048, Board).

count(_, [], 0).
count(X, [X|T], Count) :- !,
    count(X, T, Count1),
    Count is Count1 + 1.
count(X, [_|T], Count) :-
    count(X, T, Count).
    

successor(Board, NewBoard) :-
    gen_moves(Board, _, MovedBoard),
    gen_new_tiles(MovedBoard, NewBoard).

%% player moves
gen_moves(Board, Direction, MovedBoard) :- 
    direction(Direction), 
    move_board(Board, Direction, MovedBoard),
    Board \= MovedBoard. % moves that do not change the board are not allowed

direction(left).
direction(right).
direction(up).
direction(down).

move_board(Board, Direction, NewBoard) :-
    % for I = 0 .. 3 * 16: % pass the board 3 times in order to achieve stable position of each piece
    %   Pos = I mod 16
    %   move_piece(Direction, Pos, Board, NewBoard)
    move_board_repeat(0, 3 * 16, Board, Board1, Direction),
    unmark_board(Board1, NewBoard).

move_board_repeat(N, Max, Board, Board, _) :- N >= Max, !.
move_board_repeat(N, Max, Board, NewBoard, Direction) :- !,
    Pos is N mod 16,
    N1 is N+1,
    move_piece(Direction, Pos, Board, Board1),
    move_board_repeat(N1, Max, Board1, NewBoard, Direction).

move_piece(Direction, SourcePos, Board, NewBoard) :-
    checked_move(Direction, SourcePos, TargetPos), !,
    nth0(TargetPos, Board, Target),
    nth0(SourcePos, Board, Source),
    combine(Target, Source, NewTarget, NewSource),
    replace_nth0(Board,  TargetPos, Target, NewTarget, Board1),
    replace_nth0(Board1, SourcePos, Source, NewSource, NewBoard).
move_piece(_, _, Board, Board).

%% move piece, checking that it does not move past a margin of the board
checked_move(left,  Pos, NewPos) :- NewPos is Pos - 1, NewPos >= (Pos // 4) * 4.
checked_move(right, Pos, NewPos) :- NewPos is Pos + 1, NewPos  < (Pos // 4) * 4 + 4.
checked_move(up,    Pos, NewPos) :- NewPos is Pos - 4, NewPos >= 0.
checked_move(down,  Pos, NewPos) :- NewPos is Pos + 4, NewPos  < 16.


combine(0, Source, Source, 0) :- !.
combine(Target, Target, NewTarget, 0) :- number(Target), !,
    NewTargetVal is 2 * Target, 
    NewTarget = combined(NewTargetVal).
combine(Target, Source, Target, Source).


unmark_board([], []).
unmark_board([H|T], [H|RT]) :- 
    number(H), !, 
    unmark_board(T, RT).
unmark_board([combined(H)|T], [H|RT]) :- 
    unmark_board(T, RT).


%% generate random new tiles
gen_new_tiles(Board, NewBoard) :- 
    place_any_at([2,4],
                 [ 0, 1, 2, 3,
                   4, 5, 6, 7,
                   8, 9,10,11,
                  12,13,14,15 ],
                 Board, 
                 NewBoard).

place_any_at(Tiles, Positions, Board, NewBoard) :-
    member(Pos, Positions),
    member(Tile, Tiles),
    replace_nth0(Board, Pos, 0, Tile, NewBoard).

replace_nth0(List, Index, OldElem, NewElem, NewList) :-
    nth0(Index,    List, OldElem, List1),
    nth0(Index, NewList, NewElem, List1).
