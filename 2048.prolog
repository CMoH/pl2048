:- set_prolog_stack(global, limit(8 * 10**9)).
:- set_prolog_stack(local,  limit(8 * 10**9)).
:- set_prolog_stack(trail,  limit(8 * 10**9)).

solve :-
    initial_board(Board),
    %% breadth_first_search([[Board]],Solution),
    depth_first_search([], Board, Solution),
    print_solution(Solution).

print_solution([]).
print_solution([Board | Rest]) :-
    print_board(Board), 
    write('Enter stop to stop:'), read(Something),
    Something \= 'stop',
    print_solution(Rest).


depths(Depths, PathCount) :-
    findall(N, depth_test(N), L),
    length(L, PathCount),
    sort(L, Depths),
    [MinDepth | _Rest] = Depths, last(Depths, MaxDepth),
    writef('Depth=%w..%w\nCount=%w\n', [MinDepth, MaxDepth], PathCount).

depth_test(N) :-
    initial_board(Board),
    nth_successor(Board, 0, N).

nth_successor(Board, N, N) :- win(Board),  !.
nth_successor(Board, N, Res) :- 
    N1 is N+1, N1 < 4,
    successor(Board, Board1),
    nth_successor(Board1, N1, Res).

initial_board([0,0,0,0,
               0,0,0,0,
               0,0,0,0,
               0,0,0,0]).

test_board([0, 2, 4, 8,
            0, 2, 4, 8,
            0, 2, 4, 8,
            0, 2, 4, 8]).

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


%% bfs while caching moves in the database
:- dynamic cached_move/3.
:- dynamic cached_score/2.
:- dynamic parent_move/2.

%% cache utility preds

cache_stats :-
    findall(Board, cached_move(Board, _, _), Boards),
    findall(Score, cached_score(_, Score), Scores),
    findall(Board, parent_move(Board, _), Parents),
    length(Boards, LBoards),
    length(Scores, LScores),
    length(Parents, LParents),
    format('Boards: ~d~nScores: ~d~nParents:~d~n',
           [LBoards, LScores, LParents]).
           

clean_cache :-
    retractall(cached_move(_, _, _)),
    retractall(cached_score(_, _)),
    retractall(parent_move(_, _)).


%% cache hits
caching_bfs(_Board, 0) :- !.    % stop after reaching required depth
caching_bfs(Board, _MaxDepth) :-
    win(Board), !.              % stop if we got to 2048

caching_bfs(Board, MaxDepth) :- 
    MaxDepth > 0,
    cached_move(Board, _, _), !,
    MaxDepth1 is MaxDepth - 1,
    forall(cached_move(Board, _Direction, NextBoards), 
           ( forall(member(NextBoard, NextBoards), 
                    caching_bfs(NextBoard, MaxDepth1)) )).

%% cache misses -> generate new moves
caching_bfs(Board, MaxDepth) :-
    MaxDepth > 0,
    forall(gen_moves(Board, Direction, MovedBoard),
           examine_move(Board, Direction, MovedBoard, MaxDepth)).

examine_move(Board, Direction, MovedBoard, MaxDepth) :-
    findall(NextBoard, gen_new_tiles(MovedBoard, NextBoard), NextBoards),
    add_move(Board, Direction, NextBoards),
    MaxDepth1 is MaxDepth - 1,
    forall(member(NextBoard, NextBoards), caching_bfs(NextBoard, MaxDepth1)).

%% cache a move and update score of parent moves
add_move(Board, Direction, NextBoards) :-
    \+ cached_move(Board, Direction, _), !,
    findall(BoardScore, 
            board_score(Board, BoardScore), 
            Scores),
    sum_list(Scores, Score),

    sort(NextBoards, UniqueNextBoards),

    assert(cached_move(Board, Direction, UniqueNextBoards)),
    assert(cached_score(Board, Score)),
    forall(member(NextBoard, NextBoards), 
           ignore(( \+parent_move(NextBoard, _),
                    assert(parent_move(NextBoard, Board)) ))
          ),

    update_parent_scores(Board, Score).

add_move(Board, Direction, _) :- writef('re-adding %w %w\n', [Board, Direction]).


%% removes a move and related moves rom the cache
%% TODO
del_move(Board, Excluding) :-
    \+ member(Board, Excluding), 
    cached_move(Board, _, NextBoards),
    parent_move(Board, ParentBoard), !,
    retractall(cached_move(Board, _, _)),
    retractall(cached_score(Board, _)),
    retractall(parent_move(Board, _)),
    foreach(member(Move, [ParentBoard | NextBoards]), 
            del_move(Move, Excluding)).
del_move(_,_).



%% select best move from a given position
select_move(Board, Move, LesserMoves) :-
    findall(move(Direction, Score),
            ( cached_move(Board, Direction, _NextBoards),
              cached_score(Board, Score) ),
            Moves),
    best_move(Moves, move(left, 0), Move),
    select(Move, Moves, LesserMoves).

best_move([], Move, Move).
best_move([Head | Rest], CurrentMove, BestMove) :- !,
    move(_, Score) = Head,
    move(_, CurrentScore) = CurrentMove,
    ( Score > CurrentScore 
      ->  best_move(Rest, Head, BestMove) 
      ;   best_move(Rest, CurrentMove, BestMove) ).


%% the strategy for scoring the board
board_score(Board, Score) :-
    findall(S, board_score_rule(Board, S), L),
    sum_list(L, Score).

board_score_rule(Board,  2) :- win(Board).
board_score_rule(Board, -4) :- lose(Board).
board_score_rule(Board, FT) :- count(0, Board, FT).


update_parent_scores(Board, Score) :-
    parent_move(Board, ParentBoard),
    cached_score(ParentBoard, ParentScore), !,
    NewParentScore is ParentScore + Score,
    retract(cached_score(ParentBoard, ParentScore)),
    assert(cached_score(ParentBoard, NewParentScore)),
    update_parent_scores(ParentBoard, Score).
update_parent_scores(_,_).



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
    move_board(Board, Direction, MovedBoard).

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
