:- module(httpserver, [ ]).

:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_session)).
:- use_module(library(http/json)).
:- use_module(library(http/thread_httpd)).

:- use_module('2048.prolog').

:- op(200, fy, '@').

:- debug(httpserver).


/** <module> 2048 AI HTTP Server

Serves an 2048 playing AI through HTTP, at port 2048.

@author    Ciprian Ciubotariu

*/

%%

%%:- http_handler(root(swish), http_reply_file('www/app.html', []), [prefix]).
%%:- http_handler(root(debug), http_reply_file('www/debug.html', []), [prefix]).


%%    server(?Port) is det.
%
%    Start the web-server on Port.

server(Port) :-
    http_server(http_dispatch,
            [ port(Port),
              workers(16)
            ]),
    format('You can access the server at http://localhost:~w/~n', [Port]).

%    Declare HTTP locations we serve and how.

:- http_handler(root(.), welcome_page, []).

%%    welcome_page(?Request) is det.
%
%    Generate a welcome page

%% TODO: add some server info

welcome_page(_Request) :-
    reply_html_page(title('2048 AI'),
                    h1(['Welcome!'])).

%%%%%%%%%%%  HTTP session handling  %%%%%%%%%%%

:- http_set_session_options([timeout(600)]).

:- listen(http_session(begin(SessionId, _Peer)), begin_session(SessionId)).
:- listen(http_session(end(SessionId, _Peer)), end_session(SessionId)).
    
begin_session(SessionId) :-
    debug(httpserver, 'Begin session: ~q', [SessionId]).

end_session(SessionId) :-
    debug(httpserver, 'End session: ~q', [SessionId]),
    http_session_data(thread(ThreadId)),
    catch(thread_signal(ThreadId, abort_game), _, true).



%%%%%%%%%%%     HTTP REST handlers    %%%%%%%%%%%

%    Declare HTTP locations we serve and how.

:- http_handler(root(start), start, []).
:- http_handler(root(end), end, []).
:- http_handler(root(move), move, []).
:- http_handler(root(board), board, []).

%%   start(+Request) is det.
%
%    HTTP handler that starts a game

start(Request) :-
    http_read_json(Request, JSONIn),
    debug(httpserver, 'start called: ~w', [JSONIn]),
    JSONIn = json([board=Board]),
    thread_create(new_game(Board), ThreadId, 
                  [detached(true), 
                   global(2000000),
                   local( 2000000),
                   trail( 2000000)]),
    debug(httpserver, 'thread id: ~w', [ThreadId]),
    http_session_assert(thread(ThreadId)),
    reply_json(json([ok= true]), [width(0)]).


end(_Request) :-
    debug(httpserver, 'end called', []),
    reply_json(json([ok= @true]), [width(0)]),
    http_session_id(SessionId),
    http_close_session(SessionId).


move(_Request) :-
    debug(httpserver, 'move called', []),
    http_session_data(thread(ThreadId)),
    thread_self(Self),
    thread_send_message(ThreadId, needNextMove(Self)),
    thread_get_message(nextMove(Move)),
    reply_json(json([move=Move]), [width(0)]).


board(Request) :-
    http_read_json(Request, JSONIn),
    debug(httpserver, 'board called: ~w', [JSONIn]),
    JSONIn = json(Objects),
    member(board=Board, Objects),
    member(lastMove=LastMove, Objects),
    http_session_data(thread(ThreadId)),
    thread_send_message(ThreadId, board(LastMove, Board)),
    reply_json(json([ok= @true]), [width(0)]).
