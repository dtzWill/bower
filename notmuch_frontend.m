%-----------------------------------------------------------------------------%

:- module notmuch_frontend.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module list.
:- import_module string.

:- import_module callout.
:- import_module curs.
:- import_module curs.panel.
:- import_module data.
:- import_module index_view.
:- import_module pager.
:- import_module screen.

%-----------------------------------------------------------------------------%

:- pragma foreign_decl("C", local,
"
    #include <locale.h>
").

%-----------------------------------------------------------------------------%

main(!IO) :-
    setlocale(!IO),
    io.command_line_arguments(Args, !IO),
    ( Args = ["--show-thread", TId] ->
        run_notmuch(["show", "--format=json", "thread:" ++ TId],
            parse_messages_list, Messages : list(message), !IO),
        io.write(Messages, !IO),
        io.nl(!IO)
    ; Args = ["--pager", TId] ->
        run_notmuch(["show", "--format=json", "thread:" ++ TId],
            parse_messages_list, Messages : list(message), !IO),
        curs.session(interactive_pager(Messages), !IO)
    ; Args = ["--search" | Terms] ->
        run_notmuch(["search", "--format=json" | Terms],
            parse_threads_list, Threads, !IO),
        io.write(Threads, !IO)
    ; Args = ["--index" | Terms] ->
        run_notmuch(["search", "--format=json" | Terms],
            parse_threads_list, Threads, !IO),
        curs.session(interactive_index(Threads), !IO)
    ;
        io.write_string("command line error\n", !IO)
    ).

:- pred setlocale(io::di, io::uo) is det.

:- pragma foreign_proc("C",
    setlocale(IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    setlocale(LC_ALL, """");
    IO = IO0;
").

%-----------------------------------------------------------------------------%

:- pred interactive_index(list(thread)::in, io::di, io::uo) is det.

interactive_index(Threads, !IO) :-
    create_screen(Screen, !IO),
    setup_index_view(Threads, IndexInfo, !IO),
    interactive_loop(Screen, IndexInfo, !IO).

:- pred interactive_loop(screen::in, index_info::in, io::di, io::uo) is det.

interactive_loop(Screen, !.IndexInfo, !IO) :-
    draw_index_view(Screen, !.IndexInfo, !IO),
    draw_bar(Screen, !IO),
    panel.update_panels(!IO),
    get_char(Char, !IO),
    ( Char = 'q' ->
        true
    ;
        index_view_input(Screen, Char, !IndexInfo),
        interactive_loop(Screen, !.IndexInfo, !IO)
    ).

%-----------------------------------------------------------------------------%

:- pred interactive_pager(list(message)::in, io::di, io::uo) is det.

interactive_pager(Messages, !IO) :-
    create_screen(Screen, !IO),
    Cols = Screen ^ cols,
    setup_pager(Cols, Messages, PagerInfo, !IO),
    interactive_pager_loop(Screen, PagerInfo, !IO).

:- pred interactive_pager_loop(screen::in, pager_info::in, io::di, io::uo)
    is det.

interactive_pager_loop(Screen, !.PagerInfo, !IO) :-
    draw_pager(Screen, !.PagerInfo, !IO),
    draw_bar(Screen, !IO),
    panel.update_panels(!IO),
    get_char(Char, !IO),
    ( Char = 'q' ->
        true
    ;
        pager_input(Screen, Char, !PagerInfo),
        interactive_pager_loop(Screen, !.PagerInfo, !IO)
    ).

%-----------------------------------------------------------------------------%

:- pred get_char(char::out, io::di, io::uo) is det.

get_char(Char, !IO) :-
    curs.getch(C, !IO),
    ( char.from_int(C, Char0) ->
        Char = Char0
    ;
        get_char(Char, !IO)
    ).

:- pred draw_bar(screen::in, io::di, io::uo) is det.

draw_bar(Screen, !IO) :-
    Cols = Screen ^ cols,
    Panel = Screen ^ bar_panel,
    panel.erase(Panel, !IO),
    panel.attr_set(Panel, fg_bg(white, blue), !IO),
    hline(Panel, char.to_int('-'), Cols, !IO).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
