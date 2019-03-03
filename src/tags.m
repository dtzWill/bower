% Bower - a frontend for the Notmuch email system
% Copyright (C) 2012 Peter Wang

:- module tags.
:- interface.

:- import_module data.

:- import_module list.
:- import_module set.

:- type new
    --->    new
    ;       old.

:- type unread
    --->    unread
    ;       read.

:- type replied
    --->    replied
    ;       not_replied.

:- type deleted
    --->    deleted
    ;       not_deleted.

:- type flagged
    --->    flagged
    ;       unflagged.

:- type inboxed
    --->    inboxed
    ;       not_inboxed.

:- type standard_tags
    --->    standard_tags(
                unread :: unread,
                replied :: replied,
                deleted :: deleted,
                flagged :: flagged,
                inboxed :: inboxed
            ).

:- type tag_delta
    --->    tag_delta(string). % +tag or -tag

:- func tag_delta_to_string(tag_delta) = string.

    % There should be more of these instead of bare strings.
:- func draft_tag = tag.
:- func draft_sign_tag = tag.
:- func encrypted_tag = tag.

:- pred display_tag(tag::in) is semidet.

:- pred get_standard_tags(set(tag)::in, standard_tags::out, int::out) is det.

:- pred validate_tag_deltas(list(string)::in, list(tag_delta)::out,
    set(tag)::out, set(tag)::out) is semidet.

:- pred display_tag_string(tag::in, string::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module string.

:- import_module string_util.

%-----------------------------------------------------------------------------%

tag_delta_to_string(tag_delta(String)) = String.

%-----------------------------------------------------------------------------%

draft_tag = tag("draft").
draft_sign_tag = tag("draft-sign").
encrypted_tag = tag("encrypted").

%-----------------------------------------------------------------------------%

display_tag(Tag) :-
    not nondisplay_tag(Tag).

:- pred nondisplay_tag(tag::in) is semidet.

nondisplay_tag(tag("deleted")).
nondisplay_tag(tag("flagged")).
nondisplay_tag(tag("new")).
nondisplay_tag(tag("replied")).
nondisplay_tag(tag("sent")).
nondisplay_tag(tag("signed")).
nondisplay_tag(tag("unread")).
nondisplay_tag(tag("important")).
nondisplay_tag(tag("inbox")).
nondisplay_tag(tag("lists")).
nondisplay_tag(tag("old/Research")).
nondisplay_tag(tag("llvm dev lists/llvm-commits")).
nondisplay_tag(tag("llvm dev lists/clang-commits")).
nondisplay_tag(tag("llvm dev lists/clang-dev")).
nondisplay_tag(tag("llvm dev lists/clang-users")).
nondisplay_tag(tag("llvm dev lists/llvm-dev")).
nondisplay_tag(tag("llvm dev lists/llvm-bugs")).
nondisplay_tag(tag("_my projects/ALLVM")).
nondisplay_tag(tag("_my projects/ALLVM/commits")).

%-----------------------------------------------------------------------------%

get_standard_tags(Tags, StdTags, DisplayTagsWidth) :-
    StdTags0 = standard_tags(read, not_replied, not_deleted, unflagged,
        not_inboxed),
    set.fold2(get_standard_tags_2, Tags, StdTags0, StdTags,
        0, DisplayTagsWidth).

display_tag_string(Tag, S) :-
  ( display_tag(Tag) ->
    Tag = tag(OrigTagName),
    T = string.remove_prefix_if_present("lists/", OrigTagName),
    S =
    ( T = "calendar" -> "📅"
    ; T
    )
  ;
    % TODO: make this type error instead
    S = ""
  ).

:- pred get_standard_tags_2(tag::in, standard_tags::in, standard_tags::out,
    int::in, int::out) is det.

get_standard_tags_2(Tag, !StdTags, !DisplayTagsWidth) :-
    ( Tag = tag("unread") ->
        !StdTags ^ unread := unread
    ; Tag = tag("replied") ->
        !StdTags ^ replied := replied
    ; Tag = tag("deleted") ->
        !StdTags ^ deleted := deleted
    ; Tag = tag("flagged") ->
        !StdTags ^ flagged := flagged
    ; Tag = tag("inbox") ->
        !StdTags ^ inboxed := inboxed
    ; display_tag(Tag) ->
        display_tag_string(Tag, S),
        % Add one for separator.
        !:DisplayTagsWidth = !.DisplayTagsWidth +
            string_wcwidth(S) + 1
    ;
        true
    ).

%-----------------------------------------------------------------------------%

validate_tag_deltas(Words, TagDeltas, AddTags, RemoveTags) :-
    list.map_foldl2(validate_tag_delta, Words, TagDeltas,
        set.init, AddTags, set.init, RemoveTags).

:- pred validate_tag_delta(string::in, tag_delta::out,
    set(tag)::in, set(tag)::out, set(tag)::in, set(tag)::out) is semidet.

validate_tag_delta(Word, tag_delta(Word), !AddTags, !RemoveTags) :-
    (
        string.remove_prefix("+", Word, Tag),
        not blacklist_tag(Tag)
    ->
        set.insert(tag(Tag), !AddTags)
    ;
        string.remove_prefix("-", Word, Tag),
        not blacklist_tag(Tag)
    ->
        set.insert(tag(Tag), !RemoveTags)
    ;
        fail
    ).

:- pred blacklist_tag(string::in) is semidet.

blacklist_tag("").
blacklist_tag("-").
blacklist_tag("+").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
