%% The task from the interview - CONTACTS
%%
%% We have contacts gathered from different sources and some contacts
%% share the same emails, so we can group multiple contacts related
%% to the same person by finding same emails in different contatcs.
%%
%% Write program that groups such contacts.
%%
-module(contacts).

-export([start/0]).
-compile(export_all).

start() ->
    true and
        test(#{1 => [a, b],
               2 => [c, d],
               3 => [e],
               4 => [f, g],
               5 => [d, h],
               6 => [a, f]},
             [[1, 4, 6], [2, 5], [3]]) and
        test(#{1 => [a, b],
               2 => [c, d],
               3 => [e, f],
               4 => [g, h],
               5 => [i, j],
               6 => [i, h],
               7 => [g, f],
               8 => [e, d],
               9 => [c, b]},
             [[1, 2, 3, 4, 5, 6, 7, 8, 9]]) and
        true.

test(Contacts, Expected) ->
    Result = group(Contacts),
    SortedResult = lists:sort([lists:sort(ContactList) || ContactList <- Result]),
    fbcommon:test(Contacts, Expected, SortedResult).


group(ContactsToEmails) ->
    EmailsToContacts = contacts_to_emails(ContactsToEmails),
    ContactsAdjacency = maps:values(EmailsToContacts),
    group_adjacent_in_forest(ContactsAdjacency).

contacts_to_emails(ContactsToEmails) ->
    maps:fold(fun contacts_to_emails/3, #{}, ContactsToEmails).

contacts_to_emails(Contact, Emails, EmailsToContacts) ->
    lists:foldl(fun(Email, Acc) ->
                        maps:update_with(Email,
                                         fun(Contacts) -> [Contact | Contacts] end,
                                         [Contact], Acc)
                end, EmailsToContacts, Emails).

group_adjacent_in_forest([ListOfAdjacent | Forest]) ->
    process_adjacent(ListOfAdjacent, Forest, []);
group_adjacent_in_forest([]) ->
    [].

process_adjacent([Vertice | Vertices], Forest, Processed) ->
    {NewVertices, NewForest} = find_containing_vertice(Vertice, Forest),
    process_adjacent(NewVertices ++ Vertices, NewForest, [Vertice | Processed]);
process_adjacent([], Forest, Processed) ->
    [Processed | group_adjacent_in_forest(Forest)].

find_containing_vertice(Vertice, Forest) ->
    lists:foldl(fun(ListOfAdjacent, {NewVertices, NewForest}) ->
                        case lists:member(Vertice, ListOfAdjacent) of
                            true ->
                                {lists:filter(fun(Item) -> Item =/= Vertice end,
                                              ListOfAdjacent)
                                 ++ NewVertices, NewForest};
                            false ->
                                {NewVertices, [ListOfAdjacent | NewForest]}
                        end
                end, {[], []}, Forest).
