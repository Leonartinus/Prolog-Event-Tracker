
/*
  parts of the natural language grammar below are from: 
  https://www.cs.ubc.ca/~poole/cs312/2024/prolog/geography_QA.pl
*/

/*
"Can I add an event on 2002, 11, 25?"
"Can I add an event on 2002, 11, 25 in the ballroom?"
"Can I add an event on 2002, 11, 25 at 16:00?"

"Is 2002, 11, 25 free?"

"When is the next available day after 2022, 5, 6?"
"When is the next available time on 2012, 6, 5?"

"When is BD?"
"Where is BD?"

"What events happen on 2023, 9, 4?"
"What events happen on 2023, 9, 4 at 21:00?"
"What events happen in the ballroom?"
"What events happen on 2023, 9, 4 at 21:00 in the ballroom?
*/

% A noun phrase is a determiner followed by adjectives followed
% by a noun followed by an optional modifying phrase:
noun_phrase(L0, L4, Ind) :-
    det(L0, L1, Ind), 
    adjectives(L1, L2, Ind),
    noun(L2, L3, Ind),
    omp(L3, L4, Ind).
noun_phrase([N | L], L, E) :- name(E, N).

det(["the" | L], L, _).
det(["a" | L], L, _).
det(["my" | L], L, _).
det(L, L, _).

% adjectives(L0, L1, Ind) is true if 
% L0-L1 is a sequence of adjectives that true of Ind
adjectives(L0, L2, Ind) :-
    adj(L0, L1, Ind), 
    adjectives(L1, L2, Ind).
adjectives(L, L, _).

% A  modifying phrase / relative clause is either
% a relation (verb or preposition) followed by a noun_phrase or
% 'that' followed by a relation then a noun_phrase 
mp(L0, L2, Subject) :-
    reln(L0, L1, Subject, Object), 
    aphrase(L1, L2, Object).
mp(["that" | L0], L2, Subject) :-
    reln(L0, L1, Subject, Object), 
    aphrase(L1, L2, Object).

% reln(L0,L1,Sub,Obj) is true if L0-L1 is a relation on individuals Sub and Obj
reln(["borders" | L], L, Sub, Obj) :- borders(Sub, Obj).
reln(["bordering" | L], L, Sub, Obj) :- borders(Sub, Obj).
reln(["next", "to" | L], L, Sub, Obj) :- borders(Sub, Obj).
reln(["the", "capital", "of" | L], L, Sub, Obj) :- capital(Obj, Sub).
reln(["the", "name", "of" | L], L, Sub, Obj) :- name(Obj, Sub).
% reln(["free", "on" | L], L, P, D) :- findall()

% An optional modifying phrase is either a modifying phrase or nothing
omp(L0,L1,E) :-
    mp(L0,L1,E).
omp(L, L, _).

% dphrase is is true if the string contains a valid date
dphrase([Y | [M | [D | _]]], date(YN, MN, DN)) :- 
    number_string(YN, Y),
    number_string(MN, M),
    number_string(DN, D),
    date(YN, MN, DN).
dphrase([N | _], D) :-
    event(ID, name, N).
    event(ID, date, D).
% try dphrase(["2002","3","6","isdlfkj"], D).

% lphrase is true if string contains location that is part of an event
lphrase([N | _], P) :-
    event(ID, name, N),
    event(ID, place, P).

% a phrase is a noun_phrase or a modifying phrase
aphrase(L0, L1, E) :- noun_phrase(L0, L1, E).
aphrase(L0, L1, E) :- mp(L0, L1, E).

% change individual to yes, no answers
yes(_, yes).
no(_, no).

% yes no questions: can I add on this day?, is the day free?
% question(Question, QR, Ind) is true if Ind is  an answer to Question
question(["Is" | L0], _, Ans) :-
    dphrase(L0, D),
    not(lookupEvent(_, date, D)),
    yes(_, Ans).
question(["Is" | L0], _, Ans) :-
    dphrase(L0, D),
    lookupEvent(_, date, D),
    no(_, Ans).
question(["Can", "I", "add", "an", "event", "on" | L0], _, Ans) :-
    dphrase(L0, D),
    not(lookupEvent(_, date, D)),
    yes(_, Ans).
question(["Can", "I", "add", "an", "event", "on" | L0], _, Ans) :-
    dphrase(L0, D),
    lookupEvent(_, date, D),
    no(_, Ans).
question(["Where", "is" | L0], _, Ind) :-
    lphrase(L0, Ind).
question(["When", "is" | L0], _, Ind) :-
    dphrase(L0, Ind).

question(["What", "is" | L0], L1, Ind) :-
    aphrase(L0, L1, Ind).
question(["What" | L0], L2, Ind) :-
    noun_phrase(L0, L1, Ind), 
    mp(L1, L2, Ind).
question(["What" | L0], L1, Ind) :-
    mp(L0, L1, Ind).

% ask(Q, A) gives answer A to question Q
ask(Q, A) :-
    question(Q, [], A).

% To get the input from a line:
q(Ans) :-
    write("Ask me: "), flush_output(current_output), 
    read_line_to_string(user_input, St),
    notin(St, ["quit", "quit.", "q", "q."]), % quit or q ends interaction
    split_string(St, " -", " ,?.!-", Ln), % ignore punctuation
    write(Ln),
    (ask(Ln, Ans)  
    ;  write("No more answers\n"),  % ; is "or"
      q(Ans)).

% notin(E,L) is true if E is not in list L. Allows for E or elements of L to be variables.
notin(_,[]).
notin(E,[H|T]) :-
    dif(E,H),
    notin(E,T). 