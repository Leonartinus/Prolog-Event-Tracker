:- discontiguous event/3.
% even(N) is true if number N is an even number
even(N) :- 0 is mod(N,2).

% odd(N) is true if number N is an odd number
odd(N) :- not(even(N)).

% maxDay(YYYY, MM, MD) is true if year, month and the max days of the month matches
maxDay(YYYY, 2, 29) :- leapYear(YYYY).
maxDay(YYYY, 2, 28) :- not(leapYear(YYYY)).
maxDay(_, MM, 31) :- between(1, 7, MM), odd(MM).
maxDay(_, MM, 30) :- between(1, 7, MM), even(MM), not(MM is 2).
maxDay(_, MM, 30) :- between(8, 12, MM), odd(MM).
maxDay(_, MM, 31) :- between(8, 12, MM), even(MM).

% leapYear(YYYY) is true if year YYYY is a leap year
leapYear(YYYY) :- 0 is mod(YYYY,4).

% date(YYYY, MM, DD) is true if the date is a valid date
date(YYYY, MM, DD) :- YYYY > 0, between(1, 12, MM), maxDay(YYYY, MM, MD), between(1, MD, DD).

% before(date(YYYY1, MM1, DD1), date(YYYY2, MM2, DD2)) is true if date1 is before date2
before(date(YYYY1, _, _), date(YYYY2, _, _)) :- YYYY2 > YYYY1.
before(date(YYYY, MM1, _), date(YYYY, MM2, _)) :- MM2 > MM1.
before(date(YYYY, MM, DD1), date(YYYY, MM, DD2)) :- DD2 > DD1.

% after(date(YYYY1, MM1, DD1), date(YYYY2, MM2, DD2)) is true if date1 is after date2.
after(date(YYYY1, MM1, DD1), date(YYYY2, MM2, DD2)) :- 
    not(before(date(YYYY1, MM1, DD1), date(YYYY2, MM2, DD2))),
    not(same(date(YYYY1, MM1, DD1), date(YYYY2, MM2, DD2))).

% same is true if 2 dates are the same.
same(date(Y,M,D), date(Y,M,D)).

% daysDiff(date1, date2, D) is true if the days difference between date1 and date2 is D
daysDiff(date(YYYY, MM, DD), date(YYYY, MM, DD), 0).
daysDiff(date(YYYY, MM, DD1), date(YYYY, MM, DD2), D) :- 
    before(date(YYYY, MM, DD1), date(YYYY, MM, DD2)),
    D is (DD2 - DD1).
daysDiff(date(YYYY, MM1, DD1), date(YYYY, MM2, DD2), D) :- 
    before(date(YYYY, MM1, DD1), date(YYYY, MM2, DD2)),
    maxDay(YYYY, MM1, MD),
    daysDiff(date(YYYY, MM1, DD1), date(YYYY, MM1, MD), DA),
    MM3 is MM1 + 1,
    daysDiff(date(YYYY, MM3, 1), date(YYYY, MM2, DD2), DB),
    D is (DA + DB + 1).
daysDiff(date(YYYY1, MM1, DD1), date(YYYY2, MM2, DD2), D) :-
    before(date(YYYY1, MM1, DD1), date(YYYY2, MM2, DD2)),
    daysDiff(date(YYYY1, MM1, DD1), date(YYYY1, 12, 31), DA),
    YYYY3 is YYYY1 + 1,
    daysDiff(date(YYYY3, 1, 1), date(YYYY2, MM2, DD2), DB),
    D is DA + DB + 1.
daysDiff(date(YYYY1, MM1, DD1), date(YYYY2, MM2, DD2), D) :-
    after(date(YYYY1, MM1, DD1), date(YYYY2, MM2, DD2)),
    daysDiff(date(YYYY2, MM2, DD2), date(YYYY1, MM1, DD1), DN),
    D is -DN.

% eventAfter(Date, Event) is true if the Event is after the Date
eventAfter(Date0, SortedEvents) :-
    findall(event(ID, date, Date), (event(ID, date, Date), before(Date0, Date)), Events),
    findEventName(Events, Names),
    sort_events_by_date(Names, SortedEvents).

% find event names given ids
findEventName(Events, Names) :-
    findall((ID, N,D), (event(ID, name, N), member(event(ID, date, D), Events)), Names).

% find event names given ids
findEventPlace(Events, Names) :-
    findall((ID, N,D), (event(ID, name, N), member(event(ID, date, D), Events)), Names).

% Custom comparison function: Compare events based on their date
compare_events_by_date(<, (_, _, Date1), (_, _, Date2)) :-
    before(Date1, Date2).

compare_events_by_date(>, (_, _, Date1), (_, _, Date2)) :-
    after(Date1, Date2).

compare_events_by_date(=, (_, _, date(Y,M,D)), (_, _, date(Y,M,D))).

% Sort the list of events based on their date
sort_events_by_date(Events, SortedEvents) :-
    predsort(compare_events_by_date, Events, SortedEvents).


% returns the next Event of the given date
nextEvent(Date0, E) :-
    findall(event(ID, date, Date), (event(ID, date, Date), before(Date0, Date)), Events),
    findEventName(Events, Names),
    sort_events_by_date(Names, SortedEvents),
    head(SortedEvents, E).

% head is true if H is the head of the list
head([H|_], H).

% Pertaining to events:
% Reified events, this way we can add new aspects to the events much more easily
% ids must be unique
% now event(eventid, Property, Value)

% returns true if there is no events occuring on the same date with the same name.
addEvent(N, date(YYYY, MM, DD)) :- 
    date(YYYY, MM, DD), 
    string(N),
    event(_, name, N),
    not(event(_, date, date(YYYY, MM, DD))).
addEvent(N, date(YYYY, MM, DD)) :- 
    date(YYYY, MM, DD), 
    string(N),
    event(_, date, date(YYYY, MM, DD)),
    not(event(_, name, N)).
addEvent(N, date(YYYY, MM, DD)) :- 
    date(YYYY, MM, DD), 
    string(N),
    not(event(_, date, date(YYYY, MM, DD))),
    not(event(_, name, N)).


/*
  parts of the natural language grammar below are from: 
  https://www.cs.ubc.ca/~poole/cs312/2024/prolog/geography_QA.pl
*/

/*
"Can I add an event on 2002, 11, 25?"
"Can I add an event on 2002, 11, 25 in the ballroom?"
"Can I add an event on 2002, 11, 25 at 16:00?"

"Is 2002, 11, 25 free?"

"When is BD?"
"Where is BD?"
"What happens in ballroom?"

Todo:

"When is the next available day after 2022, 5, 6?"
"When is the next available time on 2012, 6, 5?"

"What events happen on 2023, 9, 4?"
"What events happen on 2023, 9, 4 at 21:00?"
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

% get the date from name
gdphrase([N | _], D) :-
    event(ID, name, N),
    event(ID, date, D).
% try dphrase(["2002","3","6","isdlfkj"], D).

% lphrase is true if string contains location that is part of an event
lphrase([N | _], P) :-
    event(ID, name, N),
    event(ID, place, P).

% get name from place
glphrase([P | _], N) :-
    event(ID, place, P),
    event(ID, name, N).

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
    not(event(_, date, D)),
    yes(_, Ans).
question(["Is" | L0], _, Ans) :-
    dphrase(L0, D),
    event(_, date, D),
    no(_, Ans).
question(["Can", "I", "add", "an", "event", "on" | L0], _, Ans) :-
    dphrase(L0, D),
    not(event(_, date, D)),
    yes(_, Ans).
question(["Can", "I", "add", "an", "event", "on" | L0], _, Ans) :-
    dphrase(L0, D),
    event(_, date, D),
    no(_, Ans).
question(["Where", "is" | L0], _, Ind) :-
    lphrase(L0, Ind).
question(["When", "is" | L0], _, Ind) :-
    gdphrase(L0, Ind).
question(["What", "happens", "on" | L0], _, Ind) :-
   dphrase(L0, D),
   event(ID, date, D),
   event(ID, name, Ind).
question(["What", "happens", "in" | L0], _, Ind) :-
    glphrase(L0, Ind).


% ask(Q, A) gives answer A to question Q
ask(Q, A) :-
    question(Q, [], A).

/*
To get the input from a line:
*/ 

q(Ans) :-
    write("Ask me: "), flush_output(current_output), 
    read_line_to_string(user_input, St),
    notin(St, ["quit", "quit.", "q", "q."]), % quit or q ends interaction
    split_string(St, " -", " ,?.!-", Ln), % ignore punctuation
    % write(Ln),
    (ask(Ln, Ans)  
    ;  write("No more answers\n"),  % ; is "or"
      q(Ans)).

% notin(E,L) is true if E is not in list L. Allows for E or elements of L to be variables.
notin(_,[]).
notin(E,[H|T]) :-
    dif(E,H),
    notin(E,T). 

% databaseevent(bd, date, date(2024, 4, 17)).
event(bd, name, "BD").
event(bd, place, "Breka").
event(bd1, date, date(2024, 3, 17)).
event(bd1, name, "BD1").
event(bd1, place, "Home").
event(home, date, date(2024, 4, 28)).
event(home, name, "home").
event(home, place, "123 smiling ave").
event(interview, date, date(2024, 4, 24)).
event(interview, name, "interview").
event(interview, place, "ballroom").

wr(D) :- write(D).