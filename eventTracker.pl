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

% Custom comparison function: Compare events based on their date
compare_events_by_date(<, (ID1, N1, Date1), (ID2, N2, Date2)) :-
    before(Date1, Date2).

compare_events_by_date(>, (ID1, N1, Date1), (ID2, N2, Date2)) :-
    after(Date1, Date2).

compare_events_by_date(=, (ID1, N1, date(Y,M,D)), (ID2, N2, date(Y,M,D))).

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
% Names must be unique
% now event(eventid, Property, Value)
event(bd, date, date(2024, 4, 17)).
event(bd, name, "BD").
event(bd1, date, date(2024, 3, 17)).
event(bd1, name, "BD").
event(home, date, date(2024, 4, 28)).
event(home, name, "home").
event(interview, date, date(2024, 4, 24)).
event(interview, name, "interview").

% returns true if there is no events occuring on the same date with the same name.
addEvent(N, date(YYYY, MM, DD)) :- 
    date(YYYY, MM, DD), 
    string(N),
    lookupEvent(_, name, N),
    not(lookupEvent(_, date, date(YYYY, MM, DD))).
addEvent(N, date(YYYY, MM, DD)) :- 
    date(YYYY, MM, DD), 
    string(N),
    lookupEvent(_, date, date(YYYY, MM, DD)),
    not(lookupEvent(_, name, N)).
addEvent(N, date(YYYY, MM, DD)) :- 
    date(YYYY, MM, DD), 
    string(N),
    not(lookupEvent(_, date, date(YYYY, MM, DD))),
    not(lookupEvent(_, name, N)).

% returns true is the event with the same property value exists.
lookupEvent(ID, P, V) :- event(ID, P, V).

% define a database for the events in a calendar? maybe in JSON? 