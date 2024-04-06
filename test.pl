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

event(bd, date, date(2024, 4, 17)).
event(bd, name, "BD").
event(bd1, date, date(2024, 3, 17)).
event(bd1, name, "BD").
event(home, date, date(2024, 4, 28)).
event(home, name, "home").

% returns true if there is no events occuring on the same date with the same name.
addEvent(N, date(YYYY, MM, DD)) :- 
    date(YYYY, MM, DD), string(N),
    not(lookupEvent(N, date(YYYY, MM, DD))).
    % notSameNameAndDate(N, date(YYYY, MM, DD)).

notSameNameAndDate(N, date(YYYY, MM, DD)) :- not(event(_, date, date(YYYY, MM, DD))), not(event(_, name, N)).

% returns true is the event with the same name exists.
lookupEvent(N, D) :- event(_, name, N), event(date, D).