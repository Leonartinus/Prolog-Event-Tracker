/*
  parts of the natural language grammar below are from: 
  Poole and Mackworth, Artificial Intelligence: foundations of computational agents, Cambridge, 2017
*/

/*
"Can I add an event on 2002, 11, 25?"
"Can I add an event on 2002, 11, 25 in the ballroom?"
"Can I add an event on 2002, 11, 25 at 16:00?"

"Is 2002, 11, 25 free?"

"When is the next available day?"
"When is the next available time on 2012, 6, 5?"

"What events happen on 2023, 9, 4?"
"What events happen on 2023, 9, 4 at 21:00?"
"What events happen in the ballroom?"
"What events happen on 2023, 9, 4 at 21:00 in the ballroom?
*/
?- [eventTracker].

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
dphrase([Y | [M | [D | _ ]]]) :- 
    number_string(YN, Y),
    number_string(MN, M),
    number_string(DN, D),
    date(YN, MN, DN).

% a phrase is a noun_phrase or a modifying phrase
aphrase(L0, L1, E) :- noun_phrase(L0, L1, E).
aphrase(L0, L1, E) :- mp(L0, L1, E).

% question(Question, QR, Ind) is true if Ind is  an answer to Question
question(["Is" | L0], L2, Ind) :-
    noun_phrase(L0, L1, Ind), 
    mp(L1, L2, Ind).
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
    (ask(Ln, Ans)  
    ;  write("No more answers\n"),  % ; is "or"
      q(Ans)).