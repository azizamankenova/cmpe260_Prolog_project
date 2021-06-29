% aziza mankenova
% compiling: yes
% complete: yes

% Bonus is completed

:- ['load.pro'].

%Subtract one list from another
subtr_lists([],[],[]).
subtr_lists([H1|T1], [H2|T2], [Subtr|TailResult]):-
    subtr_lists(T1, T2, TailResult),
    subtract(H1, H2, Subtr).

%subtract two numbers from each other if both are not equal to 1
subtract(Num1, Num2, Res) :-
    ( Num1 \= -1, Num2 \= -1, Res is Num1-Num2, !);
    Res = 0.
%Square each element in a list
square_list([], []).
square_list([Head|Tail], Result) :-
    square_list(Tail,TailResult),
    HeadResult is Head*Head,
    Result = [HeadResult|TailResult].
%Find the sum of all elements
sum_list([], 0).
sum_list([Head|Tail], Sum):-
    sum_list(Tail, Tailsum),
    Sum is Head + Tailsum.
%3.1 glanian_distance(Name1, Name2, Distance). Find the glanian distance
glanian_distance(Name1, Name2, Distance) :-
    (expects(Name1, _, Feat_list)),
    Exp_feat_list = Feat_list,
    % print(Exp_feat_list),
    (glanian(Name2, _, Features)),
    Gl_features = Features,
    subtr_lists(Exp_feat_list, Gl_features, Res1),
    square_list(Res1, Res2),
    sum_list(Res2, Res3),
    Distance is sqrt(Res3).
%Multiply the corresponding elements of two lists
multiply_two_lists([],[],[]).
multiply_two_lists([H1|T1],[H2|T2], [Mult|TailRes]) :-
    multiply_two_lists(T1, T2, TailRes),
    Mult is H1*H2.
%3.2 weighted_glanian_distance(Name1, Name2, Distance). Weighted Glanian Distance
weighted_glanian_distance(Name1, Name2, Distance) :-
    (expects(Name1, _, Feat_list)),
    Exp_feat_list = Feat_list,    
    (glanian(Name2, _, Features)),
    Gl_features = Features,
    (weight(Name1, Weight_list)),
    subtr_lists(Exp_feat_list, Gl_features, Res1),
    square_list(Res1, Res2),
    multiply_two_lists(Res2, Weight_list, Res3),
    sum_list(Res3, Res4),
    Distance is sqrt(Res4).

%Find the glanian's current city
member_city(Name, Currentcity):-
    ( city(City, HabitantList, _), member(Name, HabitantList), Currentcity = City, ! ).

%3.3 find_possible_cities(Name, CityList). Find possible cities for a glanian
find_possible_cities(Name, CityList) :-
    member_city(Name, HeadCity),
    likes(Name, _, Tailcities),
    List = [HeadCity| Tailcities],
    list_to_set(List, CityList).

%3.4 merge_possible_cities(Name1, Name2, MergedCities). Merge possible cities of two glanians
merge_possible_cities(Name1, Name2, CityList):-
    find_possible_cities(Name1, Clist1),
    find_possible_cities(Name2, Clist2),
    append(Clist1,Clist2, List),
    list_to_set(List, CityList).

%3.5 find_mutual_activities(Name1, Name2, MutualActivities). Find mutual activities of two glanians
find_mutual_activities(Name1, Name2, ActivityList):-
    likes(Name1, LikedAct1, _),
    likes(Name2, LikedAct2, _),
    intersection(LikedAct1, LikedAct2, ActivityList).

%Finding expected glanians for a glanian by expected gender
expected_glanian_by_gender(_,[],[]).
expected_glanian_by_gender(Glanian,[Head|Tail], Allglanians):-
    expected_glanian_by_gender(Glanian, Tail, Glanianlist),
    findall(Name, (glanian(Name, Head, _), Name \= Glanian ), GenderList),
    append(GenderList, Glanianlist, Allglanians).
%Find a list of glanian distances for a list of targets
glanian_dist_list(_,[],[]).
glanian_dist_list(Name, [Head|Tail], [H1|T1]):-
    glanian_dist_list(Name, Tail, T1),
    glanian_distance(Name, Head, H1).

%Creates key value list from two lists
create_key_value_list([],[],[]).
create_key_value_list([H1|T1], [H2|T2], [Head|Tail]):-
    create_key_value_list(T1, T2, Tail),
    Head = H1 - H2.

%3.6 find_possible_targets(Name, Distances, TargetList). Finds possible targets
find_possible_targets(Name, Distances, TargetList):-
    expects(Name, ExpGenders, _),
    expected_glanian_by_gender(Name,ExpGenders, TargetList1),
    glanian_dist_list(Name, TargetList1, Dist),
    create_key_value_list(Dist, TargetList1, KeyedList),
    keysort(KeyedList, Sorted),
    pairs_values(Sorted, TargetList),
    pairs_keys(Sorted, Distances).

%Find a list of weighted glanian distances for a list of targets
glanian_weight_dist_list(_,[],[]).
glanian_weight_dist_list(Name, [Head|Tail], [H1|T1]):-
    glanian_weight_dist_list(Name, Tail, T1),
    weighted_glanian_distance(Name, Head, H1).
    
%3.7 find_weighted_targets(Name, Distances, TargetList).Finds weighted targets
find_weighted_targets(Name, Distances, TargetList):-
    expects(Name, ExpGenders, _),
    expected_glanian_by_gender(Name,ExpGenders, TargetList1),
    glanian_weight_dist_list(Name, TargetList1, Dist),
    create_key_value_list(Dist, TargetList1, KeyedList),
    keysort(KeyedList, Sorted),
    pairs_values(Sorted, TargetList),
    pairs_keys(Sorted, Distances).


%Finds all of the old relationships of a name
find_old_relations(Name, List):-
    findall(Name2, old_relation([Name, Name2]), List1),
    findall(Name3, old_relation([Name3, Name]), List2),
    append(List1, List2, List).

%Check if target is a member of old relationships list
check_old_relation_member(Target, Old_rel, Res):-
    ( \+member(Target, Old_rel), Res = Target,! );
    Res = [].

%Excludes targets who had a relationship from a list of targets
exclude_old_relations([],_,[]).
exclude_old_relations([TargetH|TargetT], Old_rel, [Head|Tail]):-
    exclude_old_relations(TargetT, Old_rel, Tail),
    check_old_relation_member(TargetH, Old_rel, Head).

empty([]).
%Excludes empty lists from a list
delete_empty_lists_inlist(List, Res):-
    exclude(empty, List, Res),!.

%Checks if the feature is within the limit given
check_limits_individually(Limit, Feature, Res):-
    length(Limit, Limitlength),
    (Limitlength = 0, Res = Feature, !);
    (Limit = [H,T],H < Feature, T > Feature, Res = Feature, !);
    Res = [].

%Checks if the features are within the limits
check_features([],[],[]).
check_features([Limits1|Limits2], [Features1|Features2], [Head|Tail]):-
    check_features(Limits2, Features2, Tail),
    check_limits_individually(Limits1, Features1, Head).

%Checks if the list contains empty lists
list_contains_emptylists(List,Target, Res):-
    (\+memberchk([], List), Res = Target, !);
    Res = [].

%checks toleraance limits for every target, and gives a new list. 
check_tolerance_limits(_,[],[]).
check_tolerance_limits(DislikeLimits, [Target1|Target2], [Head|Tail]):-
    check_tolerance_limits(DislikeLimits, Target2, Tail),
    glanian(Target1, _, Features),
    check_features(DislikeLimits, Features, Result),
    list_contains_emptylists(Result, Target1, Head).

%Divides the dashed list into five separate lists
divide_dashed_list([],[],[],[],[]).
divide_dashed_list([Head|Tail], [Dist1| Dist2], [Act1|Act2],[City1|City2], [Targ1|Targ2]) :-
    Head = Dist1 - Act1 - City1 - Targ1,
    divide_dashed_list(Tail, Dist2, Act2, City2, Targ2).

%3.8 find_my_best_target(Name, Distances, Activities, Cities, Targets). Finds the best targets for a glanain
find_my_best_target(Name, Distances, ActivityList, CityList, TargetList) :-
    find_weighted_targets(Name, _, TargetList1),
    find_old_relations(Name, Old_relations),
    exclude_old_relations(TargetList1, Old_relations, TargetList2),
    delete_empty_lists_inlist(TargetList2, TargetList3),
    dislikes(Name,_,_, Limits),
    check_tolerance_limits(Limits, TargetList3, TargetList4),
    delete_empty_lists_inlist(TargetList4, TargetList5),
    check_targetlist(Name, TargetList5, ResList),
    keysort(ResList, Result),
    divide_dashed_list(Result, Distances, ActivityList, CityList, TargetList).

%3.9 find_my_best_match(Name, Distances, Activities, Cities, Targets). Finds the best matches for a glanian
find_my_best_match(Name, Distances, ActivityList, CityList, TargetList):-
    find_weighted_targets(Name, _, TargetList1),
    find_old_relations(Name, Old_relations),
    exclude_old_relations(TargetList1, Old_relations, TargetList2),
    delete_empty_lists_inlist(TargetList2, TargetList3),
    dislikes(Name,_,_, Limits),
    check_tolerance_limits(Limits, TargetList3, TargetList4),
    delete_empty_lists_inlist(TargetList4, TargetList5),
    check_targetlist_best_match(Name, TargetList5, Reslist),
    keysort(Reslist, Result),
    divide_dashed_list(Result, Distances, ActivityList, CityList, TargetList).

%Checks compatability of name and target with city and activity for pred. 3.9. And calculates the distance.
check_compatability9(Name,City, Target, ActivH,  TailRes, Res ):-
    ( is_compatible(Name, City, ActivH), is_compatible(Target, City, ActivH), 
    weighted_glanian_distance(Name, Target, Dist1), 
    weighted_glanian_distance(Target, Name, Dist2),
    Dist is (Dist1+Dist2)/2, 
    append([Dist- ActivH- City - Target], TailRes, Res), !);
    Res = TailRes.

%Checks each activity in a city for compatability for pred. 3.9
check_each_activity9(_, _, _, [], []).
check_each_activity9(Name, Target, City, [ActivH|ActivT], Res):-
    check_each_activity9(Name, Target, City, ActivT, TailRes),
    check_compatability9(Name,City, Target, ActivH, TailRes, Res ).

%Checks each city for all activities for pred. 3.9  to determine the compatability.
check_target_by_city9(_, _, [], []).
check_target_by_city9(Name, Target, [Cityhead|Citytail], Res):-
    check_target_by_city9(Name, Target, Citytail, Tail),
    city(Cityhead,_, Activities),
    check_each_activity9(Name, Target, Cityhead, Activities, Head),
    append(Head,Tail, Res).

%Checks conditions of the predicate 9
pred_9_conditions(Name, Target1, Gender, ExpectedGenders, Length, Size1, Size2, TailResult, Res):-
    (member(Gender, ExpectedGenders), 
    Length \= 0, 
    merge_possible_cities(Name, Target1, MergedCities1),
    list_to_set(MergedCities1, MergedCities),
    Size1 =< 2, Size2 =< 2,
    check_target_by_city9(Name, Target1, MergedCities, HeadResult), append(HeadResult, TailResult, Res),! );
    Res = TailResult.
%Checks the given targetlist for condition of pred. 3.9
check_targetlist_best_match(_,[],[]).
check_targetlist_best_match(Name, [Target1|Target2], Res):-
    check_targetlist_best_match(Name, Target2, TailResult),
    glanian(Name, Gender, Features),
    expects(Target1, ExpectedGenders, _),
    dislikes(Target1, DislikedActTarget1, _, ToleranceLimits),
    check_features(ToleranceLimits, Features, List1),
    list_contains_emptylists(List1,[1], L1),
    length(L1, Length),
    dislikes(Name, DislikedActName, _,_),
    likes(Name, LikedActName, _),
    likes(Target1, LikedActTarget1, _),
    intersection(DislikedActName, LikedActTarget1, Inters1),
    intersection(LikedActName, DislikedActTarget1, Inters2),
    length(Inters1, Size1),
    length(Inters2, Size2),
    pred_9_conditions(Name, Target1, Gender, ExpectedGenders, Length, Size1, Size2, TailResult, Res).
    

%Checks if the name is compatible with a city and activity
is_compatible(Name, City, Activity):-
    find_possible_cities(Name, PossibleCities),
    city(City, _, ActivityList),
    dislikes(Name, DislikedAct, DislikedCities,_),
    likes(Name, LikedAct, _),
    memberchk(Activity, ActivityList),
    ((memberchk(City, PossibleCities),\+memberchk(City, DislikedCities), \+memberchk(Activity,DislikedAct), !);
    (\+memberchk(City, DislikedCities), memberchk(Activity, LikedAct))).

%Checks compatability of name with city and activity for pred. 3.8. And calculates the distance.
check_compatability(Name,City, Target, ActivH,  TailRes, Res ):-
    ( is_compatible(Name, City, ActivH), 
    weighted_glanian_distance(Name, Target, Dist), 
    append([Dist- ActivH- City - Target], TailRes, Res), !);
    Res = TailRes.

%Checks each activity in a city for compatability for pred. 3.8
check_each_activity(_, _, _, [], []).
check_each_activity(Name, Target, City, [ActivH|ActivT], Res):-
    check_each_activity(Name, Target, City, ActivT, TailRes),
    check_compatability(Name,City, Target, ActivH, TailRes, Res ).

%Checks each city for all activities for pred. 3.8 to determine the compatability.
check_target_by_city(_, _, [], []).
check_target_by_city(Name, Target, [Cityhead|Citytail], Res):-
    check_target_by_city(Name, Target, Citytail, Tail),
    city(Cityhead,_, Activities),
    check_each_activity(Name, Target, Cityhead, Activities, Head),
    append(Head,Tail, Res).

%Checks the intersection of activities, and calls check_target_by_city
check_intersection_act(Intersection, Name, H, MergedCities, TailResult, Res):-
    ( length(Intersection, Size), Size =< 2, check_target_by_city(Name, H, MergedCities, HeadResult), append(HeadResult, TailResult, Res),! );
    Res = TailResult.

%Checks the given targetlist for condition of pred. 3.8
check_targetlist(_, [],[]).
check_targetlist(Name, [H|T], Res):-
    check_targetlist(Name, T, TailResult),
    merge_possible_cities(Name, H, MergedCities1),
    list_to_set(MergedCities1, MergedCities),
    dislikes(Name, DislikedAct, _,_),
    likes(H, LikedAct, _),
    intersection(DislikedAct, LikedAct, Intersection),
    check_intersection_act(Intersection,  Name, H, MergedCities, TailResult, Res).


%BONUS - top 10 best matches. Firstly, it finds the best match for each glanian using the predicate 3.9, and stores the result. I think the predicate 3.9 
%is the most suitable as it calculates the best match taking ito account all of the conditions and preferences. 
%in a list as tuples of Dist-Name-Target. Then it is sorted. Then only the first 20 are extracted because the permutations are possible.
%Then take_pairs leaves the list of pairs as Name-Target, excluding the Dist. Then the duplicates are removed from the list.
%Then only 10 pairs are extracted from a list into a file named top10.txt. 
top_10_best_matches(Result):-    
    findall(Dist-Name-Target1, ( glanian(Name, _,_), find_my_best_match(Name, [Dist|_], _, _, [Target1|_]) ), Bag),
    keysort(Bag, Sorted),
    extract(20, Sorted, Res),
    take_pairs(Res, Result1),
    make_listinlist(Result1, Result2),
    delete_duplicates(Result2, Result3),
    make_dashedlist(Result3, Result4),
    extract(10, Result4, Result),
    open('top10.txt',write,Out),
    print_to_file(Result, Out),
    close(Out).

%prints list elements on new line
print_to_file([],_).
print_to_file([X|List], Out) :-
    writeln(Out, X),
    print_to_file(List,Out).

%Deletes duplicates
delete_duplicates([],[]).
delete_duplicates([H|T], TT) :- 
    msort(H,SH), 
    contains_dup(SH,T), 
    delete_duplicates(T, TT).
delete_duplicates([H|T], [H|TT]) :- 
    msort(H,SH), 
    \+ contains_dup(SH,T), 
    delete_duplicates(T, TT).

%Checks for duplicates
contains_dup(_, []) :- 
    fail.
contains_dup(SH, [H|_]) :- 
    msort(H, SH).
contains_dup(SH, [_|T]) :- 
    contains_dup(SH, T).

%creates dashed list
make_dashedlist([],[]).
make_dashedlist([[H1,H2]|Tail], [H1-H2 | TailRes]):-
    make_dashedlist(Tail, TailRes).

%Makes lists in list
make_listinlist([],[]).
make_listinlist([H1-H2|Tail], [[H1,H2]|TailRes]):-
    make_listinlist(Tail, TailRes).

%Dist-Name-Target ->Name-Target
take_pairs([], []).
take_pairs([Head | Tail ], [Name-Target|Res2]):-
    take_pairs(Tail, Res2),
    Head = _-Name-Target.

%extract first N elements from a list
extract(N, List, Front):- 
    length(Front, N), append(Front, _, List).

