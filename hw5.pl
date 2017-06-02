/* CS 381 HW 5 - Glenn Upthagrove, Brian Ozarowicz, David Baugh */

/* Exercise 1 */

when(275,10).
when(261,12).
when(381,11).
when(398,12).
when(399,12).

where(275,owen102).
where(261,dear118).
where(381,cov216).
where(398,dear118).
where(399,cov216).

enroll(mary,275).
enroll(john,275).
enroll(mary,261).
enroll(john,381).
enroll(jim,399).

schedule(Student,Place,Time) :- enroll(Student,Class), where(Class,Place), when(Class,Time).

usage(Place,Time) :- where(Class,Place), when(Class,Time).

conflict(Class1,Class2) :- where(Class1,Place), where(Class2,Place), when(Class1,Time), when(Class2,Time), Class1\=Class2.

meet(Student1,Student2) :- enroll(Student1,Class), enroll(Student2,Class), Student1\=Student2.
meet(Student1,Student2) :- enroll(Student1,Class1), enroll(Student2,Class2), where(Class1,Place), where(Class2,Place), when(Class1,Time1), when(Class2,Time2), Time2 is Time1+1, Student1\=Student2.

/* Exercise 2 */

