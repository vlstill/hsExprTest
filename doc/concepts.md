This page explains some concepts used in our evaluator.

## Evaluator

Evaluator is the whole service which performs assignment evaluation, starting from collection of the assignment, then its evaluation, and finally providing feedback and points to the student.
In order to do its work, it can communicate with students using several [interfaces](#evaluator-interface) and mark assignments using [checkers](#checker).

## Evaluator Interfaces

Evaluator interface defines the way the evaluator communicates with the student for the given assignment.
Currently, there are the following interfaces.

* [**IS MU Questionares**][isq] -- this interface is useful mostly for short assignments, possibly with richer interface.
    For example, it is possible to create a JavaScript editor of finite automata in the questionare, or to present graphical data (embedded in HTML) in the feedback.
    This interface is synchronous -- i.e., the results are expected to come fast.

* [**IS MU Submission Folders**][isf] -- this interface can be used to poll submission folders, collect submitted files, and write results of evaluation into IS and send them by e-mail.
    For this interface, speed of replies is not as critical.

* [**Unprivileged Interface**][unpr] -- this interface allows [hint mode](#hind-mode) submissions only.

[isq]: is-questionares.md
[iqf]: is-submission-folders.md
[unpr]: unprivileged.md

## Checker

Checker is (usually) course-specific script or program which handles actual evaluation of the assignment (but not communication collection of solutions and submission of results.
More about checkers can be found in [separate documentation page][checkers]

[checkers]: checkers.md

## Hint Mode

Sometimes it is useful to allow students to have some feedback even before they submit the exercise.
For example, the Haskell checker has support to run only the syntax check.
To support this, it is possible to enable hint mode in the [main configuration][mainconf] for the given course and then to send solution over the [unprivileged  interface][unpr] -- these submissions are automatically using hint mode (or are blocked if hint mode is not allowed for the given course.

[mainconf]: main_configuration.md
