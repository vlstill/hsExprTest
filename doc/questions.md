# Questions

A question, or assignment, is given by a file in the [question
repository][qrep].
The question file to be used for evaluation is given by the question ID (which
is sent from the frontend): the file with question is named
`QUESTION_ID.q.SUFFIX`, where `SUFFIX` is typically the suffix customarily used
with the given type of file.
For example, file with path (relative to question repository) `2019/0101.q.py`
corresponds to the question with ID `2019/0101` and is probably a Python file.
As we can see in this example, question ID can contain the forward slash
character (`/`), and therefore questions can be organized into directories.

The interpretation of content of the question file is left to the
[checker][checker] given for the course; therefore, it is easily possible to
create custom question formats.

## Options

In case there are multiple questions which differ only in some kind of
parameter, it is possible to use the same question ID and add a parameter to
it.
This is done using a question mark (`?`) character after the question ID: for
example: `2019/0101.q.py?ARG`.
The value of the argument (`ARG` in this case) is passed to the
[checker][checker] on commandline, and it can use it in any way.

## Question Repository

[qrep]: #question-repository
[checker]: checkers.md
