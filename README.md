# Advanced Programming 2024

<img align="right" width="300" src="https://github.com/diku-dk/ap-e2024-pub/assets/55833/2a499f62-386c-4fbb-a66e-108a027364a0">

Welcome to the course website for Advanced Programming (AP) 2024! All
material and general information will be provided here. Announcements,
assignment handin, and the discussion forum remains on Absalon. While
this website is a Git repository, you are not required or expected to
use Git to interact with it, but feel free to do so if convenient for
you.

The programming language we will be using is **Haskell**.

The website and course is currently under construction, and all
information is still tentative.

The teaching material for each week can be found in the `weekI`
subdirectories.

The assignments for each week can be found in the `assignmentI`
subdirectories.

## Discord

We have created a Discord server for discussing the course: [Invite
link](https://discord.gg/dJgTJ7mry7). Please use your government name
rather than your sweet hacker nick.

You are not *required* or *expected* to use Discord. We continue to
monitor the Absalon discussion forum, and course announcements are
posted solely on Absalon.

## Preparation

AP has a reputation as a difficult course, particularly for
inexperienced programmers, and *especially* if you do not have prior
experience with functional programming. If you worry that your
background is insufficient, we recommend spending a little time
(re-)familiarising yourself with functional programming. The specific
language and book does not matter, but since we will use Haskell in
AP, you might as well read [Programming in
Haskell](https://www.cs.nott.ac.uk/~pszgmh/pih.html) by Graham Hutton.
If you do not like the style of this book, there are plenty others
available (such as [Haskell from First
Principles](https://haskellbook.com/)).

Many Haskell books focus on industrial uses, and go into great detail
about how to use complex libraries and such. This is well and good for
real-world programming, but unnecessary for AP, where we take a more
narrow focus. You do not need to read a thick book, unless you want
to.

If you have taken the course previously, see [these notes on
translating Erlang concepts to Haskell](erlang.md).

## Textbooks

We do not mandate a specific textbooks for learning Haskell, but we do
have suggestions. We will also specify some research papers and course
notes as mandated or suggested reading.

* [Programming in Haskell](https://www.cs.nott.ac.uk/~pszgmh/pih.html) (video lectures [here](https://www.youtube.com/playlist?list=PLF1Z-APd9zK7usPMx3LGMZEHrECUGodd3) and [here](https://www.youtube.com/playlist?list=PLF1Z-APd9zK5uFc8FKr_di9bfsYv8-lbc))
* [AP Course Notes](https://diku-dk.github.io/ap-notes/)
* [Course notes from Brent Yorgey's UPenn Haskell course](https://www.cis.upenn.edu/~cis1940/spring13/lectures.html)

We will also occasionally suggest reading material labeled *Going
Beyond*. This is material that goes beyond the requirements of the
course, but may be of interest to students who wish to apply the
techniques we investigate in applied settings. That is, students who
wish to become capable of writing advanced Haskell programs that solve
real problems.

## Course-specific material

* [Guide for setting up a Haskell development environment](haskell.md)
* [Haskell programming hints](haskell-hints.md)

## Software

See [this document on setting up a Haskell development environment](haskell.md).

## Course structure

The course consists of two lectures and two exercise classes every
week.

The lectures are on Tuesday 10:00-12:00 in Aud 01 at HCØ and Thursday
13:00-15:00 in Store UP1 at DIKU.

The exercise classes are both on Thursday, at 10:00-12:00 and
15:00-17:00 respectively.

### Exercise class locations

* **Hold 1:**

  - TA: [Robert Schenck](https://github.com/zfnmxt) (rschenck@di.ku.dk)

  - 10-12: DIKU 3-0-25

  - 15-17: NBB 2.2.I.158

* **Hold 2:**

  - TA: TBD

  - 10-12: DIKU 1-0-37

  - 15-17: NBB 2.2.H.142

* **Hold 3:**

  - TA: TBD

  - 10-12: DIKU 1-0-04

  - 15-17: NBB 2.3.H.142

* **Hold 4:**

  - TA: TBD

  - 10-12: DIKU 1-0-26

  - 15-17: NBB NBB 2.3.I.164

* **Hold 5:**

  - TA: Mikkel

  - 10-12: DIKU 1-0-30

  - 15-17: DIKU 1-0-30

* **Hold 6:**

  - TA: Ian

  - 10-12: DIKU 1-0-18

  - 15-17: DIKU 1-0-18

* **Hold 7:**

  - TA: Rasmus Pallisgaard

  - 10-12: DIKU 1-0-22

  - 15-17: DIKU 1-0-22

* **Hold 8:**

  - TA: TBD

  - 10-12: 1-0-10

  - 15-17: 1-0-10

## Assignments

There are 6 assignment in total during the course with deadlines every
week. They overlap slightly to allow for more flexibility in your
scheduling, but think of them as weekly assignments.

The assignments will be graded with points from 0 to 4 and it is not
possible to re-hand-in any of the assignments.

Assignments are made to be solved in groups of up to three students.
We strongly encourage you not to work alone. Each group must make
their own solutions and cannot share implementations and report with
other. You may discuss material and ideas.

### General assignment rules

The following rules apply to all assignments. They are intended to
ease our correction process, and in particular to allow automated
testing. Consider the assignments to be a specification or API that
you are asked to implement.

1. Do not modify the types of any definitions in the handout, except
   when the assignment text explicitly instructs you to do so.

2. Do not rename or remove any definitions that are present in the
   handout, except when the assignment text explicitly instructs you
   to do so.

3. Do not remove anything from module export lists.

4. Do not rename modules or otherwise modify the file tree. (You may
   add new files if you wish, although it is rarely necessary.)

5. Your code should compile without warnings. (Do not achieve this by
   disabling warnings.)

6. When handing in, you must hand in a complete workable program
   (including unmodified files from the handout).

7. When handing in, do not include temporary build files (such as
   `dist-newstyle`), editor backup files, or various other computer
   detritus. Run e.g. the `tree` command and read the file listing and
   ponder for each file whether it is something it makes sense to hand
   to your TA. At a *minimum*, run `cabal clean` before handing in.

Violation of these rules will result in points deductions. If you
violate these rules at the exam, it will negatively influence your
grade.

## Exam

The exam will be a multi-day take-home exam held in one of the exam
weeks. It will strongly resemble the mandatory
assignments in content and form, with the following differences:

* It is strictly individual.

* It is roughly the size of two assignments.

* You receive only summative feedback (i.e., a grade).

### Qualification

To qualify for the exam you are required to achieve at least 50% of
the total number of points in the assignments (that is, 12 points at
minimum). You also need to get *at least* one point in each of the
assignments.

### If you qualified in a previous year

If you qualified for the exam in a previous year, then you are still
qualified. When we approach the exam, and we have to send in a list of
students who have qualified, we will post a message telling you to
contact us to inform us of this.

We still recommend you follow the course.

### Date

TBD
