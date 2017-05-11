We want to again thank the reviewers and our shepherd for their
comments and criticism. As requested, we have made a series of changes
to our paper.

In order to more clearly explain the core compiler passes, we have
included extra explanatory prose, and we have threaded a single
running example through the paper, showing the incremental effects of
each pass on the same simple program. We have also added further
explanation to our existing example code, which the reviewers
previously found confusing.

We have expanded the section describing the intermediary language and
its type system.  As the shephard noted, several reviewers were
confused about the details of the type system and what it was meant to
guarantee. After reviewing section 4 of our paper, we agree that the
presentation of the type system had serious shortcomings, and it has
been completely overhauled.  Through a combination of clearer
notation, more thorough examples, and additional explanatory prose, as
well as a demonstration of the types as used in the running example,
we hope to have cleared up any confusion relating to the intermediate
language.

The reviewers also brought up some shortcomings of our paper's
evaluation. We added a full explanation of the machine we ran
benchmark tests on, as well as the C compiler version and flags. Also,
we added a discussion of the performance of programs that illustrate
weaknesses in our compilation approach.

A discussion of Lattner and Adve's work, as well as the three
citations mentioned by the shepherd, have been added. A proper
citation for session types has also been added, and included in
the expanded discussion of our type system.

We hope that we have addressed the main concerns you have had
with our paper. 