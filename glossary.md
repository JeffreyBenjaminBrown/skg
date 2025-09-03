# Some terms and abbreviations used in the code

## lp = length-prefixed

Sometimes an API response is prefixed with a length, letting the receiver know how many characters to expect in the response.

## title != headline

Headline is a term from org-mode. It refers to a line that begins with some asterisks, followed by at least one whitespace, followed by at least one non-whitespace. The text after the first chunk of adjacent whitespace is what I'm calling the 'title'.

## "child" and "parent"

The TypeDB graph contains a nummber of relationships -- 'contains', 'hyperlinks to', etc. Where in this code the terms "child" or "parent" are used, it refers to none of those TypeDB relations, but rather the relatinoship between headlines in an Emacs org mode buffer. (I have tried to stick to the more precise "org-child" and "org-parent".)

The reason for this is that the map from the child-parent relationship in an Emacs buffer to the corresponding relationship, if any, in TypeDB depends on context. Usually, if an org headline P has a child headline C, they will correspond to a TypeDB node Outer that 'contains' a TypeDB node Inner, where Outer corresponds to P and Inner to C. But that does not always hold: see, for instance, the discussion of alias nodes in [the architecture documentation](coding-advice/architecture.org).
