Some terms and abbreviations used in this code.

# Terms and abbreviations specific to Skg

## "buffer", or sometimes "forest"

Buffer is a term from Emacs.
Each view onto the graph is represented via a separate buffer.
Each buffer is a forest of "viewnodes".

## "child" and "parent"

The TypeDB graph contains a number of relationships -- 'contains', 'textlinks to', etc. Where in this code the terms "child" or "parent" are used, it refers to none of those TypeDB relations, but rather the relationship between headlines in an Emacs org mode buffer. (I have tried to stick to the more precise "org-child" and "org-parent".)

The reason for this is that the map from the child-parent relationship in an Emacs buffer to the corresponding relationship, if any, in TypeDB depends on context. Usually, if an org headline P has a child headline C, they will correspond to a TypeDB node Outer that 'contains' a TypeDB node Inner, where Outer corresponds to P and Inner to C. But that does not always hold: see, for instance, the discussion of alias nodes in [the architecture documentation](coding-advice/architecture.md).

## "col" is short for "collection"

in some type definitions or enum varieties.

## ephem = ephemeral

Some data regarding a node is ephemeral --
it lasts only as long as the view of it in some buffer.

Non-ephemeral data, by contrast,
remains on disk when the app terminates,
and can be recovered by a later session.

## fork : can be any number > 1

Some people intpret a fork as a binary choice.
In Skg a fork can split into any number.

## hiderel = "hides" relationship

See schema.tql for what a "hides" relationship is.

## interp = interpretation

## kv-pair = key-value pair

## lp = length-prefixed

Sometimes an API response is prefixed with a length, letting the receiver know how many characters to expect in the response.

## An 'm' prefix

In Haskell, 'm' is sometimes prefixed to a word to indicate 'maybe'.
The Rust term for Maybe is Optional, but I've used the Haskell idiom.
For any function with a name like this, if its meaning is unclear,
the type signature should clarify what's going on.

## md = Metadata

Most org nodes are prefixed with some metadata.
In Emacs, that metadata is represented as an s-expression like
(skg (key value) bare_value ...),
with any number of key-value and bare-value pairs in any order.
The metadata is used for Rust to know what to do with it,
and for Emacs to let the user know how to interpret it.

## mk = make / create

## "subscribee as such"

In Skg some nodes are "subscribers", which "subscribe" to "subscribees".
(See [the schema](./schema.tql).)
A node that plays the 'subscribee' role can be viewed as an ordinary node,
or *as* a subscribee. In the latter case it appears
underneath the relevant subscriber, in a 'subscribeeCol':
```
  * subscriber
  ** subscribeeCol
  *** subsribee1
  *** subsribee2
  ...
```

In the above view, subscribee1 and subscribee2 are both "subscribees as such":
They are not just subscribees, but they are being shown *as* subscribees.
This changes the way edits are interpreted:
Edits to a "subscribee as such" can only affect
what the relevant subscriber hides, nothing else.

## read-only set

A read-only set is a generated collection whose membership follows
from graph facts rather than direct edits to that collection.  A view
may preserve an order for the set locally, but adding or removing an
item in the generated collection does not add or remove the underlying
graph membership fact.  If a user labels a nonmember inside such a set,
the save treats that item as independent of the generated membership.
If completion discovers that a real member is missing from the view, it
restores that member.

For instance, if nodes R and T subscribe to nodes N, the user might land
at a view like the following:
```
  * N
  ** subscriberCol
  *** R
  *** T
```
The subscriberCol ("collection of subscribees") is a read-only set.
Thus the user could flip the order of R and T in the subscribeeCol,
and this change would be respected in the display, but have no effect
in the graph. The user could insert X under subscriberCol,
but because it is a read-only set, X will have its parentIs field
rewritten to 'Independent', and have no effect on N.
(If the user wants X to subscribe to N, they must modify X, not N.)
The user could delete R, but it will respawn as soon as they save,
because, again, the subscriberCol is a read-only set.

## title != headline

Headline is a term from org-mode. It refers to a line that begins with some asterisks, followed by at least one whitespace, followed by at least one non-whitespace. The text after the first chunk of adjacent whitespace is what I'm calling the 'title'.

# Terms specific to Tantivy

## Document

A Tantivy association. In my case, from a title or alias to an ID.
(Each ID can have multiple such associations.)
