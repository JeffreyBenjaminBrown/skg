# How sharing works in skg

Each author or source you want access to should be downloaded to some local folder. Some of these notes will, presumably, be ones you wrote; others can be ones others wrote.

You can navigate and edit them as if you owned them all. When you "modify" someone else's original note, you actually create a clone of it, in your data rather than theirs. The clone is implemented as an ordinary note just like the original, with the same text and branches, and with a subscription to the original. You can change the text of the clone, rearrange its branches, discard them, scatter them across your other files.

All of the original content is considered "integrated" into the clone. Branches you then delete from the clone or move elsewhere become branches which the clone "hides_from_its_subscriptions". These hiding relationships are recorded explicitly in the filesystem upon saving, but there is also an implicit "hides_from_its_subscriptions" relationship from the clone to all of its normal content. (That's because any branch that shows up as ordinary integrated content does not need to also show up in the subscriptions.)

If, later, the other author adds content to their note, it will be visible from the clone, thanks to its subscription to the original, as "unintegrated subscribed content". You can if you wish proceed to incorporate that content into yours, too.

Because nodes in Skg are represented as flat files, this way of lensing onto foreign data duplicates very little of it. For instance, if the original had two branches when the clone was made, the clone would only need to `contain` two IDs. None of the descendents of those IDs need to be mentioned, unless and until one of them is also modified.
