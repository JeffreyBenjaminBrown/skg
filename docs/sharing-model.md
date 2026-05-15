# How sharing works in skg

Each author or source you want access to should be downloaded to some local folder. Some of these notes will, presumably, be ones you wrote; others can be ones others wrote.

You can navigate and edit them as if you owned them all. When you "modify" someone else's original note, you actually create a clone of it, in your data rather than theirs. The clone is implemented as an ordinary note just like the original, with the same text and branches, and with a subscription to the original. You can change the text of the clone, rearrange its branches, discard them, scatter them across your other files.

All of the original content is considered "integrated" into the clone. Branches you then delete from the clone or move elsewhere become branches which the clone "hides_from_its_subscriptions". These hiding relationships are recorded explicitly in the filesystem upon saving, but there is also an implicit "hides_from_its_subscriptions" relationship from the clone to all of its normal content. (That's because any branch that shows up as ordinary integrated content does not need to also show up in the subscriptions.)

If, later, the other author adds content to their note, it will be visible from the clone, thanks to its subscription to the original, as "unintegrated subscribed content". You can if you wish proceed to incorporate that content into yours, too.

Because nodes in Skg are represented as flat files, this way of forking foreign data duplicates very little of it. For instance, if the original had two branches when the clone was made, the clone would only need to `contain` two IDs. None of the descendents of those IDs need to be mentioned, unless and until one of them is also modified.

## Editing subscribed content in a view

The view-children of a subscribee-as-such can be edited (in limited ways) to control what the subscriber hides from the subscribee.

Let's call the subscriber R and the subscribee E.

If N is graph-content of E, and you delete N as a view-child of E, you have indicated to Skg that you would like to hide N from R's view of E. Skg obliges by creating a hides relationship from R to N. This does *not* change what E graph-contains; it just changes how R views E. (Remember, it's likely that E is not even something you own.)

If you later put that same child back directly under the subscribed node in the subscription view and save, the hide is removed.

This rule only applies to the subscribee's children. Edits to deeper descendants have the usual effect, subject to ownership.

If E does not contain N, and you make N a view-child of E, N will persist in your view but have no effect on E or R.

Title and body edits to E should be made from some view that does not present E as a subscriber. (C-c g RET from E will open such a view.) Skg rejects title/body edits to subscribees-as-such, regardless of who owns the subscribee.

The generated "hidden from this subscription" and "hidden from all subscriptions" collections are read-only displays. They show what R currently hides, but editing those collections is not how you hide or unhide subscribed content. To hide something, remove it from E's immediate visible graph-content in the subscription view. To unhide it, put it back there.
