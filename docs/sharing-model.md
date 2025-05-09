# How sharing works in skg

Your config file should list all the repos of notes you want to use. Each repo should be downloaded somewhere locally. Some of these notes will, presumably, be ones you wrote. Others can be ones others wrote.

The weird thing is, you can navigate and edit them as if you owned them all. When you "modify" someone else's note N, you actually create a lens onto their note. You don't need to change N. In your view you can change the text content of N, and reorder the node content of N, and add to that node content, and discard some of it.

As soon as you do any of those, you create a duplicate note N', with all of the content of N. The nodes that you remove from N become ones which your N' "hides_in_subscriptions", and N becomes a node to which N' is "subscribed". If, later, more content appears in N, this will be visible to you from N', as "unincorporated subscribed content", which you can if you wish proceed to incorporate into your own graph.

Because nodes in skg are represented as flat files, this kind of lensing duplicates very little data. For instance, if N consisted of two big branches, then the file representing N would only contain two nodes. If you changed their order, you would have to duplicate N (with its text content and its two nodes), but you would not have to duplicate anything inside either branch.
