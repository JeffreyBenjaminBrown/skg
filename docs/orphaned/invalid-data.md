# What ensures a document is a tree (i.e. has no cycles)? What prevents cycles or infinite trees?
Nothing. Skg can't control what data the user creates. But it can warn them, and it can provide finite views of infinite data (see the `repeated` field of the `OrgNode` type for how). So you can't get in trouble.

If the user tries to view non-tree data, ambiguities may arise regarding the context in which to show a node. For instance, suppose the user requests to see A, but both B and C contain A. In this case the application will simply show the options and let the user choose the context to view A from. It will also display a warning, suggesting the user replace at least one of the two competing `contains` relationships with a textlink -- but they don't have to.
