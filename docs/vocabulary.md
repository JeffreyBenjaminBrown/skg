# Trees and hyperlinks: Some simple but important definitions
Each document is a `tree` with `hyperlinks`. All that means is the document consists of ordinary text with hyperlinks, plus the ability (this is the "tree" part) to be divided into sections, subsections, sub-subsections, etc. The divisions don't have to be balanced -- some branches can be much deeper than others.

Each spot in the tree is a `node`. Each node has some text, which can have hyperlinks. Every hyperlink has a `source`, which is where you find it, and a `target`, which is where the link takes you. The source is some text you can click on, and the target is a node somewhere in a tree. The source and the target can be in the same tree, or not.

Each tree has exactly one special node called the `root`, which can contain other nodes, but which nothing contains. The relationship that connects nodes in the tree is called `containment`. When a node contains other nodes, we can say it `branches`. A node that contains no nodes is called a `leaf` (branching stops at leaves). A node's `descendents` include its branches, its branches' branches, etc.

"Branch" and "node" mean almost the same thing. A branch is just a node together with all its descendents. The difference can often be ignored. For instance, we can say that a hyperlink takes you to a node or to a branch.

Each branch and each sub-branch of a tree -- even each leaf-- is itself a tree.

A collection of documents is called a `graph`. (Skg stands for `shared knowledge graph`.)
