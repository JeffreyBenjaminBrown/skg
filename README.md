# what this app would do

Skg is an app for collecting, creating and sharing information. I believe it offers the best solution yet to the problem of sharing in a world of scarce attention overflowing with information.

## the data you could control

This "data model" is thousands of years old -- ordinary books with citations use it. But citations are slow. Apps let you create and follow links much faster. Some popular apps that do this today include org-roam, Roam, Notion, Obsidian, OneNote, and Tana; I think there are literally dozens of others.

Some people use different terms for the same things. I have tried to use the most common ones. Each user's data will consist of a collection of "documents" (or "notes"), each of which  is a `tree` with `hyperlinks`. That is, each document is just ordinary text, which can be divided into sections, subsections, sub-subsections, etc., and in which there can be hyperlinks.

That's all been standard for years. But unlike previous technologies, Skg will let you split, merge and rearrange anyone's notes, safely mixing your public and private notes with anything they have shared with you, without needing ownership. When you incorporate someone else's notes into yours, you can (and by default will) receive updates if they change things. Skg will also let you learn more about your own notes from how others incorporate yours into theirs (if they share that they did, and if you want to know).

# adressing some common fears

## it's free

Skg is free and open-source (see the [[license](LICENSE.md)]. No cloud tyrant needs to control your data. But that said, sharing data through some big cloud provider will be convenient.

## anonymity and lurking are both fine

A timid or antisocial user will be free to use whatever work others have shared while keeping their own completely private.

## "public" and "private" are just two points on a spectrum.

There will be more kinds of sharing. A user can define cliques for selective sharing. (But in this documentation I'll avoid talking about selective sharing, because the words public and private get the idea across.)

# why we need an app like this

## Writing is powerful.

Journaling, reporting, and taking notes have all had tremendous impacts on history. Countless thinkers have made powerful cases (sometimes through example) for taking notes as a tool for understanding the world and oneself. They include James Baldwin, Bertrand Russel, Marcus Aurelius, Joan Didion, Richard Feynman, George Orwell, Michel de Montaigne, Charles Darwin ...

### Note: Knowledge gardening is actually more than writing.

The authors mentioned earlier, when they describe the value of "writing", are really describing the value of knowledge gardening, a more recent term. Knowledge gardening includes:

- writing
- reading (your own or others' work)
- thinking and meditating (already a common gardening metaphor)
- even surfing the internet (esp. while taking notes)

Knowledge gardening differs from, say, much of social media, in that it is the repeated processing of persistent bodies of thought. It is not an ephemeral stream; it is the construction of something permanent.

(But I might keep using the less precise term "writing", out of habit.)

## Sharing is powerful.

Anybody who has shared notes in a good student group has felt the benefits that even a tiny number of people focused on a tiny sliver of reality can achieve. Anyone familiar with how science, academia, law or government works knows about the power of larger groups cooperating to build knowledge.

If you are unfamiliar with those processes, but you know how helpful taking notes has been for your own understanding, just imagine doing that with good collaborators. (Yes, your collaborators will be good, because you control your view of others' work. You won't have to see contributions you don't care about.)

## Skg solves the sharing problem.

To explain why sharing data is a hard problem, and to argue that nothing before Skg has solved it, I have to first make explicit the data model you are probably used to.

In the usual sharing model, some people can see and change a document, and others can only see it. Those who can only see it cannot contribute, which deprives the entire community of potential knowledge. Those who can also change the document might disagree, and those conflicts, too, lead to the loss of ideas and perspectives.

By contrast, the data model presented here permits "competing" frameworks to coexist and cooperate, without getting in each others' way.

# The Skg experience

## First some vocabulary

Sorry, I have to do it. Fortunately these are common ideas.

As mentioned earlier, each document is a `tree` with `hyperlinks`. In more detail: Each spot in the tree is a `node`, and the relationship connects nodes in the tree is called `containment`. Each tree has exactly one special node called the `root`. The root might contain other nodes, but nothing contains the root. Each node in the tree has a `title`, which is ordinary text with hyperlinks. Some nodes contain other nodes (which is sometimes called "having branches"). A node with no contents is called a `leaf` (because leaves are where branches stop branching).

A node's `descendents` include its branches, and its branches' branches, etc.

Notice that "branch" and "node" are almost the same thing. A branch is just a node together with all its descendents. The difference can often be ignored.

Each branch and each sub-branch of a tree is itself a tree -- even the leaves.

## Hyperlinks point to nodes (or equivalently, to branches)

A hyperlink can lead be to an entire document, or to a spot within a document.

For example, consider a document called `eggs` like this:
```
  * eggs
  ** health
  *** protein
  *** cholesterol
  ** recipes
  ** mystical
  *** Which came first, the chicken or the egg?
```
The document branches into three parts: `health`, `recipes` and `mystical`. A hyperlink could be to the whole document, or to the `eggs / health` branch, or the `eggs / health / protein` sub-branch, etc. (The last one would probably be about the protein in eggs, but the app does not constrain how a user should interpret paths such as `eggs / health / protein`.)

## Trees can be rearranged -- even if they aren't yours.

When you read another's notes, you can choose to incorporate any branch from their data into any node of yours.

### A detailed example

Suppose, for instance, the `eggs` document above belongs to your friend Egg Man. And suppose this document, `mysticism`, is yours:
```
  * mysticism
  ** imagination
  ** God
  ** common objects with mystical associations
  *** black cats
  *** the moon
```

You might choose to move the `mystical` branch from `eggs` into your `mysticism` document, under `common objects with mystical associations`. I will not call the new node a "copy", but rather a `clone` of `mystical` from `eggs`. Your document after incorporating the clone would look like this:
```
  * mysticism
  ** imagination
  ** God
  ** common objects with mystical associations
  *** black cats
  *** the moon
  *** eggs / mystical -- CLONED from Egg Man's "eggs"
  **** Which came first, the chicken or the egg?
```
(Notice that the title of the root of the new branch, `eggs / mystical`, joins two separate titles found in `eggs`. That might be the default, but you can also choose some other title instead. The point is that `eggs / mystical` in your `mysticsm` comes from `mystical` in Egg Man's `eggs`.)

Suppose Egg Man then extends the original `eggs` note, so it looks like this:
```
  * eggs
  ** health
  ** recipes
  ** mystical
  *** Which came first, the chicken or the egg?
  *** Egg limpia (curanderismo) -- NEW
  *** The philosophical egg     -- NEW
```

Your clone in "mysticsm" of the original "eggs / mystical" is by default `subscribed` to the original. While it is subscribed (which you can change), after Egg Man adds the new nodes, when you look at your "mysticism" you might see the following:
```
  * mysticism
  ** imagination
  ** God
  ** common objects with mystical associations
  *** black cats
  *** the moon
  *** eggs / mystical -- CLONED from Egg Man's "eggs"
  **** Which came first, the chicken or the egg?
  **** << unincorporated subscribed content from Egg Man's document "eggs" >>
  ***** Egg limpia (curanderismo)
  ***** The philosophical egg
```
The node `<< unincorporated subscribed content from Egg Man's document "eggs" >>` was automatically generated, based on the subscription from your node titled `eggs / mystical` to the node titled `mystical` in Egg Man's document `eggs`.

Since [[branches can be folded](./docs/tree-folding-is-powerful.md)], even if Egg Man has added hundreds of new nodes, the automatically-generated branch titled `<< unincorporated subscribed content from Egg Man's document "eggs" >>` does not have to clutter your view of `mysticism`.

You can of course unsubscribe from Egg Man's changes to that branch if you want, or even unsubscribe from Egg Man's writings entirely.

### What the example demonstrates

You and Egg Man can thus focus on exactly what you care about. Your "changes" to Egg Man's data don't have to affect Egg Man's experience at all. However, if you make public the fact that you incorporated Egg Man's note into yours, and Egg Man has chosen to see such information, then an indicator in Egg Man's view of `eggs` will show that his `mystical` branch is part of a document owned by you. If Egg Man is curious, he can follow that indicator to discover your `mysticism` document.

# That's the motivation.

The rest of this document is technical.

# data model

See [[the schema](schema.tql)], [[the content model](docs/content-model.md)] and [[the sharing model](docs/sharing-model.md)].

# how to contribute

You don't have to want to work on my plans (below), although that'd be awesome. If you've got ideas you want to implement, I'm interested.

# what's done and what's left to do

## what's done

This code so far includes a Rust server and an Emacs client. The server can:

- read and write to files in a format called .skg (it's just .yaml, but not all .yaml is .skg)
- build, modify and consult a model of the data in TypeDB
- build, by consulting TypeDB, a document to send Emacs

The client can:

- Receive that document.
- Let the user edit it.
- Let the user send it back.

## what's left to do

Most immediately, Rust doesn't yet know what to do with the document when Emacs sends it back. (It should save the data to disk, update TypeDB and Tantivy, and silently replace the Emacs document with a new one with IDs where the user had created new headings without IDs.) This is sketched in a little more detail in [[TODO.org](./devel-handy/TODO.org)].

For that plan and more in greater detail (plus architectural observations, conceptual problems, and more), see [[my notes in org-roam on Skg](https://github.com/JeffreyBenjaminBrown/public_notes_with_github-navigable_links/blob/main/shareable_knowledge_gardens.org)]

# usage: how to run it

Two alternatives:

## Install nothing. Run it in a tailor-made Docker container.

Find that container at https://github.com/JeffreyBenjaminBrown/docker-typedb-rust and build it.

Start it (using something like the
`docker run` command in `docker.sh`).

Enter it (using something like the
`docker exec` command in `docker.sh`).

Run `cd` to go to the home folder (`/home/ubuntu`).

Then continue below, at
"If you've already got Rust and TypeDB", below.

## Without Docker, if you've already got Rust and TypeDB

First start the TypeDB server
(by typing just that: `typedb server`).

In a separate shell, run `cargo run`.
This starts the Rust server.

Now visit [[this Emacs lisp program](elisp/client.el)]
and read the top comment for further instructions.
