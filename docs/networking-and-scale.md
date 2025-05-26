# How will it network and scale?

Two choices, described first in bullets and then in more depth. Both could be used simultaneously.

-Frequent sharing: Unlimited scale, uses more bandwidth but little local space, requires users make their public notes available online (Github and others will do the job free).

-Infrequent sharing: Keep a collaborator's entire set of public notes on your disk, occasionally updating it whole. Limited to maybe a few thousand prolific collaborators. Works even via USB without internet, good for espionage.

# The frequent sharing of individual notes option

In this solution you only fetch the individual files you need. A view of a topic is stored across multiple files, so plan to hit the other user's server once for every line in the file they want to view. (My data averages 14.5 nodes per note.)

Tis solution scales to any number of users, as could any individual user's ("social") network of subscriptions and links to other users' notes. If someone got really pepular, their fans might have to set up mirrors. (I say fans because time does not allow an internet-popular person to listen to everyone who listens to them -- although the Skg granular subscription model would allow them to listen better.)

I don't know how GitHub would feel about it, but poking a server for each new sentence to read (assuming people write with extreme [atomicity](https://notes.andymatuschak.org/Evergreen_notes_should_be_atomic)) seems acceptable to me.

# The infrequent sharing of whole collections of notes option

The [Agora federation](https://github.com/flancian/agora) uses a sharing model in which one chooses whose notes to duplicate locally -- on an author by author basis, not a note by note one. Agora polls very frequently (every minute? more?) for updates. They have on the order of a few hundred members.

The infrequent sharing model works even without internet access. Each person might be limited to sharing with around ten thousand collaborators, if each one's notes were 15 MB like mine are after five years of prolific note-taking. (That would use 150 GB of space.)
