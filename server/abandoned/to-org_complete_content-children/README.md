# Why I buried this

This completes a definitive org branch,
fetching children from disk and making sure they are all present.
I don't know what I was thinking when I wrote it,
because they *must* all be present,
since it was only being used in the save-buffer path,
after saving changes to disk.
Whatever the definitive node had as branches
would have been saved as content.
