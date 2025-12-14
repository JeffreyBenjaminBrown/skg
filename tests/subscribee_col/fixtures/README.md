# Test fixtures for SubscribeeCol functionality.

Two sources: 'home' (user-owned) and 'away' (foreign).

Structure:
  home/
    1         contains 11, 12, 13, 14
    11        contains 111, subscribes_to 11-sees
    111       (leaf)
    12        subscribes_to 12-sees
    13        (leaf, no subscriptions)
    14        contains 141
    141       (leaf)

  away/
    11-sees   (subscribee of 11)
    12-sees   (subscribee of 12)
    invisible (not referenced)

Expected content view from node 1:
  * 1
  ** 11
  *** subscribees     <-- SubscribeeCol because 11 subscribes to something
  *** 111
  ** 12
  *** subscribees     <-- SubscribeeCol because 12 subscribes to something
  ** 13               <-- no SubscribeeCol (no subscriptions)
  ** 14               <-- no SubscribeeCol (no subscriptions)
  *** 141
