# NFL-Field-Goal-Research

Some research on win probability from field goal attempts.

Code updated with 2019 data.

Link to original thread: https://twitter.com/ZachFeldman3/status/1174515609129967616

The definition of possessions is a little wordy. I defined the num_poss as the number of possessions a team is up or down. This just means it is binned by 8 points, with 0 being a tie game. The num_poss_change is the number of possessions changed from the play. Since a field goal is only 3 points, the only way to have a change of 2 is to go from losing to winning, as it crosses through 0 (which counts as a possession by this definition).
