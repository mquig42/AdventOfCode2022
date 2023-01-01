# AdventOfCode2022
 
Solutions to Advent of Code 2022. I had some fun learning Racket last year, so I'll keep using it this year. I still haven't finished day 19 from last year, so my goal this year is to get all 50 stars.

Self-imposed rules:
1. Each day's puzzle input will be stored in a text file. The program must be able to read and parse this file.
2. When possible, use purely functional programming. Avoid mutable state and side effects.
3. No looking at the subreddit until after I've got both stars for the day.

Update 2023-01-01:
The difficulty spike hit HARD this year. Still, I managed to finish every puzzle. It's interesting to look at the completion times. Up to the 15th, there was only one day where I didn't finish the puzzle in 24 hours, and that was because I was at a family event. After the 15th I got half of the part 1s on the same day, but only 3 of the part 2s.

Here's how I did on the self-imposed rules.
1. Every program can parse the input file, though a couple of them are still tailored to my specific input and won't work on the example without modification. The main one there is day 22 part 2, where all the cube edges are hard-coded. That was also the hardest one to debug because of all the (literal) edge and corner cases.
2. I used global mutable data on days 7 and 12. Apart from that, the only side effects were reading files and displaying output, and that's inevitable.
3. Completely abandoned after the difficulty spike, since I usually wasn't solving the problem on the same day anymore. Sometimes I would use hints from the subreddit, like "y=mx+b" for day 21. Sometimes I would just enjoy the memes. Didn't want to miss out.

Day 17 was the last one I solved. Not because it was the hardest (that might be 16), but my part 2 solution was pretty unusual. No coding, except a function that turned each level of the stack into an integer. I just looked at the output and did some math. I thought I was going to need to modify the simulation to skip a bunch of time and then resume, but it turned out I could solve the whole thing with just simple math.
