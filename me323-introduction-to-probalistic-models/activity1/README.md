# Monty Hall problem

Simulation for the Monty Hall problem, a brain teaser in the form of a probability
puzzle.


The problem:

There are 3 doors, behind which are two goats and a car.
You pick a door (call it door A). You’re hoping for the car of course.
Monty Hall, the game show host, examines the other doors (B & C) and opens one
with a goat. (If both doors have goats, he picks randomly.)
Here’s the game: Do you stick with door A (original guess) or switch to the
unopened door? Does it matter?


Basic assumptions:

The host must always open a door that was not picked by the contestant;
The host must always open a door to reveal a goat and never the car;
The host must always offer the chance to switch between the originally chosen
door and the remaining closed door.


The problem is a paradox of the veridical type, because the correct choice (that
one should switch doors) is so counterintuitive it can seem absurd, but is
nevertheless demonstrably true.


The solution:

The solutions are: 1/3 chance of getting car if the player doesn't switch doors
                   2/3 chance of getting car if the player does switch doors

A way to think about this problem is as follows: suppose the player chooses a door.
the probability of the car being in that door is 1/3, while the probability of the
car being in the other 2 doors is 2/3. The host then eliminates 1 door out of the
2 which were not picked, so it is as if the 2/3 probability concentrates in the
one door which the host didn't pick.

Another way to approach it is to list all the possible outcomes if the player
switches doors (Of course, if the player doesn't switch doors, the chance of
winning the car is 1/3):

Scenario 1: Player picks the car. Host shows a goat. Player switches to the other
goat. Player loses.
Scenario 2: Player picks goat 1. Host shows the other goat. Player switches to
the car. Player wins.
Scenario 3: Player picks goat 2. Host shows the other goat. Player switches to
the car. Player wins.

So, the player wins by switching if he picked a goat the first time around, which
has a chance of 2/3.
