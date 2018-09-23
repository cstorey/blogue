---
date: '2018-05-31'
orig_url: "https://tech.labs.oliverwyman.com/blog/2018/06/01/solving-zelda-puzzles-satisfactorily/"
title: "Solving Zelda Puzzles Satisfactorily"
description: 'The other kind of laziness'
---

In the game Breath of the Wild, there's a puzzle which involves a set of fans and turbines in a 4x5 grid, and you must position the fans in order ensure all of the turbines are spinning. Unfortunately, I've never really had much patience for solving this kind of logic puzzle the old fashioned way, so given that computers are good at this sort of thing, I thought I'd try that.<!--more-->

Now, we can express this problem as a boolean algebra problem. Given that we have some set of variables and some constraints (ie: the fan positions and directions), how do we ensure all of these variables are set to true?

Happily, this kind of problem can be given to a [Satisfiability solver](https://en.wikipedia.org/wiki/Boolean_satisfiability_problem) to solve. The only remaining question is ... how do we teach it to solve the problem?

So, we know that in our problem, each fan can only ever be facing in one direction at once.

![Puzzle layout](/images/2018-05-31-solving-zelda-puzzles-satisfactorily/Puzzle Layout.svg)

So that leaves us with two problems: Ensuring that each fan only blows in a single direction, and ensuring that all turbines are propelled by some fan.

We know that all of the turbines on the line from the fan in the direction of air flow will be spinning. So, if we have a trivial 1x2 grid, with a Fan in (0, 0) and a turbine at (1, 0); we can intuit that the turbine will only be spinning if the fan is facing east, ie: the wind flows with a direction of (1, 0).

So, given that we know the grid size and fan locations ahead of time, we can notate this problem:


![two cell layout](/images/2018-05-31-solving-zelda-puzzles-satisfactorily/two-cells-layout.svg)
as follows:

$$ Fan(0, 0, E) \vdash Turbine(1, 0) $$

ie: A fan at `0, 0` will propel the turbine at `1, 0`.
Now, in a setup with two fans, say another one at `(3, 0)`:

![four cell layout](/images/2018-05-31-solving-zelda-puzzles-satisfactorily/four-cells-layout.svg)


Can be expressed as:

$$ Fan(0, 0, E) \vdash Turbine(1, 0) \land Turbine(3, 0) $$
$$ Fan(2, 0, E) \vdash Turbine(3, 0) $$
$$ Fan(2, 0, W) \vdash Turbine(1, 0) $$

(the ordering of the turbines here doesn't particularly matter; it's mostly included for convenience).

Now, this is fine, but there's one small hitch; our solver will only accept input in Conjunective Normal Form (CNF for short). so, we can say that "(A ∨ ¬B) ∧ (A ∨ B)", for example. However, because we know that all of the turbines have to be spinning, we don't need to explicitly denote them in the description we pass to the solver (ie: we don't need to create variables for them), we can simply assert the conditions that make them up. In this case, this would be a union of the fans that blowing over that given position.

```
# Turbine(1, 0)
Fan(0, 0, E) ∨ Fan(2, 0, W)
# Turbine(3, 0)
Fan(0, 0, E) ∨ Fan(2, 0, E)
...
```

And so on. as you can tell, this can get quite tedious to do by hand. Happily, we can make the computer do the boring bits for us. I've used the library [pycryptosat](https://pypi.org/project/pycryptosat/) to do this. I'm not going to include snippets here, since the code I wrote is hardly exemplary, but can be found [in a gist](https://gist.github.com/cstorey/05e94f825362defd5a04a1699322ca5e).

The other part of the problem is making sure that each fan only points in a single direction. We can say that a solution is invalid iff a fan is pointing, say, North and East at once. The python that does this is as follows:

```python
class Fan(object):
    def __init__(self, name, solver):
        self.name = name
        fvar = next_free_var(solver)
	# North, East, South and West are just offsets into our list of variables.
        self.nesw = range(fvar, fvar+4)

	# Then for each pair of directions, assert that they cannot both be set.
	# We assert that ¬(d0 ∧ d1); which via De-Morgan's laws, can be written as:
	# (¬d0 ∨ ¬d1).
	# This commutes, so (¬d0 ∨ ¬d1) is the same as (¬d1 ∨ ¬d0), and we can avoid
	# adding it in twice.
        clauses = [[-d0, -d1] for d0 in self.nesw for d1 in self.nesw  if d0 > d1]
        #print clauses
        solver.add_clauses(clauses)

    def show(self, solution):
        return "{}:{}".format(
            self.name, ",".join(d for v, d in zip(self.nesw, "NESW") if solution[v]))

    def __repr__(self):
        return "<Fan: {};{}>".format(self.name, self.nesw)
```

So when we initialize a Fan object, we pass it something to identify it (a tuple of coordinates, in this case), create some fresh variables in the solver for each direction, and then assert to the solver that the fan will only ever spin in one direction.

We then ask the solver for a set of solutions. Effectively, the solver will only return one solution at a but we then add a clause that will mean that the previous solution isn't valid any more. And thus when the solutions are exhausted, the solver will say that the original problem excluding the previously given answers is unsatisfiable.

The solver in use will output an assignment of all variables to either `true` or `false`. The `Fan#show` method above can then lookup how each variable for each direction has been assigned, and display the directions that are set to `true`. The reason output all of the directions that match is as an internal consistency check. In this case, it's easy enough to see when a fan is either not blowing in any direction, or multiple directions (both quite impossible).

So, we can chuck together an [IPython Notebook](https://gist.github.com/cstorey-owl/d98f24cc17ef0fdaeacb977976cc8789) to glue everything together, and we find it will output a single solution. Checking this by hand, we find that all of the turbines are covered by wind from a fan, thus solving the shrine puzzle, and granting access to the Monk and their spirit Orb.
