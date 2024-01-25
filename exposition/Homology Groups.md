# Homology groups

As we mentioned earlier, homology groups are used to measure "holes" in a shape. It is also very easy to compute, both for a human and for a computer.

As an example, let's imagine an undirected graph. There will be many cycles in this graph, but how do we count them? Consider a graph shaped like "8". There are obviously two cycles in sight, one goes up and one goes down. But we can also combine these two to get a big cycle that first goes around the upper circle and then the lower circle. How do we express the intuition that all possible cycles can be formed by combining the two "basic cycles"?

(Figure 8 graph)

Here we have a graph with four edges. Let's say going from bottom to top on an edge is counted as positive, and the reverse negative. Then we can write the top cycle (clockwise) as $x - y$, because it goes along $a$ from bottom to top, and then follows $b$ from top to bottom. Similarly, the clockwise bottom cycle is $z - w$. Note that $-w + z$ and $z - w$ is the same thing, because of the usual properties of addition and subtraction. The clockwise big cycle would be $x - y - w + z$, which is exactly the sum of the two smaller cycles.

How exactly do we define a cycle? It should be something "without endpoints". The endpoints of $x$ is $A$ and $B$, but going from bottom to top, it leaves $B$ and goes into $A$. So it makes sense to make one positive and one negative. We define the total boundary (in the one-dimensional case, a boundary is just an endpoint) of $x$ to be $A - B$. Boundary is usually denoted by $\partial$ in mathematics, so $\partial x = A-B$. Since $y$ also starts and ends at the same place, we have $\partial y = A-B$, and therefore the total boundary of the cycle $x - y$ is zero. This shows why we used opposite signs for the two endpoints: If one edge goes into a point, and another edge leaves, then the boundary will cancel each other, and this point will not appear in the end result.

So our definition of cycles is just a combination of edges such that the boundary is zero. What cycles are there? There are four edges $x,y,z,w$, so the possible linear combinations are $px + qy + rz + sw$, where $p,q,r,s$ are integers. We can compute the boundary to be $(p + q) A + (r + s - p - q) B - (r + s) C$. Since the boundary needs to be zero, we have
$$\begin{aligned}
p + q &= 0 \\
p + q &= r + s \\
0 &= r + s
\end{aligned}$$
So basically we need $q = -p$ and $s = -r$, and all the cycles are of the form $$ p(x - y) + r(z - w) $$
where $p, r \in \mathbb Z$. Indeed, we see that all the cycles are formed by combinations of $x-y$ and $z-w$.

(Introduce faces and homology)

(infinitely many generators)
