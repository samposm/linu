# Linu

A Mathematica package for linearizing systems of differential equations. 
Sometimes you might just use the Jacobian for the whole system, but in some 
cases same functions occur several times in the Jacobian, so this more 
element-wise approach might come helpful. The end results are meant for humans 
to view, not for further use in Mathematica. Some commands show intermediate 
steps in the derivation, but Linmat1 shows the final linear system, and Linmat2 
also divides everything by Δt to make the result look a bit shorter.

![code1][fig1]
![code2][fig2]

[fig1]: https://github.com/samposm/linu/blob/master/fig1.png
[fig2]: https://github.com/samposm/linu/blob/master/fig2.png
