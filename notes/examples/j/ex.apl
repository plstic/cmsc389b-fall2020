⍝ using the suggested extension:
⍝ backtick ` followed by a character results in an APL symbol
⍝ ex:
⍝ `, -> ⍝ (comment)
⍝ `a -> ⍺ (left-arg)
⍝ `w -> ⍵ (right-arg)
⍝ press undo (ctrl+z) after typing any backtick sequence to make the original appear

⍝ code time
⍝ do NOT change the function names, or you will not receive credit!

⍝ print out: Hello, World!
hello←{'Hello, World!'}

⍝ perform multiplication
mult←{⍺×⍵}

⍝ perform exponentiation (raise to power)
pow←{⍺*⍵}

⍝ print the ⍵th fibonacci number
fibonacci←{⍵≤0:0⋄⍵<3:1⋄∇(⍵-1)+∇(⍵-2)}

⍝ iterative fibonacci
⍝ A+.×B performs the matrix product AB
fibIter←{⍵≤0:0⋄(↑+.×/⍵⍴⊂2 2⍴1 1 1 0)[1;2]}

⍝ head of list
head←{⍵[1]}

⍝ tail of list
tail←{(|1-⍴⍵)⍴(1⌽⍵)}

⍝ write a dyadic function that "zips" together ⍺ and ⍵
zip←{⍺,⍵} ⍝ todo

⍝ mergesort
⍝  note ⍵[⍋⍵] sorts ⍵
merge←{0=⍴⍺:⍵ ⋄ 0=⍴⍵:⍺ ⋄ ⍺[1]<⍵[1]: ⍺[1],((tail ⍺)∇⍵) ⋄ ⍵[1], (⍺∇(tail ⍵))}
lower←{(⌊((⍴⍵)÷2))⍴⍵} ⍝ calculate the lower "half" of ⍵
upper←{(⌈((⍴⍵)÷2))⍴((⌊((⍴⍵)÷2))⌽⍵)} ⍝ calculate the upper "half" of ⍵
mergesort←{(⍴⍵)≤1:⍵⋄(∇lower⍵)merge(∇upper⍵)}
