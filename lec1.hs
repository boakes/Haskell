doubleMe x = x + x

doubleUs x y = y * x + 2 * y

doubleSmallNumber x = if x > 10
                      then x 
                      else x * 2

--Write a function minThree that takes three numbers and returns the minimum of them all.
--
--Write a function middle that takes three numbers and returns the middle one.
--
--Write minThree without any conditionals
--
--Write middle without using any conditions(ifs).
--
--write a function strangeFunc that takes three numbers, and if their sum is greater than 100, finds
--the minmum, otherwise finds the middle.
--For instance, StrangeFunc 10 20 30 = 20, while strangeFunc 10 20 80 = 10.
--