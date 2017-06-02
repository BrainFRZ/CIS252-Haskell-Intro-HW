-- Name:     Terry Weiss (466751950)
-- Email:    ttweiss@syr.edu
-- Section:  M013 LSB-105
-- Project:  Homework 9

import Turing


-----------------------------------------------------------
--
-- Counts the number of "dggd" sequences in a string
--
-- Initial input: a possibly empty string of d's and g's
-- Result: A unary count of the number of times the sequence
--         "dggd" appears in the string, included directly
--         to the right of the original string.
--
--  Example: 
--        Initial: [d]dggddddggdggdggggd
--          Final: ddggddddggdggdggggd[1]11 
-----------------------------------------------------------

countDGGD :: Prog
countDGGD = [
              (("scanD1",'g'), ('g',Rght,"scanD1")),
              (("scanD1",'d'), ('d',Rght,"scanG1")),
              -- If no d's (including if empty), will halt at end space

              (("scanG1",'d'), ('d',Rght,"scanG1")),
              (("scanG1",'g'), ('g',Rght,"scanG2")),

              (("scanG2",'d'), ('d',Rght,"scanG1")),
              (("scanG2",'g'), ('g',Rght,"scanD2")),

              (("scanD2",'g'), ('g',Rght,"scanD1")),
              (("scanD2",'d'), ('D',Rght,"tally")),

              (("tally", 'd'), ('d',Rght,"tally")),
              (("tally", 'g'), ('g',Rght,"tally")),
              (("tally", '1'), ('1',Rght,"tally")),
              (("tally", ' '), ('1',Lft, "back")),

              (("back",  '1'), ('1',Lft, "back")),
              (("back",  'd'), ('d',Lft, "back")),
              (("back",  'g'), ('g',Lft, "back")),
              (("back",  'D'), ('d',Rght,"scanG1"))
          ]


-----------------------------------------------------------
--
-- Reverses a string of m's and q's
--
-- Initial input: a possibly empty string of m's and g's
-- Result: The original string in reverse
--
--  Example: 
--        Initial: [m]qqqmqmm
--          Final: mmqmqqq[m]
-----------------------------------------------------------

rev :: Prog
rev = [
        (("read",  'x'), ('x',Rght,"read")),
        (("read",  'm'), ('x',Lft, "writeM")),
        (("read",  'q'), ('x',Lft, "writeQ")),
        (("read",  ' '), (' ',Lft, "clean")),
        
        (("writeM",'m'), ('m',Lft, "writeM")),
        (("writeM",'q'), ('q',Lft, "writeM")),
        (("writeM",'x'), ('x',Lft, "writeM")),
        (("writeM",' '), ('m',Rght,"scroll")),

        (("writeQ",'m'), ('m',Lft, "writeQ")),
        (("writeQ",'q'), ('q',Lft, "writeQ")),
        (("writeQ",'x'), ('x',Lft, "writeQ")),
        (("writeQ",' '), ('q',Rght,"scroll")),

        (("scroll",'m'), ('m',Rght,"scroll")),
        (("scroll",'q'), ('q',Rght,"scroll")),
        (("scroll",'x'), ('x',Rght,"read")),

        (("clean", 'x'), (' ',Lft, "clean"))
      ]

-----------------------------------------------------------
--
-- Reverses a string of m's and q's
--
-- Initial input: a possibly empty string of m's and g's
-- Result: The original string in reverse
--
--  Example: 
--        Initial: [m]qqqmqmm
--          Final: mmqmqqq[m]
-----------------------------------------------------------

