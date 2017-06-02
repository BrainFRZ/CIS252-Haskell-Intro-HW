-- Name:     Terry Weiss (466751950)
-- Email:    ttweiss@syr.edu
-- Section:  M013 LSB-105
-- Project:  Homework 9

import Turing


-- State naming note: I noticed the tabulation frequently got messed up with
-- state names of different lengths, so I've right-padded the names in the code,
-- but not in my descriptions. Also, to easily identify final states, each final
-- state is capitalized.


-----------------------------------------------------------
--
-- countDGGD
--     Counts the number of "dggd" sequences in a string
--
-- Initial input: a possibly empty string of d's and g's
-- Result: A unary count of the number of times the sequence
--         "dggd" appears in the string, included directly
--         to the right of the original string.
--
-- Consider d1 to be the next instance of the first d in the next sequence, g1
-- to be the first g in the current sequence, g2 to be the second g in the current
-- sequence, and d2 to be the last d and final character in the current sequence.
-- 
-- Initially, the tape head begins at the left of the input string, so we can
-- start testing for the sequence immediately ("ScanD1"). If the first cell is
-- already a 'd', we look for g1 ("ScanG1"), otherwise we keep scanning for d1.
--
-- We then continue scanning ("ScanG2", "ScanD2") until either we finish the
-- sequence or run into an incorrect character (c). If c is 'g', we start
-- looking for d1 again, but if it's 'd', then we look for g1 because that
-- incorrect 'd' is the beginning of the next potential sequence. If any of the
-- scanning states reach a blank cell, there are no more characters, so no more
-- sequences, so the machine halts.
--
-- Otherwise, once a sequence is found, we mark our spot with a 'D', scan to the
-- right-most blank and replace it with a unary increment ("tally"), then scan
-- back to our place holder ("back"). Once we're back, we fix 'D' back to 'd' and
-- look for g1 again, treating this 'd' as the beginning of the next potential
-- sequence.
-----------------------------------------------------------

countDGGD :: Prog
countDGGD = [
              (("ScanD1",'g'), ('g',Rght,"ScanD1")),
              (("ScanD1",'d'), ('d',Rght,"ScanG1")),

              (("ScanG1",'d'), ('d',Rght,"ScanG1")),
              (("ScanG1",'g'), ('g',Rght,"ScanG2")),

              (("ScanG2",'d'), ('d',Rght,"ScanG1")),
              (("ScanG2",'g'), ('g',Rght,"ScanD2")),

              (("ScanD2",'g'), ('g',Rght,"ScanD1")),
              (("ScanD2",'d'), ('D',Rght,"tally ")),

              (("tally ",'d'), ('d',Rght,"tally ")),
              (("tally ",'g'), ('g',Rght,"tally ")),
              (("tally ",'1'), ('1',Rght,"tally ")),
              (("tally ",' '), ('1',Lft, "back  ")),

              (("back  ",'1'), ('1',Lft, "back  ")),
              (("back  ",'d'), ('d',Lft, "back  ")),
              (("back  ",'g'), ('g',Lft, "back  ")),
              (("back  ",'D'), ('d',Rght,"ScanG1"))
          ]


-----------------------------------------------------------
--
-- rev
--     Reverses a string of m's and q's
--
-- Initial input: a possibly empty string of m's and g's
-- Result: The original string in reverse
--
-- Initially, the tape head begins at the left of the input string, so we begin
-- by reading a character and marking it as read with an 'x' ("read"), then
-- scroll to copy the character in the closest blank cell to the left ("writeM",
-- "writeQ"). This writes the string in reverse by creating a mirror image of the
-- origial string on the other side of the markers.
--
-- We then continue scrolling to the right until we reach our markers ("scroll").
-- At this point, we immediately switch back to reading for the next character.
-- We know the first character after the markers will either be valid or blank.
-- If it's valid, copy that one the same way.
--
-- Otherwise, we've finished copying the string in reverse and can clean up all
-- the marks ("clean").
-----------------------------------------------------------

rev :: Prog
rev = [
        (("read  ",'x'), ('x',Rght,"read  ")),
        (("read  ",'m'), ('x',Lft, "writeM")),
        (("read  ",'q'), ('x',Lft, "writeQ")),
        (("read  ",' '), (' ',Lft, "Clean ")),

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
        (("scroll",'x'), ('x',Rght,"read  ")),

        (("Clean ",'x'), (' ',Lft, "Clean "))
      ]


-----------------------------------------------------------
--
-- more4's
--     Says whether there are more 4's than 3's or 5's in a string "[345]*".
--
-- Initial input: a possibly empty string of 3's, 4's and 5's
-- Result: "Y" if more 4's than 3's or 5's together, or else "N".
--
-- Initially, the tape head begins at the left of the input string. The method
-- of my solution is to drop every pair of ('4', '[35]'), where '[35]' is an
-- instance of the char class containing '3' or '5', in the original string.
-- This way, after every complete operation, the difference between the instances
-- of each symbol is maintained.
--
-- First, we determine whether the first symbol is '4' or '[35]' and drop it
-- ("start"), then move to the state that will drop its opposite in the pair
-- ("dropY" if the symbol was '[35]', or "dropN" if '4').
--
-- When dropping a character ("dropY", "dropN"), we scroll right until we've
-- found a '4' or '[35]' respectively, leaving every other symbol alone. We
-- will either (1) find it or (2) we won't.
--
--     (1) If we find one, we mark it with an 'x', because a space would make
--         the string more difficult to scan. Then, once we've marked it, we
--         scroll back to the left-most character and start all over ("back").
--
--         Once we've reached the first space, we switch start over ("start").
--         This time, if the left-most character is an 'x', we drop all x's
--         because the 'x' denotes marking them for deletion. This also improves
--         runtime by reducing the number of unnecessary unhelpful comparisons.
--         If we've reached an empty string (including originally), it means the
--         drops of each symbol have balanced, so there weren't more 4's than
--         3's and 5's added together. We write 'N' on the tape,
--
--     (2) If we can't, then there were more of its opposite since we were able
--         to find its opposite to drop, but not the current symbol, so it was
--         unbalanced. If this happens, we can stop early. If we were looking
--         to drop '4', it means there were less 4's ("less"), otherwise we were
--         looking to drop '[35]', so there were more 4's ("more").
--
--         Once we've determined whether there were more or less 4's ("more" or
--         "less" respectively), there might still have been marks or leftover
--         symbols of the opposite type to the left, so we must scroll left and
--         clear every cell until we reach a blank. Once we do, we write 'Y' or
--         'N' respectively, and stop ("Done").
-----------------------------------------------------------

more4's :: Prog
more4's = [
            (("start",'3'), (' ',Rght,"dropY")),
            (("start",'4'), (' ',Rght,"dropN")),
            (("start",'5'), (' ',Rght,"dropY")),
            (("start",'x'), (' ',Rght,"start")),
            (("start",' '), ('N',Rght,"Done ")), -- equal

            (("dropY",'3'), ('3',Rght,"dropY")),
            (("dropY",'4'), ('x',Lft, "back ")),
            (("dropY",'5'), ('5',Rght,"dropY")),
            (("dropY",'x'), ('x',Rght,"dropY")),
            (("dropY",' '), (' ',Lft, "less ")),

            (("dropN",'3'), ('x',Lft, "back ")),
            (("dropN",'4'), ('4',Rght,"dropN")),
            (("dropN",'5'), ('x',Lft, "back ")),
            (("dropN",'x'), ('x',Rght,"dropN")),
            (("dropN",' '), (' ',Lft, "more ")),

            (("back ",'3'), ('3',Lft, "back ")),
            (("back ",'4'), ('4',Lft, "back ")),
            (("back ",'5'), ('5',Lft, "back ")),
            (("back ",'x'), ('x',Lft, "back ")),
            (("back ",' '), (' ',Rght,"start")),

            (("more ",'4'), (' ',Lft, "more ")),
            (("more ",'x'), (' ',Lft, "more ")),
            (("more ",' '), ('Y',Rght,"Done ")),

            (("less ",'3'), (' ',Lft, "less ")),
            (("less ",'5'), (' ',Lft, "less ")),
            (("less ",'x'), (' ',Lft, "less ")),
            (("less ",' '), ('N',Rght,"Done "))

            --Done is an empty state
          ]


-----------------------------------------------------------
--
-- xor
--     Calculates the bitwise XOR operation of two bit strings
--
-- Initial input: Two non-empty bit strings of equal length separated by '#'
-- Result: The bit-wise XOR value of the two bit strings
--
-- Initially, the tape head begins at the left of the input string, so first
-- we read the first bit in the x (left) string ("Read") and erase it, since
-- it will be held in its corresponding state ("left0" if 0, or "left1" if 1)
-- and we only want one string to finish with.
--
-- Once we've read the first bit, we scroll to the right ("left0", "left1")
-- until we reach the '#' separator, which lets us know we've reached the y
-- (right) string.
--
-- Once we've entered the y string, we scroll until we reach the first unmarked
-- digit ("xor0" if x's bit was 0, or "xor1" if x's bit was 1) and change y's
-- bit to the logic value of the xor operation between the carried x bit and the
-- current y bit (x!=y). We must use a non-bit symbol so the the Yxor* states
-- won't stop too early and operate on the wrong bit. Once the bit has been
-- changed, we scroll back to the first space, moving the head back to the
-- left-most symbol ("back").
--
-- Eventually, the left-most symbol will be the '#' separator, meaning the
-- operation has been completed on all bits. At this point, we erase the '#'
-- separator. Then all we need to do is convert the T's and F's back to the
-- respective 1's and 0's to get the actual bit string ("Read"). When it reaches
-- a blank cell, all the bits have been converted, so the machine stops.
-----------------------------------------------------------

xor :: Prog
xor = [
        (("Read ",'0'), (' ',Rght,"left0")),
        (("Read ",'1'), (' ',Rght,"left1")),
        (("Read ",'#'), (' ',Rght,"Read ")),
        (("Read ",'T'), ('1',Rght,"Read ")),
        (("Read ",'F'), ('0',Rght,"Read ")),

        (("left0",'0'), ('0',Rght,"left0")),
        (("left0",'1'), ('1',Rght,"left0")),
        (("left0",'#'), ('#',Rght,"xor0 ")),

        (("left1",'0'), ('0',Rght,"left1")),
        (("left1",'1'), ('1',Rght,"left1")),
        (("left1",'#'), ('#',Rght,"xor1 ")),

        (("xor0 ",'T'), ('T',Rght,"xor0 ")),
        (("xor0 ",'F'), ('F',Rght,"xor0 ")),
        (("xor0 ",'0'), ('F',Lft, "back ")),
        (("xor0 ",'1'), ('T',Lft, "back ")),

        (("xor1 ",'T'), ('T',Rght,"xor1 ")),
        (("xor1 ",'F'), ('F',Rght,"xor1 ")),
        (("xor1 ",'0'), ('T',Lft, "back ")),
        (("xor1 ",'1'), ('F',Lft, "back ")),

        (("back ",'T'), ('T',Lft, "back ")),
        (("back ",'F'), ('F',Lft, "back ")),
        (("back ",'0'), ('0',Lft, "back ")),
        (("back ",'1'), ('1',Lft, "back ")),
        (("back ",'#'), ('#',Lft, "back ")),
        (("back ",' '), (' ',Rght,"Read "))
      ]
