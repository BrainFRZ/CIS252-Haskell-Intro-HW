-- counts subsequences of "dggd"
countDGGD :: Prog
countDGGD = [
              (("start", 'd'), ('d',Rght,"scanG1")),
              (("start", 'g'), ('g',Rght,"start")),
              -- If no d's (including if empty), will halt at end space

              (("scanG1",'d'), ('d',Rght,"scanG1")),
              (("scanG1",'g'), ('g',Rght,"scanG2")),

              (("scanG2",'d'), ('d',Rght,"scanG2")),
              (("scanG2",'g'), ('g',Rght,"scanD")),

              (("scanD", 'g'), ('g',Rght,"scanD")),
              (("scanD", 'd'), ('D',Rght,"tally")),

              (("tally", 'd'), ('d',Rght,"tally")),
              (("tally", 'g'), ('g',Rght,"tally")),
              (("tally", '1'), ('1',Rght,"tally")),
              (("tally", ' '), ('1',Lft, "back")),

              (("back",  '1'), ('1',Lft, "back")),
              (("back",  'd'), ('d',Lft, "back")),
              (("back",  'g'), ('g',Lft, "back")),
              (("back",  'D'), ('d',Rght,"scanG1"))
          ]
