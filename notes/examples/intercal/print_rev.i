    DO .4 <- #6
    DO .2 <- #1
    DO ,1 <- #50
    DO ,1 SUB #1 <- #0
    DO ,1 SUB #2 <- #5
    DO ,1 SUB #3 <- #4
    DO ,1 SUB #4 <- #3
    DO ,1 SUB #5 <- #2
    DO ,1 SUB #6 <- #1
    DO ,1 SUB #7 <- #0

5) DO READ OUT ,1 SUB .4
    DO .1 <- .4
    DO (1010) NEXT
    PLEASE DO .4 <- .3
    PLEASE DO (6) NEXT
    DO (5) NEXT

(6) PLEASE DO (7) NEXT
    PLEASE GIVE UP

(7) DO .1 <- .4
    DO (1010) NEXT
    DO .1 <- '.3~.3'~#1
    PLEASE (1020) NEXT
    DO RESUME .1
